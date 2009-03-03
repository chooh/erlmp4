-module(ts_lander).
-compile(export_all).
%-compile([native]).

-define(HOST, "192.168.4.110").
-define(PORT, 7000).
-define(PATH, "/stream/ntv.ts").

% MP4Box -add video.264#video -add audio.aac#audio out.mp4

start() ->
    Pid = start(?HOST, ?PORT, ?PATH),
    %VideoPesExPid = start_pes_extractor(start_pes_writer("video.264")),
    %AudioPesExPid = start_pes_extractor(start_pes_writer("audio.aac")),
    %Pid ! {demuxer, {subscribe, video, fun(P) -> P == 69 end, VideoPesExPid}},
    %Pid ! {demuxer, {subscribe, audio, fun(P) -> P == 68 end, AudioPesExPid}},
    DtsCounterPid = start_dts_counter(),
    VideoPesExPid = start_pes_extractor(DtsCounterPid),
    AudioPesExPid = start_pes_extractor(DtsCounterPid),
    Pid ! {demuxer, {subscribe, video, fun(P) -> P == 101 end, VideoPesExPid}},
    Pid ! {demuxer, {subscribe, audio, fun(P) -> P == 100 end, AudioPesExPid}},
    Pid.

start(Host, Port, Path) ->
    spawn(fun() -> http_connect(Host, Port, Path, spawn_link(fun() -> demuxer([]) end)) end).

http_connect(Host, Port, Path, DemuxerPid) ->
    {ok, Socket} = gen_tcp:connect(Host ,Port, [binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, lists:concat(["GET ", Path, " HTTP/1.0\r\n\r\n"])),
    receive
        {tcp, Socket, Bin} ->
            case Bin of
                <<"HTTP/1.0 200 OK\r\n", Rest/binary>> ->
                    http_receive_data(Socket, Rest, DemuxerPid);
                _ ->
                    {error, invalid_http_response}
            end;
        {tcp_closed, Socket} ->
            {error, socket_closed}
    end.

http_receive_data(Socket, Acc, DemuxerPid) ->
    receive
        {tcp, Socket, Bin} ->
            synchronizer(Socket, list_to_binary([Acc, Bin]), DemuxerPid);
        {tcp_closed, Socket} ->
            {error, socket_closed};
        {demuxer, Msg } ->
            DemuxerPid ! Msg,
            http_receive_data(Socket, Acc, DemuxerPid);
        stop ->
            gen_tcp:close(Socket),
            exit(normal)
    end.

synchronizer(Socket, Bin, DemuxerPid) when size(Bin) > 377 ->
    case Bin of
        <<16#47:8/integer,_:187/binary,16#47:8/integer,_:187/binary, 16#47:8/integer,_/binary>> ->
            {Packet,Rest} = split_binary(Bin, 188),
            DemuxerPid ! {ts_packet, Packet},
            synchronizer(Socket, Rest, DemuxerPid);
       % if we cannot find TS packet, skip one byte and try again
        _ ->
            {_, Rest} = split_binary(Bin, 1),
            synchronizer(Socket, Rest, DemuxerPid)
    end;

synchronizer(Socket, Bin, DemuxerPid) ->
    http_receive_data(Socket, Bin, DemuxerPid).

demuxer(State) ->
    receive
        {ts_packet, Packet} ->
            <<16#47:8, _:3, TsPid:13, _/binary>> = Packet,
            %io:format("Found TS Pid ~p~n", [TsPid]),
            lists:foreach(fun({_Name, Fun, PesExtractorPid}) ->
                case Fun(TsPid) of
                    false -> ok;
                    _ -> PesExtractorPid ! {ts_packet, Packet}
                end
            end, State),
            demuxer(State);
        {subscribe, Name, Fun, PesExtractorPid} ->
            demuxer(lists:keystore(Name, 1, State, {Name, Fun, PesExtractorPid}));
        {unsubscribe, Name} ->
            case lists:keytake(Name, 1, State) of
                {value, _, NewState} -> demuxer(NewState);
                false -> demuxer(State)
            end
    end.

start_pes_extractor(PesProcessorPid) ->
    spawn(fun() -> pes_extractor({sync, false, [], PesProcessorPid}) end).

pes_extractor(State) ->
    receive
        {ts_packet, Packet} ->
            <<_:9, PayloadStart:1, _:16, AdaptationControl:2, _:4, Rest/binary>> = Packet,
            {sync, Sync, T, PesProcessorPid} = State,
            NewState = if
                not Sync, PayloadStart /= 1 ->
                    State;
                not Sync, PayloadStart == 1; Sync, PayloadStart /= 1 ->
                    {sync, true, [extract_ts_payload(AdaptationControl, Rest)|T], PesProcessorPid};
                Sync, PayloadStart == 1 ->
                    %io:format("Found PES~n", []),
                    PesProcessorPid ! {pes_packet, list_to_binary(lists:reverse(T))},
                    {sync, true, [extract_ts_payload(AdaptationControl, Rest)], PesProcessorPid}
            end,
            pes_extractor(NewState)
    end.

extract_ts_payload(AdaptationControl, Bin) ->
    case AdaptationControl bsr 1 of
        1 ->
            <<AdaptationLength:8, _/binary>> = Bin,
            {_AdaptationField, Payload} = split_binary(Bin, AdaptationLength+1),
            %<<_:8, _:AdaptationLength/binary, Payload/binary>> = Bin,
            Payload;
        _ -> Bin
    end.

start_pes_writer(FileName) ->
    spawn(fun() ->
        {ok, File} = file:open(FileName, [write, raw, binary]),
        pes_writer(File)
    end).

pes_writer(File) ->
    receive
        {pes_packet, Packet} ->
            <<1:24/integer,
            StreamId:8/integer,
            PesPacketLength:16/integer,
            2#10:2,
            _PESScramblingControl:2,
            _PESPriority:1,
            _DataAlignmentIndicator:1,
            _Copyright:1,
            _OriginalOrCopy:1,
            _PTS_DTS_flags:2,
            _ESCRFlag:1,
            _ESRateFlag:1,
            _DSMTrickModeFlag:1,
            _AdditionalCopyInfoFlag:1,
            _PESCRCFlag:1,
            _PESExtensionFlag:1,
            PESHeaderDataLength:8,
            _/binary>> = Packet,
            {_, Data} = split_binary(Packet, PESHeaderDataLength+9),
            io:format("Write PES Data ~p StreamId ~p PES length ~p Header length ~p~n", [size(Packet), StreamId, PesPacketLength, PESHeaderDataLength]),
            case file:write(File, Data) of
                ok ->
                    io:format("OK write to file: ~n", []),
                    pes_writer(File);
                {error, Reason} ->
                    io:format("Error write to file: ~p~n", [Reason]),
                    exit(Reason)
            end
    end.

start_dts_counter() ->
    spawn(fun() -> pes_dts_counter(0) end).

pes_dts_counter(Counter) ->
    receive
        {pes_packet, Packet} ->
            <<1:24/integer,
            _StreamId:8/integer,
            _PesPacketLength:16/integer,
            2#10:2,
            _PESScramblingControl:2,
            _PESPriority:1,
            _DataAlignmentIndicator:1,
            _Copyright:1,
            _OriginalOrCopy:1,
            PTS_DTS_flags:2,
            _ESCRFlag:1,
            _ESRateFlag:1,
            _DSMTrickModeFlag:1,
            _AdditionalCopyInfoFlag:1,
            _PESCRCFlag:1,
            _PESExtensionFlag:1,
            _PESHeaderDataLength:8,
            _/binary>> = Packet,
            DTS = case PTS_DTS_flags of
                2#11 ->
                    <<_:9/binary, 3:4/integer, _:36/integer, 1:4/integer, Dts3:3/integer, 1:1/integer, Dts2:15/integer, 1:1/integer, Dts1:15/integer, 1:1/integer, _/binary>> = Packet,
                    Dts1 + (Dts2 bsl 15) + (Dts3 bsl 30);
                2#10 ->
                    <<_:9/binary, 2:4/integer, Pts3:3/integer, 1:1/integer, Pts2:15/integer, 1:1/integer, Pts1:15/integer, 1:1/integer, _/binary>> = Packet,
                    Pts1 + (Pts2 bsl 15) + (Pts3 bsl 30);
                _ ->
                    %io:format("No DTS found~n"),
                    Counter
            end,
            case DTS > Counter of
                true ->
                    io:format("New DTS ~p, Delta ~p~n", [DTS, DTS-Counter]),
                    pes_dts_counter(DTS);
                false ->
                    io:format("!!! DTS ~p, Delta ~p~n", [DTS, DTS-Counter]),
                    pes_dts_counter(Counter)
            end
    end.

