-module(ts_lander).
-compile(export_all).
%-compile([native]).

-define(HOST, "192.168.4.61").
-define(PORT, 5001).
-define(PATH, "/stream/ntv.ts").
-define(OUT_FILE, "out.mpg").

start() ->
    start(?HOST, ?PORT, ?PATH).

start(Host, Port, Path) ->
    spawn(fun() -> http_connect(Host, Port, Path, spawn_link(fun() -> demuxer(dict:new()) end)) end).

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

start_demuxer() ->
    spawn(fun() -> demuxer(dict:new()) end).

demuxer(State) ->
    receive
        {ts_packet, Packet} ->
            <<16#47:8, _:3, TsPid:13, _/binary>> = Packet,
            %io:format("Found TS Pid ~p~n", [TsPid]),
            case dict:find(TsPid, State) of
                {ok, PesExtractorPid} ->
                    PesExtractorPid ! {ts_packet, Packet};
                error ->
                    error
            end,
            demuxer(State);
        {subscribe, TsPid, PesExtractorPid} ->
            demuxer(dict:store(TsPid, PesExtractorPid, State));
        {unsubscribe, TsPid} ->
            demuxer(dict:erase(TsPid, State))
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

write_pes(Pes) ->
    <<1:24/integer, StreamId:8/integer, PesPacketLength:16/integer, _/binary>> = Pes,
    io:format("Found PES Data ~p StreamId ~p PES length ~p~n", [size(Pes), StreamId, PesPacketLength]).

start_pes_writer(FileName) ->
    {ok, File} = file:open(FileName, [write, raw, binary]),
    spawn(fun() -> pes_writer(File) end).

pes_writer(File) ->
    receive
        {pes_packet, Packet} ->
            <<1:24/integer, StreamId:8/integer, PesPacketLength:16/integer, Data/binary>> = Packet,
            io:format("Write PES Data ~p StreamId ~p PES length ~p~n", [size(Packet), StreamId, PesPacketLength]),
            case file:write(File, Data) of
                ok -> pes_writer(File);
                {error, Reason} -> exit(Reason)
            end
    end.

