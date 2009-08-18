-module(ts_lander).
-compile(export_all).
%-compile([native]).

-define(HOST, "127.0.0.1").
-define(PORT, 7000).

%-define(HOST, "192.168.4.65").
%-define(PORT, 500).
-define(PATH, "/stream/bbc.ts").

% MP4Box -add video.264#video -add audio.aac#audio out.mp4

start() ->
    Pid = start(?HOST, ?PORT, ?PATH),
    %VideoPesExPid = start_pes_extractor(start_pes_writer("video.264")),
    VideoPesExPid = start_pes_extractor(spawn(?MODULE, nal_receiver, [[]])),
    %AudioPesExPid = start_pes_extractor(start_pes_writer("audio.aac")),
    Pid ! {demuxer, {subscribe, 69, VideoPesExPid}},
    %Pid ! {demuxer, {subscribe, 68, AudioPesExPid}},
    %DtsCounterPid = start_dts_counter(),
    %VideoPesExPid = start_pes_extractor(DtsCounterPid),
    %AudioPesExPid = start_pes_extractor(DtsCounterPid),
    %Pid ! {demuxer, {subscribe, video, fun(P) -> P == 101 end, VideoPesExPid}},
    %Pid ! {demuxer, {subscribe, audio, fun(P) -> P == 100 end, AudioPesExPid}},

    %SubtitlesPesExPid = start_pes_extractor(start_pes_writer("subs.bin")),
    %Pid ! {demuxer, {subscribe, 66, SubtitlesPesExPid}},

    Pid.

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
        _ ->
            {_, Rest} = split_binary(Bin, 1),
            synchronizer(Socket, Rest, DemuxerPid)
    end;

synchronizer(Socket, Bin, DemuxerPid) ->
    http_receive_data(Socket, Bin, DemuxerPid).

demuxer(State) ->
    receive
        {ts_packet, Packet} ->
            <<16#47:8, _:1, PayloadStart:1, _:1, TsPid:13, _/binary>> = Packet,
            case PayloadStart of
                %1 -> io:format("Found TS Pid ~p~n", [TsPid]);
                _ -> true
            end,
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
            %io:format("Received PES ~p bytes~n~p~n", [size(Packet),Packet]),
            Data = Packet,
            %<<1:24/integer,
            %StreamId:8/integer,
            %PesPacketLength:16/integer,
            %2#10:2,
            %_PESScramblingControl:2,
            %_PESPriority:1,
            %_DataAlignmentIndicator:1,
            %_Copyright:1,
            %_OriginalOrCopy:1,
            %_PTS_DTS_flags:2,
            %_ESCRFlag:1,
            %_ESRateFlag:1,
            %_DSMTrickModeFlag:1,
            %_AdditionalCopyInfoFlag:1,
            %_PESCRCFlag:1,
            %_PESExtensionFlag:1,
            %PESHeaderDataLength:8,
            %_/binary>> = Packet,
            %{_, Data} = split_binary(Packet, PESHeaderDataLength+9),
            %io:format("Write PES Data ~p StreamId ~p PES length ~p Header length ~p~n", [size(Packet), StreamId, PesPacketLength, PESHeaderDataLength]),
            case file:write(File, Data) of
                ok ->
                    io:format("OK write to file~n~p~n", [Data]),
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

% TEST ONLY----
start(FileName) ->
    {ok, File} = file:open(FileName, [read,binary,raw]),
    NalRcv = spawn(?MODULE, nal_receiver, [[]]),
    feed_bytestream_file(File, NalRcv).

feed_bytestream_file(File, Pid) ->
    case file:read(File, 1024) of
        {ok, Data} ->
            Pid ! {raw, Data},
            feed_bytestream_file(File, Pid);
        eof ->
            file:close(File)
    end.

nal_receiver(Buffer) ->
    receive
        {pes_packet, Packet} ->
            <<1:24, _:24, 2#10:2, _:14, PESHeaderDataLength:8, _/binary>> = Packet,
            {_, Data} = split_binary(Packet, PESHeaderDataLength+9),
            nal_synchronizer(list_to_binary([Buffer, Data]));
        {raw, Data} ->
            nal_synchronizer(list_to_binary([Buffer, Data]))
    end.

nal_synchronizer(Buffer) ->
    Offset1 = nal_unit_start_code_finder(Buffer, 0)+3,
    Offset2 = nal_unit_start_code_finder(Buffer, Offset1+3),
    case Offset2 of
        false -> nal_receiver(Buffer);
        _ ->
            Length = Offset2-Offset1-1,
            <<_:Offset1/binary, NAL:Length/binary, Rest/binary>> = Buffer,
            decode_nal(NAL),
            nal_synchronizer(Rest)
    end.

nal_unit_start_code_finder(Bin, Offset) when Offset < size(Bin) ->
    case Bin of
        <<_:Offset/binary, 16#000001:24, _/binary>> ->
            Offset;
        _ ->
            nal_unit_start_code_finder(Bin, Offset + 1)
    end;

nal_unit_start_code_finder(_, _) -> false.

decode_nal(Bin) ->
    <<0:1, NalRefIdc:2, NalUnitType:5, Rest/binary>> = Bin,
    case NalUnitType of
        7 ->
            <<ProfileId:8, _:3, 0:5, Level:8, AfterLevel/binary>> = Rest,
            {SeqParameterSetId, AfterSPSId} = exp_golomb_read(AfterLevel),
            {Log2MaxFrameNumMinus4, _} = exp_golomb_read(AfterSPSId),
            Profile = case ProfileId of
                66 -> "Baseline";
                77 -> "Main";
                88 -> "Extended";
                100 -> "High";
                110 -> "High 10";
                122 -> "High 4:2:2";
                144 -> "High 4:4:4";
                _ -> "Uknkown "++integer_to_list(ProfileId)
            end,
            io:format("~nSequence parameter set ~p ~p~n", [Profile, Level/10]),
            io:format("seq_parameter_set_id: ~p~n", [SeqParameterSetId]),
            io:format("log2_max_frame_num_minus4: ~p~n", [Log2MaxFrameNumMinus4]);
        8 ->
            io:format("Picture parameter set [~p]~n", [size(Bin)]);
        1 ->
            %io:format("Coded slice of a non-IDR picture :: "),
            slice_header(Rest, NalRefIdc);
        2 ->
            %io:format("Coded slice data partition A     :: "),
            slice_header(Rest, NalRefIdc);
        5 ->
            io:format("~nCoded slice of an IDR picture~n"),
            slice_header(Rest, NalRefIdc);
        9 ->
            <<PrimaryPicTypeId:3, _:5, _/binary>> = Rest,
            PrimaryPicType = case PrimaryPicTypeId of
                0 -> "I";
                1 -> "I, P";
                2 -> "I, P, B";
                3 -> "SI";
                4 -> "SI, SP";
                5 -> "I, SI";
                6 -> "I, SI, P, SP";
                7 -> "I, SI, P, SP, B"
            end,
            io:format("Access unit delimiter, PPT = ~p~n", [PrimaryPicType]);
        _ ->
            io:format("Unknown NAL unit type ~p~n", [NalUnitType])
    end.

slice_header(Bin, NalRefIdc) ->
    {_FirstMbInSlice, Rest} = exp_golomb_read(Bin),
    {SliceTypeId, Rest2 } = exp_golomb_read(Rest),
    {PicParameterSetId, Rest3 } = exp_golomb_read(Rest2),
    <<FrameNum:5, FieldPicFlag:1, BottomFieldFlag:1, _/bitstring>> = Rest3,
    SliceType = case SliceTypeId of
        0 -> "P";
        1 -> "B";
        2 -> "I";
        3 -> "p";
        4 -> "i";
        5 -> "P";
        6 -> "B";
        7 -> "I";
        8 -> "p";
        9 -> "i"
    end,
    io:format("~s~p:~p:~p:~p:~p ", [SliceType, FrameNum, PicParameterSetId, FieldPicFlag, BottomFieldFlag, NalRefIdc]).

exp_golomb_read(Bin) ->
    LeadingZeros = count_zeros(Bin,0),
    <<0:LeadingZeros, 1:1, ReadBits:LeadingZeros, Rest/bitstring>> = Bin,
    CodeNum = (1 bsl LeadingZeros) -1 + ReadBits,
    {CodeNum, Rest}.

count_zeros(Bin, Offset) ->
    case Bin of
        <<_:Offset, 1:1, _/bitstring>> -> Offset;
        <<_:Offset, 0:1, _/bitstring>> -> count_zeros(Bin, Offset+1)
    end.
