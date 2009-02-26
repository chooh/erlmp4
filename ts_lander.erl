-module(ts_lander).
-compile(export_all).
-compile([native]).

-define(HOST, "192.168.4.61").
-define(PORT, 5001).
-define(PATH, "/stream/ntv.ts").
-define(OUT_FILE, "out.mpg").

start() ->
    {ok, Socket} = gen_tcp:connect(?HOST ,?PORT, [binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, lists:concat(["GET ", ?PATH, " HTTP/1.0\r\n\r\n"])),
    skip_http_header(Socket).

skip_http_header(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            case Bin of
                <<"HTTP/1.0 200 OK\r\n", Rest/binary>> ->
                    receive_data(Socket, Rest, {status, false, []});
                _ ->
                    {error, invalid_http_response}
            end;
        {tcp_closed, Socket} ->
            {error, socket_closed}
    end.

receive_data(Socket, Rest, N) ->
    receive
        {tcp, Socket, Bin} ->
            find_ts(Socket, list_to_binary([Rest, Bin]), N);
        {tcp_closed, Socket} ->
            {error, socket_closed}
    end.

find_ts(Socket, Bin, N) when size(Bin) > 377 ->
    case Bin of
        <<16#47:8/integer,Packet:187/binary,16#47:8/integer,_:187/binary, 16#47:8/integer,_/binary>> ->
            {_,Rest} = split_binary(Bin, 188),
            find_ts(Socket, Rest, parse_ts(Packet, N));
       % if we cannot find TS packet, skip one byte and try again
        _ ->
            {_, Rest} = split_binary(Bin, 1),
            find_ts(Socket, Rest, N)
    end;

find_ts(Socket, Bin, N) ->
    receive_data(Socket, Bin, N).

parse_ts(Packet, Acc) ->
    <<_TsError:1, PayloadStart:1, _TsPriority:1, Pid:13, _ScrambleControl:2, AdaptationControl:2, ContCounter:4, Rest/binary>> = Packet,
    case Pid of
        320 ->
    {status, Sync, T} = Acc,
    %io:format("Pid: ~p ContCounter: ~p Payload Start indicator: ~p~n", [Pid,ContCounter,PayloadStart]),
    case AdaptationControl bsr 1 of
        1 ->
            <<AdaptationLength:8, _/binary>> = Rest,
            <<_:8, _:AdaptationLength/binary, Payload/binary>> = Rest;
        _ -> Payload = Rest
    end,
    if
        not Sync, PayloadStart /= 1 ->
            Acc;
        not Sync, PayloadStart == 1; Sync, PayloadStart /= 1 ->
            {status, true, [Payload|T]};
        Sync, PayloadStart == 1 ->
            write_pes(list_to_binary(lists:reverse(T))),
            {status, true, [Payload]}
    end;
    _ ->
        Acc
    end.

write_pes(Pes) ->
    <<1:24/integer, StreamId:8/integer, PesPacketLength:16/integer, _/binary>> = Pes,
    io:format("Found PES Data ~p StreamId ~p PES length ~p~n~p~n", [size(Pes), StreamId, PesPacketLength,Pes]).

