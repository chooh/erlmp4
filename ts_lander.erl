-module(ts_lander).
-compile(export_all).
-compile([native]).

-define(HOST, "192.168.4.61").
-define(PORT, 5001).
-define(PATH, "/stream/ntv.ts").

start() ->
    {ok, Socket} = gen_tcp:connect(?HOST ,?PORT, [binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, lists:concat(["GET ", ?PATH, " HTTP/1.0\r\n\r\n"])),
    skip_http_header(Socket).

skip_http_header(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            case Bin of
                <<"HTTP/1.0 200 OK\r\nContent-Type: video/mpeg2\r\n\r\n", Rest/binary>> ->
                    receive_data(Socket, Rest, 32);
                _ ->
                    {error, invalid_http_response}
            end;
        {tcp_closed, Socket} ->
            {error, socket_closed}
    end.

receive_data(Socket, Rest, N) ->
    receive
        {tcp, Socket, Bin} ->
            parse_ts(Socket, list_to_binary([Rest, Bin]), N);
        {tcp_closed, Socket} ->
            {error, socket_closed}
    end.

parse_ts(Socket, _, 0) ->
    gen_tcp:close(Socket);

parse_ts(Socket, Bin, N) when size(Bin) > 377 ->
    case Bin of
        %<<16#47:8/integer,Packet:187/binary,16#47:8/integer,_:187/binary,16#47:8/integer>> ->
        <<16#47:8/integer,Packet:187/binary,16#47:8/integer,_/binary>> ->
            io:format("Sync found"),
            <<_TsError:1, _PayloadStart:1, _TsPriority:1, Pid:13, _ScrambleControl:2, _AdaptationControl:2, ContCounter:4, _/binary>> = Packet,
            io:format("Pid: ~p ContCounter: ~p~n", [Pid,ContCounter]),
            {_,Rest} = split_binary(Bin, 188),
            parse_ts(Socket, Rest, N-1);
        _ ->
            {_, Rest} = split_binary(Bin, 1),
            parse_ts(Socket, Rest, N)
    end;

parse_ts(Socket, Bin, N) ->
    receive_data(Socket, Bin, N).

