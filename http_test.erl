-module(http_test).
-compile(export_all).

-define(D(X), io:format("DEBUG ~p:~p ~p~n",[?MODULE, ?LINE, X])).

main() ->
  application:start(inets),
  {ok, RequestId} = http:request(get, {"http://cdn.xxx.xxx/stream/ort-tm.ts", []}, [], [{sync, false}, {stream, self}]),
  loop(RequestId).

loop(RequestId) ->
  receive
    {http, {RequestId, stream_start, Headers}} ->
      ?D(Headers),
      loop(RequestId);
    {http, {RequestId, stream, BinBodyPart}} ->
      ?D("HTTP Stream received: " ++ integer_to_list(size(BinBodyPart))),
      loop(RequestId);
    {http, {RequestId, {error, Reason}}} ->
      ?D("HTTP Error: " ++ Reason);
    HZ -> ?D(HZ)
  end.

