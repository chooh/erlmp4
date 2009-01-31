-module(moov_parser).
-compile(export_all).

-define(ATOM_PREAMBLE_SIZE, 8).
-define(CONTAINER_ATOMS, []).

parse(Bin) ->
    parse(Bin, []).

parse(<<>>, Acc) ->
    Acc;

parse(<<108:32, "mvhd", Data:100/binary, Rest/binary>>, Acc) ->
    <<_:16/binary, Duration:32/integer, _/binary>> = Data,
    io:format("Found mvhd!!!, duration is ~p~n", [Duration]),
    parse(Rest, Acc);

parse(Bin, Acc) ->
    <<Size:32/integer, Type:4/binary, _/binary>> = Bin,
    io:format("Found ~p~n", [Type]),
    if
        
    {_, <<Rest/binary>>} = split_binary(Bin, Size),
    parse(Rest, Acc).

