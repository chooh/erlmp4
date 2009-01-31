-module(moov).
-export([find_moov\1]).

-define(ATOM_PREAMBLE_SIZE, 8).

find_moov(FileName) when is_list(FileName) ->
    {ok, File} = file:open(FileName, [read,binary,raw]),
    {ok, Moov} = find_moov(File),
    file:close(File),
    Moov;

find_moov(File) ->
    find_moov(File, 0).

find_moov(File, Pos) ->
    case file:pread(File, Pos, ?ATOM_PREAMBLE_SIZE) of
        {ok, <<Size:32/integer, "moov">>} ->
            io:format("Found MOOV @ ~p, length ~p~n", [Pos, Size]),
            file:pread(File, Pos + ?ATOM_PREAMBLE_SIZE, Size - ?ATOM_PREAMBLE_SIZE);
        {ok, <<Size:32/integer, _:4/binary>>} ->
            find_moov(File, Pos + Size);
        eof ->
            {error, moov_not_found}
    end.

