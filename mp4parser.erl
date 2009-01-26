-module(mp4parser).
-export([main/2]).

-define(ATOM_PREAMBLE_SIZE, 8).

-record(mp4_atom, {
    type,
    atom_size,
    start}).

-record mvhd_atom, {
    version,
    flags,
    creation_time,
    modification_time_,
    timescale,
    duration,
    rate,
    volume,
    reserved1,
    reserved2,
    matrix,
    predefined,
    next_track_id}).

atom_read_header(File, Pos, Atoms) ->
    case file:pread(File, Pos, 8) of
        {ok, Bin} ->
            {<<Size:32>>, Type} = split_binary(Bin, 4),
%            io:format("Atom [~s], length ~w~n", [Type, Size]),
            NewAtoms = dict:store(binary_to_list(Type), #mp4_atom{type=Type, atom_size=Size, start=Pos}, Atoms),
            atom_read_header(File, Pos + Size, NewAtoms);
        eof ->
            Atoms
    end.


main(FileName, _StartTime) ->
    {ok, File} = file:open(FileName, [read,binary,raw]),
    RootAtoms = atom_read_header(File, 0, dict:new()),
    io:format("Root atoms:~n", []),
    lists:map(fun(Key) -> X = dict:fetch(Key, RootAtoms), io:format("Atom [~s], Start ~w, Length ~w~n", [
        X#mp4_atom.type, X#mp4_atom.start, X#mp4_atom.atom_size]) end, dict:fetch_keys(RootAtoms)),
    MoovAtom = dict:fetch("moov", RootAtoms),
    MoovAtoms = atom_read_header(File, MoovAtom#mp4_atom.start + ?ATOM_PREAMBLE_SIZE, dict:new()),
    io:format("Moov atoms:~n", []),
    lists:map(fun(Key) -> X = dict:fetch(Key, MoovAtoms), io:format("Atom [~s], Start ~w, Length ~w~n", [
        X#mp4_atom.type, X#mp4_atom.start, X#mp4_atom.atom_size]) end, dict:fetch_keys(MoovAtoms)),
    file:close(File).
