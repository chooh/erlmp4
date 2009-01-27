-module(mp4parser).
-export([main/2]).

-define(ATOM_PREAMBLE_SIZE, 8).

-record(mp4_atom, {
    type,
    size_base,
    size_ext=0,
    start}).

%-record(mvhd_atom, {
%    version,
%    flags,
%    creation_time,
%    modification_time_,
%    timescale,
%    duration,
%    rate,
%    volume,
%    reserved1,
%    reserved2,
%    matrix,
%    predefined,
%    next_track_id}).

read_atoms(File) ->
    read_atoms(File, 0, 0, []).

read_atoms(File, Pos, End) ->
    read_atoms(File, Pos, End, []).

read_atoms(File, Pos, End, Atoms) when (End > Pos) or (End == 0) ->
    case file:pread(File, Pos, ?ATOM_PREAMBLE_SIZE) of
        {ok, Bin} ->
            {<<Size:32>>, Type} = split_binary(Bin, 4),
            % TODO: Check if SizeExt is needed
            NewAtoms = lists:append(Atoms, [#mp4_atom{type=binary_to_list(Type), size_base=Size, start=Pos}]),
            read_atoms(File, Pos + Size, End, NewAtoms);
        eof ->
            Atoms
    end;

read_atoms(_File, _Pos, _End, Atoms) ->
    Atoms.

find_atom(Type, Atoms) ->
    case lists:filter(fun(X) -> X#mp4_atom.type == Type end, Atoms) of
        [H|_] ->
            H;
        [] ->
            {error, not_found}
     end.

pp_atoms(Atoms) ->
    lists:foreach(fun(X) -> io:format("Atom [~s], Start ~w, Length ~w~n", [
        X#mp4_atom.type, X#mp4_atom.start, X#mp4_atom.size_base]) end, Atoms).

main(FileName, _StartTime) ->
    {ok, File} = file:open(FileName, [read,binary,raw]),

    RootAtoms = read_atoms(File),
    io:format("Root atoms:~n", []),
    pp_atoms(RootAtoms),
    MoovAtom = find_atom("moov", RootAtoms),
    MoovAtoms = read_atoms(File, MoovAtom#mp4_atom.start + ?ATOM_PREAMBLE_SIZE, MoovAtom#mp4_atom.start + MoovAtom#mp4_atom.size_base),
    io:format("Moov atoms:~n", []),
    pp_atoms(MoovAtoms),
    file:close(File).
