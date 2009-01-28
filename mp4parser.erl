-module(mp4parser).
-export([main/2]).

-define(ATOM_PREAMBLE_SIZE, 8).

%-define(CONTAINER_ATOMS, [

-record(mp4_atom, {
    type,
    size_base,
    start}).

-record(moov_atom, {
    mvhd,
    tracks}).

-record(mvhd_atom, {
    version,
    flags,
    creation_time,
    modification_time,
    timescale,
    duration,
    rate,
    volume,
    reserved,
    matrix,
    predefined,
    next_track_id}).

-record(trak_atom, {
    tkhd_atom,
    mdia_atom,
    chunks_size,
    chunks,
    samples_size,
    samples}).

-record(chunk, {
    sample,         % number of the first sample in the chunk
    size,           % number of samples in the chunk
    id,             % for multiple codecs mode - not used
    pos}).          % start byte position of chunk

-record(sample, {
    pts,            % decoding/presentation time
    size,           % size in bytes
    pos,            % byte offset
    cto}).          % composition time offset



read_atoms(File) ->
    read_atoms(File, 0, 0, []).

read_atoms(File, Atom) ->
    read_atoms(File, Atom#mp4_atom.start + ?ATOM_PREAMBLE_SIZE, Atom#mp4_atom.size_base).

read_atoms(File, Pos, End) ->
    read_atoms(File, Pos, End, []).

read_atoms(File, Pos, End, Atoms) when (End > Pos) or (End == 0) ->
    case file:pread(File, Pos, 8) of
        {ok, <<Size:32/integer, Type:4/binary>>} ->
            Atom = #mp4_atom{type=binary_to_list(Type), size_base=Size, start=Pos},
            read_atoms(File, Pos + Size, End, lists:append(Atoms, [Atom]));
        eof ->
            Atoms
    end;

read_atoms(_File, _Pos, _End, Atoms) ->
    Atoms.

find_atoms(Type, Atoms) ->
    lists:filter(fun(X) -> X#mp4_atom.type == Type end, Atoms).

parse_atom(File, Atom, "mvhd") ->
    case file:pread(File, Atom#mp4_atom.start, Atom#mp4_atom.size_base) of
        {ok, <<_:4/binary, "mvhd", Version:8, Flags:3/binary, CreationTime:32, ModificationTime:32, TimeScale:32, Duration:32, PrefRate:32, PrefVolume:16, _Reserved:10/binary, Matrix:36/binary, _:24/binary, NextTrackID:32>>} ->
            #mvhd_atom{
                version = Version,
                flags = Flags,
                creation_time = CreationTime,
                modification_time = ModificationTime,
                timescale = TimeScale,
                duration = Duration,
                rate = PrefRate,
                volume = PrefVolume,
                matrix = Matrix,
                next_track_id = NextTrackID}
    end.

pp_atoms(Atoms) ->
    lists:foreach(fun(X) -> io:format("  Atom [~s], Start ~w, Length ~w~n", [
        X#mp4_atom.type, X#mp4_atom.start, X#mp4_atom.size_base]) end, Atoms).

main(FileName, _StartTime) ->
    {ok, File} = file:open(FileName, [read,binary,raw]),

    RootAtoms = read_atoms(File),
    io:format("Root atoms:~n", []),
    pp_atoms(RootAtoms),
    [MoovAtom|_] = find_atoms("moov", RootAtoms),
    MoovAtoms = read_atoms(File, MoovAtom),
    io:format("Moov atoms:~n", []),
    pp_atoms(MoovAtoms),
    [MvhdAtom|_] = find_atoms("mvhd", MoovAtoms),
    MvhdAtomFull = parse_atom(File, MvhdAtom, "mvhd"),
    %io:format("Mvhd Atom:~n~p~n", [MvhdAtomFull]),
    io:format("Timescale: ~w, Duration: ~w~n", [MvhdAtomFull#mvhd_atom.timescale, MvhdAtomFull#mvhd_atom.duration]),
    io:format("Trak atoms:~n", []),
    lists:foreach(fun(Atom) -> 
        io:format("~p~n", [Atom]),
        pp_atoms(read_atoms(File, Atom)) end, find_atoms("trak", MoovAtoms)),
    file:close(File).
