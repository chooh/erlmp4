-module(mp4parser).
%-export([main/2]).
-compile(export_all).

-define(ATOM_PREAMBLE_SIZE, 8).

%-define(CONTAINER_ATOMS, [

-record(mp4_atom, {
    type,
    size_base,
    start}).

-record(moov_atom, {
    mvhd,
    traks}).

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

-record(tkhd_atom, {
    version,
    flags,
    creation_time,
    modification_time,
    track_id,
    duration,
    layer,
    predefined,
    volume,
    matrix,
    width,
    height}).

-record(mdia_atom, {
    mdhd,
    hdlr,
    minf}).

-record(minf_atom, {
    vmhd,
    stbl}).

-record(hdlr_atom, {
    version,
    flags,
    predefined,
    handler_type,
    component_name}).


-record(trak_atom, {
    tkhd,
    mdia,
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
    read_atoms(File, Atom#mp4_atom.start + ?ATOM_PREAMBLE_SIZE, Atom#mp4_atom.start + Atom#mp4_atom.size_base - ?ATOM_PREAMBLE_SIZE).

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
    pp_atoms(Atoms),
    Atoms.

find_atoms(Type, Atoms) ->
    lists:filter(fun(X) -> X#mp4_atom.type == Type end, Atoms).

find_atom(Type, Atoms) ->
    [Return|_] = find_atoms(Type, Atoms),
    Return.

parse_atom(File, Atom) ->
    parse_atom(File, Atom, Atom#mp4_atom.type).

parse_atom(File, Atom, "moov") ->
    Children = read_atoms(File, Atom),
    MvhdAtom = parse_atom(File, find_atom("mvhd", Children)),
    TrakAtoms = lists:map(fun(A) -> parse_atom(File, A) end, find_atoms("trak", Children)),
    #moov_atom{
        mvhd=MvhdAtom,
        traks=TrakAtoms};

parse_atom(File, Atom, "trak") ->
    Children = read_atoms(File, Atom),
    TkhdAtom = parse_atom(File, find_atom("tkhd", Children)),
    #trak_atom{
        tkhd=TkhdAtom};

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
    end;

parse_atom(File, Atom, "tkhd") ->
    case file:pread(File, Atom#mp4_atom.start, Atom#mp4_atom.size_base) of
        {ok, <<_Size:4/binary, "tkhd", Version:8, Flags:3/binary, CreationTime:32, ModificationTime:32, TrackID:32, _:4/binary, Duration:32, _:8/binary, Layer:16, AlternateGroup:16, Volume:16, _:2/binary, Matrix:36/binary, TrackWidth:32, TrackHeight:32>>} ->
            #tkhd_atom{
                version = Version,
                flags = Flags,
                creation_time = CreationTime,
                modification_time = ModificationTime,
                track_id = TrackID,
                duration = Duration,
                layer = Layer,
                predefined = AlternateGroup,
                volume = Volume,
                matrix = Matrix,
                width = TrackWidth,
                height = TrackHeight}
    end.


pp_atoms(Atoms) ->
    lists:foreach(fun(X) -> io:format("  Atom [~s], Start ~w, Length ~w~n", [
        X#mp4_atom.type, X#mp4_atom.start, X#mp4_atom.size_base]) end, Atoms).

main(FileName, _StartTime) ->
    {ok, File} = file:open(FileName, [read,binary,raw]),

    RootAtoms = read_atoms(File),
    io:format("Root atoms:~n", []),
    pp_atoms(RootAtoms),
    MoovAtom = parse_atom(File, find_atom("moov", RootAtoms)),
    io:format("~p~n", [MoovAtom]),
    file:close(File),
    MoovAtom.
