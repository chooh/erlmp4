-module(moov_parser).
-compile(export_all).

-define(ATOM_PREAMBLE_SIZE, 8).
-define(CONTAINER_ATOMS, ["trak", "mdia", "minf", "stbl"]).
-define(MAX_TRACKS, 2).
-define(TIME_TO_SAMPLE_TABLE_ENTRY_SIZE, 8).
-define(SAMPLE_SIZE, 4).

parse(Bin) ->
    parse(Bin, [{currentTrack, 0}, {tracks, array:new([{size, ?MAX_TRACKS}, {default, []}])}]).

parse(<<>>, Acc) ->
    Acc;

parse(<<108:32, "mvhd", Data:100/binary, Rest/binary>>, Acc) ->
    <<_:12/binary, Timescale:32/integer, Duration:32/integer, _/binary>> = Data,
    io:format("Found mvhd!!!, duration is ~p, Timescale is ~p~n", [Duration, Timescale]),
    MvhdData = [{mvhdDuration, Duration}, {mvhdTimescale, Timescale}],
    parse(Rest, lists:keymerge(1, Acc, MvhdData));

parse(<<92:32, "tkhd", Data:84/binary, Rest/binary>>, Acc) ->
     <<_Version:8, _Flags:3/binary, _CreationTime:32, _ModificationTime:32, _TrackID:32, _:4/binary, Duration:32, _:8/binary, _Layer:16, _AlternateGroup:16, _Volume:16, _:2/binary, _Matrix:36/binary, _TrackWidth:32, _TrackHeight:32>> = Data,
     io:format("Tkhd duration: ~p~n", [Duration]),
     Track = get_current_track(Acc),
     parse(Rest, set_current_track(Acc, lists:keymerge(1, Track, [{tkhdDuration, Duration}])));

parse(<<32:32, "mdhd", _Data:24/binary, Rest/binary>>, Acc) ->
    parse(Rest, Acc);

parse(<<_Size:32/integer, "stts", _Version:8/integer, _Flags:24/integer, Entries:32/integer, TableAndRest/binary>>, Acc) ->
    {TTSBinary, Rest} = split_binary(TableAndRest, Entries * ?TIME_TO_SAMPLE_TABLE_ENTRY_SIZE),
    io:format("stts table found: ~p~n", [time_to_sample_parse(TTSBinary, [])]),
    parse(Rest, Acc);

parse(<<_Size:32/integer, "stss", _Version:8/integer, _Flags:24/integer, Entries:32/integer, TableAndRest/binary>>, Acc) ->
    {SSTBinary, Rest} = split_binary(TableAndRest, Entries * ?SAMPLE_SIZE),
    io:format("stss table found: ~p~n", [sync_sample_parse(SSTBinary, [])]),
    parse(Rest, Acc);

parse(<<_:4/binary, "trak", Rest/binary>>, Acc) ->
    {value, {currentTrack, CurTrack}} = lists:keysearch(currentTrack, 1, Acc),
    parse(Rest, lists:keystore(currentTrack, 1, Acc, {currentTrack, CurTrack + 1}));

parse(Bin, Acc) ->
    <<Size:32/integer, TypeB:4/binary, _/binary>> = Bin,
    Type = binary_to_list(TypeB),
    io:format("Found ~p~n", [Type]),
    IsContainer = lists:any(fun(X) -> Type == X end, ?CONTAINER_ATOMS),
    if
        IsContainer ->
            {_, Contents} = split_binary(Bin, ?ATOM_PREAMBLE_SIZE),
            parse(Contents, Acc);
        true ->
            {_, NextAtom} = split_binary(Bin, Size),
            parse(NextAtom, Acc)
    end.

time_to_sample_parse(<<SampleCount:32/integer, SampleDuration:32/integer, Another/binary>>, Table) ->
    time_to_sample_parse(Another, [{time_to_sample, SampleCount, SampleDuration}|Table]);

time_to_sample_parse(<<>>, Table) ->
    lists:reverse(Table).

sync_sample_parse(<<SampleNumber:32/integer, Another/binary>>, Table) ->
    sync_sample_parse(Another, [{sync_sample, SampleNumber}|Table]);

sync_sample_parse(<<>>, Table) ->
    lists:reverse(Table).

get_tracks(Acc) ->
    {value, {currentTrack, CurTrack}} = lists:keysearch(currentTrack, 1,  Acc),
    {value, {tracks, Tracks}} = lists:keysearch(tracks, 1 , Acc),
    {CurTrack, Tracks}.

get_current_track(Acc) ->
    {CurTrack, Tracks} = get_tracks(Acc),
    array:get(CurTrack - 1, Tracks).

set_current_track(Acc, Track) ->
    {CurTrack, TracksOld} = get_tracks(Acc),
    TracksNew = array:set(CurTrack - 1, Track, TracksOld),
    lists:keystore(tracks, 1, Acc, {tracks, TracksNew}).

