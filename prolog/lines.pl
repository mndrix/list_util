%% lines(+Source, -Lines:list(string)) is det.
%
%  Lines is a lazy list of lines from Source. Source can be
%  one of:
%
%    * file(Filename) - read lines from a file
%    * stream(Stream) - read lines from a stream
%
% After the last line has been read, all relevant streams are
% automatically closed. One must be able to call set_stream_position/2
% on Stream.
%
% Each line in Lines does not contain the line terminator.
lines(file(Filename), Lines) :-
    open(Filename, read, Stream, []),
    lines(stream(Stream), Lines).
lines(stream(Stream), Lines) :-
    stream_property(Stream, position(Pos)),
    iterate(lines_, Stream-Pos, Lines).

lines_(Stream-Pos0, Stream-Pos, Line) :-
    is_stream(Stream), % don't touch a closed stream
    set_stream_position(Stream, Pos0), % to handle backtracking
    read_line_to_string(Stream, Tentative),
    ( Tentative = end_of_file ->
        close(Stream),
        fail  % to end the list of lines
    ; % there was a line ->
        stream_property(Stream, position(Pos)),
        Line = Tentative
    ).
