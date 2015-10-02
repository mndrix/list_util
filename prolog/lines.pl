%% lines(+Source, -Lines:list(string)) is det.
%
%  Lines is a lazy list of lines from Source. Source can be
%  one of:
%
%    * file(Filename) - read lines from a file
%    * stream(Stream) - read lines from a stream
%
% After the last line has been read, all relevant streams are
% automatically closed.
%
% Each line in Lines does not contain the line terminator.
lines(file(File), Lines) :-
    open(File, read, Stream),
    lines(stream(Stream), Lines).
lines(stream(Stream), Lines) :-
    flow_to_llist(line_flow(Stream),Lines).


at_eof(line_flow(Stream)) :-
    at_end_of_stream(Stream).


next(line_flow(Stream),Line) :-
    read_line_to_string(Stream,Line).


finalize_value(line_flow(_),X,X).
