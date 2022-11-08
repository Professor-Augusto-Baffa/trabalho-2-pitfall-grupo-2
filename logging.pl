:- module(logging, [
    enable_logging/0,
    disable_logging/0,
    log/1,
    log/2
]).

:- dynamic([
    verbose/0
]).

verbose.

enable_logging :-
    verbose,
    !.
enable_logging :-
    assertz(verbose).

disable_logging :-
    verbose,
    retractall(verbose),
    !.
disable_logging.

log(Format) :-
    verbose,
    format(Format),
    !.
log(_).

log(Format, Arguments) :-
    verbose,
    format(Format, Arguments),
    !.
log(_, _).

