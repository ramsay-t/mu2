-module(abbindtest).
-export([dv/0,ev/0]).

dv() ->
    A = 5,
    B = 6,
    A / B.

ev() ->
    A = 5,
    B = [[],[]],
    A / length(B).
