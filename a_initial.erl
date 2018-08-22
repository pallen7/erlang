-module(a_initial).
-export([factorial/1, foo/0]).

factorial(N) when N =< 1 -> 1;
factorial(N) -> N * factorial(N-1).

foo() -> io:format("Hello world!~n").