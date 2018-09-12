-module(math_functions).
-export([even/1, odd/1, filter/2, split/1, split2/1]).

even(X) ->
    case X rem 2 of
        0 -> true;
        1 -> false
    end.

odd(X) -> not even(X).

filter(_F, []) -> [];
filter(F, [H|T]) ->
    case F(H) of
        true -> [H | filter(F, T)];
        false -> filter(F, T)
    end.

split(L) ->
    {filter(fun even/1, L), filter(fun odd/1, L)}.

split2(L) ->
    split2(L, {[], []}).

split2([], {Even, Odd}) ->
    {lists:reverse(Even), lists:reverse(Odd)};

split2([H | T], {Even, Odd}) ->
    Acc =
        case even(H) of
            true  -> {[H | Even], Odd};
            false -> {Even, [H | Odd]}
        end,
    split2(T, Acc).
