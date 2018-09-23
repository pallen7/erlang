-module(my_maps).
-export([map_search_pred/2]).

map_search_pred(Map, Predicate) ->
    List = maps:to_list(Map),
    filter(List, Predicate).

filter([], _) -> undefined;
filter([{K,V} = H|T], F) ->
    case F(K,V) of
        true  -> H;
        false -> filter(T,F)
    end.