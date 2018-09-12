-module(lib_misc).
-export([for/3, perms/1, gt_5/1, my_tuple_to_list/1, my_date_string/0, my_time_func/1]).

for(Max, Max, F) -> [F(Max)];
for(I, Max, F)   -> [F(I) || for(I+1, Max, F)].

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

gt_5(L) when is_list(L) ->
% Example of using a guard expression in a case statement
    case L of
        [] -> [];
        [H|T] when is_integer(H) andalso H > 5 -> [H | gt_5(T)];
        List -> gt_5(tl(List))
    end.

my_tuple_to_list(T) ->
    my_tuple_to_list(T, tuple_size(T), []).

my_tuple_to_list(_T, 0, L) ->
    L;
my_tuple_to_list(T, N, L) ->
    my_tuple_to_list(T, N-1, [element(N, T) | L]).

my_date_string() ->
    io:format("Y:~p M:~p D:~p, h:~p m:~p s:~p~n",
    [element(1, date()), element(2, date()), element(3, date()),
     element(1, time()), element(2, time()), element(3, time())]).
    
my_time_func(F) ->
    % Start in microseconds. Note that now is deprecated but used as per book..
    Start = (element(2, now()) * 1000000) + element(3, now()),
    F(),
    End = (element(2, now()) * 1000000) + element(3, now()),
    TimeInMiliseconds = (End-Start) / 1000,
    io:format("Function took: ~p miliseconds", [TimeInMiliseconds]).