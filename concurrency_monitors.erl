-module(concurrency_monitors).
-compile(export_all).

start_critic() ->
    spawn(?MODULE, critic, []).

judge(Pid, Band, Album) ->
    Pid ! {self(), {Band, Album}},
    receive
        {Pid, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.

critic() ->
    receive
        {From, {"Rage", "Testify"}} ->
            From ! {self(), "Great"};
        {From, {"Oasis", "Maybe"}} ->
            From ! {self(), "Ok"};
        {From, {"Beyonce", "Lemonade"}} ->
            From ! {self(), "Pants"};
        {From, {_Band, _Album}} ->
            From ! {self(), "Don't know"}
    end,
    critic().
