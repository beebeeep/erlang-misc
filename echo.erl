-module(echo).

go() ->
    Pid = spawn(echo, loop, []),
    register(echo, Pid),
    echo ! {self(), hello},
    receive
        {Pid, Msg} -> 
            io:format("Got ~w~n", [Msg])
    end.

loop() -> 
    receive
        {From, Msg} -> 
            io:format("Pid ~p, got ~p from ~p~n", [self(), Msg, From]),
            From ! {self(), Msg},
            loop();
        stop -> 
            io:format("Pid ~p, got stop ~n", [self()]),
            true
    end.
