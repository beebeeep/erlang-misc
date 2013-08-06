-module(add_one).
-export([start/0, request/1, loop/0]).

start() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(add_one, loop, []),
    register(add_one, Pid),
    {ok, Pid}.

request(Int) ->
    add_one ! {request, self(), Int}, 
    receive
        {result, Result} -> Result;
        {'EXIT', _Pid, Reason} -> {error, Reason}
        after 1000 -> timeout 
    end.

loop() -> 
    receive
        {request, Pid, Msg} ->
            Pid ! {result, Msg + 1} 
    end,
    loop().
