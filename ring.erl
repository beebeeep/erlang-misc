-module(ring).
-export([start/1, child/2, parent_recv_loop/1]).

parent_loop(0, {Child, Message}) -> 
    io:format("Start process ~p, routing messaage ~p through ring, ~p iteration~n", 
        [self(), Message, 0]),
    Child ! {self(), Message},
    receive
        {Pid, Message} -> 
            io:format("Start process ~p, received message ~p from ~p~n",
                [self(), Pid, Message])
    end,
    ok;
parent_loop(M, {Child, Message}) -> 
    io:format("Start process ~p, routing messaage ~p through ring, ~p iteration~n", 
        [self(), Message, M]),
    Child ! {self(), Message},
    receive
        {Pid, Message} -> 
            io:format("Start process ~p, received message ~p from ~p~n",
                [self(), Pid, Message]),
            parent_loop(M-1, {Child, Message})
    end.

parent_recv_loop(Child) when is_pid(Child) -> 
    receive
      {msg, M, Message} -> 
      io:format("Start procces ~p, i'm going to send message ~p to ring of my childs~n", 
          [self(), Message]),
        parent_loop(M, {Child, Message}),
        parent_recv_loop(Child);
      {control, stop} -> Child ! stop
    end;

parent_recv_loop(N) when is_integer(N) -> 
    Child = spawn(?MODULE, child, [N, self()]),
    parent_recv_loop(Child).

start(N) ->
    Parent = spawn(?MODULE, parent_recv_loop, [N]),
    register(ring, Parent),
    {ok, Parent}.




child(0, StartPid) -> 
    io:format("Child ~p, i'm the last, enclosing the ring~n",
        [self()]),
    child_loop(StartPid);

child(Nremain, StartPid) -> 
    io:format("It's child #~p, pid ~p~n", [Nremain, self()]),
    Child = spawn(?MODULE, child, [Nremain - 1, StartPid]),
    io:format("Child ~p, ~p more to go, spawned my own child ~p~n", [self(), Nremain, Child]),
    child_loop(Child).
    
child_loop(Child) -> 
    io:format("Child ~p, entering receive-forward loop~n", [self()]),
    receive 
        {Pid, Message} -> 
            io:format("Child ~p, got message ~p from ~p, forwarding it to child ~p~n", 
                [self(), Message, Pid, Child]),
            Child ! {self(), Message},
            child_loop(Child);
        stop -> 
            io:format("Child ~p, got stop, forwarding it to my child ~p~n", 
                [self(), Child]),
            Child ! stop,
            {ok, self(), stopped}
    end.
