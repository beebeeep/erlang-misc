-module(test).
-export([func/1]).
-export([count/2]).
-export([count2/2]).
-export([sum/2]).
-export([dimensions/1]).

func(X) -> 
  case X of
    1   ->  ok;
    2   ->  nok
  end.

count(even, [X|Xs]) ->
  case X rem 2 of
    0   ->  1 + count(even, Xs);
    1   ->  count(even, Xs)
  end;
count(odd, [X|Xs]) ->
  case X rem 2 of
    1   ->  1 + count(odd, Xs);
    0   ->  count(odd, Xs)
  end;
count(_, []) -> 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

count2(What, [X|Xs]) ->
  case {What, X rem 2} of 
    {odd, 1}  -> 1 + count2(What, Xs);
    {odd, 0}  -> 0 + count2(What, Xs);
    {even, 0} -> 1 + count2(What, Xs);
    {even, 1} -> 0 + count2(What, Xs)
  end;
count2(_, [])   -> 0.

sum(What, [X|Xs]) -> 
  S = case {What, X rem 2} of
    {odd, 1}    -> X;
    {odd, 0}    -> 0;
    {even, 0}   -> X;
    {even, 1}   -> 0
  end,
  S + sum(What, Xs);
sum(_, [])  ->  0.

dimensions([Row|Rest]) when is_list(Row) -> 
  {length(Rest) + 1, length(Row)}.

test_rec() -> 
    receive 
        flush -> 
            io:format("Pid ~p, flushing mailbox~n", [self()]),
            dump_mbox();
        {msg, Message} -> io:format("Pid ~p, recieved message '~p'~n", [self(), Message])
    end,
    test_rec().

dump_mbox() -> dump_mbox(0).
dump_mbox(Count) -> 
    receive
        stop -> true;
        Message -> 
            io:format("Pid ~p, message #~p: ~p~n", [self(), Count, Message])
            dump_mbox(Count + 1).
    end,
