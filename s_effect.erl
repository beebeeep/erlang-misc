-module(s_effect).
seq(0) -> 
  [0];
seq (N) when N >= 0 -> 
  [N|seq(N-1)].

seq2(N) when is_integer(N), N >= 0 ->
  seq2_acc(0, N).
seq2(N, M) when is_integer(N), is_integer(M), N < M -> 
  seq2_acc(N, M).
seq2_acc(N, M) when N < M ->
  [N|seq2_acc(N+1, M)];
seq2_acc(N, N) ->
  [N].


print_seq(N) ->
  io:format("~p~n", [seq2(N)]).
