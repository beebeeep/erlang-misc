-module(sort).

qsort([]) -> [];
qsort([Pivot|List]) -> 
  {Smaller, Rest} = splitSmaller(Pivot, List),
  lists:append(qsort(Smaller), [Pivot|qsort(Rest)]).

splitSmaller(Pivot, List) -> 
  {filter(l, Pivot, List), filter(ge, Pivot, List)}.

filter(_, _, []) -> [];
filter(How, Value, [X|Xs]) -> 
  case How of
    l when X < Value -> [X|filter(How, Value, Xs)];
    ge when X >= Value -> [X|filter(How, Value, Xs)];
    _ -> filter(How, Value, Xs)
  end.

rand_list(0, _) -> [];
rand_list(N, M) -> [random:uniform(M)|rand_list(N-1, M)].

rand_list(N) -> rand_list(N, N).
