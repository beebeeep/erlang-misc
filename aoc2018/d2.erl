-module(d2).

-export([start/1]).
-compile(export_all).

start(File) ->
  case file:open(File, read) of
    {ok, F} -> solve2([], F);
    _ -> fail
  end.

solve(Twos, Threes, File)  ->
  case io:get_line(File, '') of
    eof ->
      {Twos, Threes};
    Line ->
      ID = string:chomp(Line),
      {M, N} = count(ID, []),
      solve(Twos + M, Threes + N, File)
    end.

solve2(IDs, File)  ->
  case io:get_line(File, '') of
    eof ->
      find_common(IDs, []);
    Line ->
      ID = string:chomp(Line),
      solve2([ID|IDs], File)
    end.

count([X|Xs], Chars) ->
  case lists:keyfind(X, 1, Chars) of
    false ->
      count(Xs, [{X,1}|Chars]);
    {X, Count} ->
      count(Xs, lists:keyreplace(X, 1, Chars, {X, Count+1}))
  end;
count([], Chars) -> count23(Chars, {0, 0}).
%count([], Chars) -> Chars.


count23([{_, Count}|Chars], {Twos, Threes}) ->
  case Count of
    2 -> count23(Chars, {1, Threes});
    3 -> count23(Chars, {Twos, 1});
    _ -> count23(Chars, {Twos, Threes})
  end;
count23([], {Twos, Threes}) ->
  {Twos, Threes}.

find_common([ID|IDs], Common) ->
  Match = lists:filter(fun(X) -> diff(X, ID) == 1 end, IDs),
  case length(Match) of
    0 -> find_common(IDs, Common);
    _ -> find_common(IDs -- Match, [[ID | Match] | Common])
  end;
find_common([], Common) -> Common.

diff(X, Y) -> diff(X, Y, 0).
diff([_Same|Xs], [_Same|Ys], C) -> diff(Xs, Ys, C);
diff([_X|Xs], [_Y|Ys], C) -> diff(Xs, Ys, C+1);
diff([], [], C) -> C.



