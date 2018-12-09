-module(d1).

-export([start/1]).

start(File) ->
  case file:open(File, read) of
    {ok, F} -> count(0, [0], F);
    _ -> fail
  end.

count(Sum, Freqs, File) ->
  case io:get_line(File, '') of
    eof ->
      file:position(File, 0),
      count(Sum, Freqs, File);
    Token ->
      Freq = Sum + list_to_integer(string:chomp(Token)),
      case lists:member(Freq, Freqs) of
        true -> Freq;
        false -> count(Freq, [Freq | Freqs], File)
        end
  end.



