-module(calculator).
-export([start/0, calculate/1, loop/0]).

start() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(calculator, loop, []),
    register(calculator, Pid),
    {ok, Pid}.

calculate(Expression) -> 
    calculator ! {request, self(), Expression},
    receive
        {result, Result} -> Result;
        {'EXIT', _Pid, Reason} -> {error, Reason}
    after 1000 -> timeout
    end.

loop() -> 
    receive 
        {request, Pid, Expression} -> 
            ParsedExpression = polka:parse(Expression),
            Result = polka:evaluate(ParsedExpression),
            Pid ! {result, Result}
    end,
    loop().

