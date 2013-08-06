-module(polka).

-export([parse/1, evaluate/1]).

parse([]) -> [];
parse(Exp) when is_list(Exp) ->
    lists:reverse(shunting_yard(Exp,[], [])).

evaluate([]) -> nan;
evaluate(Expr) when is_list(Expr) -> 
    evaluate(Expr, []).

evaluate([Number], []) -> Number;
evaluate([Number|Rest], Stack) when is_integer(Number); is_float(Number) -> 
    evaluate(Rest, [Number|Stack]);
evaluate([Op|RestExpr], [Y|[X|RestStack]]) when is_atom(Op) ->
    case Op of
        plus -> evaluate([X+Y|RestExpr], RestStack);
        minus -> evaluate([X-Y|RestExpr], RestStack);
        multiple -> evaluate([X*Y|RestExpr], RestStack);
        divide -> evaluate([X/Y|RestExpr], RestStack)
    end.

shunting_yard("", [], Out) -> 
    Out;
shunting_yard("", [Head|Stack], Out) -> 
    case is_operator(Head) of
        true -> shunting_yard("", Stack, [translate(Head)|Out]);
        false -> throw(missing_parenthesis)
    end;
shunting_yard(In, Stack, Out) -> 
  {Token, Rest} = get_token(In),
  try list_to_integer(Token) of
    Int -> 
      {NewStack, NewOut} = process_token(Int, Stack, Out),
      shunting_yard(Rest, NewStack, NewOut)
  catch 
    _:_ -> 
        try list_to_float(Token) of
            Float -> 
                {NewStack, NewOut} = process_token(Float, Stack, Out),
                shunting_yard(Rest, NewStack, NewOut)
        catch
            _:_ -> 
                {NewStack, NewOut} = process_token(Token, Stack, Out),
                shunting_yard(Rest, NewStack, NewOut)
        end 
  end.

process_token(Number, Stack, Out) when is_integer(Number); is_float(Number) -> 
    {Stack, [Number|Out]};
process_token(Token, Stack, Out) -> 
    case Token of
        "(" -> 
            { [Token|Stack], Out };
        ")" -> 
            process_stack_until_left_par(Stack, Out);
        "+" -> 
            process_stack_until_op(Token, Stack, Out);
        "-" -> 
            process_stack_until_op(Token, Stack, Out);
        "*" -> 
            process_stack_until_op(Token, Stack, Out);
        "/" -> 
            process_stack_until_op(Token, Stack, Out)
    end.

process_stack_until_op(Op1, [], Out) -> 
    { [Op1|[]], Out };
process_stack_until_op(Op1, Stack, Out) -> 
    [Op2|RestStack] = Stack,
    case is_operator(Op2) of
        false -> 
            { [Op1|Stack], Out };
        true -> 
            case get_preced(Op1) < get_preced(Op2) of
                false -> 
                    { [Op1|Stack], Out };
                true -> 
                    % grab Op2 from stack and go deeper
                    process_stack_until_op(Op1, RestStack, [translate(Op2)|Out])
            end
    end.

process_stack_until_left_par([], _Out) -> 
    %stack is empty, but we're still in this function - 
    %that means that there is missing parenthesis in the expression
    throw(missing_parenthesis);
process_stack_until_left_par([Token|Stack], Out) ->
    case Token of 
        "(" -> {Stack, Out}; %drop '(' from stack and stop  recursion
        "+" -> process_stack_until_left_par(Stack, [translate(Token)|Out]);
        "-" -> process_stack_until_left_par(Stack, [translate(Token)|Out]);
        "*" -> process_stack_until_left_par(Stack, [translate(Token)|Out]);
        "/" -> process_stack_until_left_par(Stack, [translate(Token)|Out])
    end.

get_preced(Op) -> 
    case Op of
        "+" -> 2;
        "-" -> 2;
        "*" -> 4;
        "/" -> 4
    end.

translate(Op) -> 
    case Op of
        "+" -> plus;
        "-" -> minus;
        "*" -> multiple;
        "/" -> divide
    end.

is_operator(Token) ->
    lists:member(Token, ["+","-","*","/"]).

get_token([]) -> {};
get_token(String) when is_list(String) ->

  [C|Rest] = String,
  Operators = "+-*/",
  Figures = "1234567890.",
  Parenthesis = "()",

  case lists:member(C, Operators) or lists:member(C, Parenthesis) of
    true -> {[C], Rest};
    false -> 
      case lists:member(C, Figures) of
        true -> 
          Number = get_number(String),
          % skip known amount of characters (i.e. number's length) to the next token
          {_, Rest2} = lists:split(length(Number), String),
          {Number, Rest2};
        false ->
          throw(invalid_token_character)
      end
  end.

get_number([]) -> [];
get_number([C|Rest]) -> 
  Figures = "1234567890.",
  case lists:member(C, Figures) of
    true -> [C|get_number(Rest)];
    false -> []
  end.

