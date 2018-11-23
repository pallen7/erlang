% Aim is to be able to compile a mathematical expression represented as a string
% i.e. (2+(3*4)) -> {add, {num, 2}, {mul, {num, 3}, {num, 4}}}
% Need:
%   Parsing
%   Pretty printing
%   Compilation (note we can either compile & execute or evaluate directly)
%       Will create a list of instructions (compile) then execute them (on a stack VM)
%       (2+(3*a))
%       {add, {num, 2}, {mul, {num, 3}, {num, 4}}}
%       PUSH2, PUSH3, FETCHa, MUL2, ADD2
%   Evaluation

-module(my_math).

-export([print/1, evaluate/2, compile/1, execute/2]).

-type expr() :: {num, integer()}
            |   {var, atom()}
            |   {add, expr(), expr()}       %% recursive definition
            |   {mul, expr(), expr()}.

-type environment() :: [{atom(), integer()}]. %% represents variables

% types for VM
-type instr() :: {push, integer()}
            |    {fetch, atom()}
            |    {add2}   
            |    {mul2}.

-type program() :: [instr()].

-type stack() :: [integer()].

% Pretty Print - take an expression and convert to a string
-spec print(expr()) -> string().

print({num, N}) ->
    integer_to_list(N);
print({var, A}) ->
    atom_to_list(A);
print({add, E1, E2}) ->
    "(" ++ print(E1) ++ "+" ++ print(E2) ++ ")";
print({mul, E1, E2}) ->
    "(" ++ print(E1) ++ "*" ++ print(E2) ++ ")".

-spec evaluate(environment(), expr()) -> integer().

evaluate(_Env, {num, N}) ->
    N;
evaluate(Env, {var, A}) ->
    lookup(A, Env);
evaluate(Env, {add, E1, E2}) ->
    evaluate(Env, E1) + evaluate(Env, E2);
evaluate(Env, {mul, E1, E2}) ->
    evaluate(Env, E1) * evaluate(Env, E2).

-spec compile(expr()) -> program().

compile({num, N}) ->
    [{push, N}];
compile({var, A}) ->
    [{fetch, A}];
compile({add, E1, E2}) ->
    compile(E1) ++ compile(E2) ++ [{add2}];
compile({mul, E1, E2}) ->
    compile(E1) ++ compile(E2) ++ [{mul2}].

-spec execute(program(), environment()) -> integer().

execute(Program, Env) ->
    execute(Program, Env, []).

-spec execute(program(), environment(), stack()) -> integer().

execute([], _Env, [N]) ->
    N;
execute([{push, N} | Instructions], Env, Stack) ->
    execute(Instructions, Env, [N | Stack]);
execute([{fetch, A} | Instructions], Env, Stack) ->
    N = lookup(A, Env),
    execute(Instructions, Env, [N | Stack]);
execute([{add2} | Instructions], Env, [X,Y|Stack]) ->
    execute(Instructions, Env, [X+Y | Stack]);
execute([{mul2} | Instructions], Env, [X,Y|Stack]) ->
    execute(Instructions, Env, [X*Y | Stack]).


-spec lookup(atom(), environment()) -> integer().

lookup(A, Env) ->
    proplists:get_value(A, Env).