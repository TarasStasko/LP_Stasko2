parse_math_expr(Input, AST) :-
    string_codes(Input, Codes),
    expr(AST, Codes, []).

expr(AST, Input, Rest) :- 
    assign_expr(AST, Input, Rest).
expr(AST, Input, Rest) :- 
    add_expr(AST, Input, Rest).

assign_expr(assign(Var, Expr), Input, Rest) :-
    identifier(Var, Input, [0'=|Rest1]),
    expr(Expr, Rest1, Rest).

add_expr(AST, Input, Rest) :-
    term(Left, Input, Rest1),
    add_op(Op, Rest1, Rest2),
    expr(Right, Rest2, Rest),
    AST =.. [Op, Left, Right].
add_expr(AST, Input, Rest) :- 
    term(AST, Input, Rest).

term(AST, Input, Rest) :-
    power(Left, Input, Rest1),
    mul_op(Op, Rest1, Rest2),
    term(Right, Rest2, Rest),
    AST =.. [Op, Left, Right].
term(AST, Input, Rest) :- 
    power(AST, Input, Rest).

power(exp(Base, Exp), Input, Rest) :-
    factor(Base, Input, [0'^|Rest1]),
    power(Exp, Rest1, Rest).
power(AST, Input, Rest) :- 
    factor(AST, Input, Rest).

factor(N, Input, Rest) :- 
    number(N, Input, Rest).
factor(AST, [0'(|Rest1], Rest) :-
    expr(AST, Rest1, [0')|Rest]).
factor(sqrt(Expr), Input, Rest) :-
    string_codes("sqrt(", Prefix),
    append(Prefix, Rest1, Input),
    expr(Expr, Rest1, [0')|Rest]).
factor(Var, Input, Rest) :- 
    identifier(Var, Input, Rest).

number(N, Input, Rest) :- 
    integer(N, Input, Rest).
integer(N, Input, Rest) :-
    digits(Digits, Input, Rest),
    number_codes(N, Digits).

digits([D|T], [D|Rest1], Rest) :-
    code_type(D, digit),
    digits(T, Rest1, Rest).
digits([D], [D|Rest], Rest) :-
    code_type(D, digit).

identifier(Var, Input, Rest) :-
    identifier_chars([C|Cs], Input, Rest),
    atom_codes(Var, [C|Cs]).

identifier_chars([C|Cs], [C|Rest1], Rest) :-
    code_type(C, alpha),
    identifier_chars(Cs, Rest1, Rest).
identifier_chars([C], [C|Rest], Rest) :-
    code_type(C, alnum).

add_op(add, [0'+|Rest], Rest).
add_op(sub, [0'-|Rest], Rest).
mul_op(mul, [0'*|Rest], Rest).
mul_op(dvd, [0'/|Rest], Rest).
mul_op(mod, [0'm,0'o,0'd|Rest], Rest).

eval_expr(assign(Var, Expr), Env, NewEnv, Result) :-
    eval_expr(Expr, Env, TempEnv, Result),
    (select((Var, _), TempEnv, RestEnv) -> 
        NewEnv = [(Var, Result)|RestEnv]
    ; 
        NewEnv = [(Var, Result)|TempEnv]
    ).

eval_expr(exp(Base, Exp), Env, Env, Result) :-
    eval_expr(Base, Env, _, V1),
    eval_expr(Exp, Env, _, V2),
    Result is V1 ** V2.

eval_expr(add(A,B), Env, Env, Result) :-
    eval_expr(A, Env, _, V1),
    eval_expr(B, Env, _, V2),
    Result is V1 + V2.

eval_expr(sub(A,B), Env, Env, Result) :-
    eval_expr(A, Env, _, V1),
    eval_expr(B, Env, _, V2),
    Result is V1 - V2.

eval_expr(mul(A,B), Env, Env, Result) :-
    eval_expr(A, Env, _, V1),
    eval_expr(B, Env, _, V2),
    Result is V1 * V2.

eval_expr(dvd(A,B), Env, Env, Result) :-
    eval_expr(A, Env, _, V1),
    eval_expr(B, Env, _, V2),
    V2 =\= 0,
    Result is V1 / V2.

eval_expr(mod(A,B), Env, Env, Result) :-
    eval_expr(A, Env, _, V1),
    eval_expr(B, Env, _, V2),
    V2 =\= 0,
    Result is floor(V1) mod floor(V2).

eval_expr(sqrt(Expr), Env, Env, Result) :-
    eval_expr(Expr, Env, _, V),
    V >= 0,
    Result is sqrt(V).

eval_expr(N, _, _, N) :- number(N).
eval_expr(Var, Env, Env, Value) :-
    atom(Var),
    memberchk((Var, Value), Env).
