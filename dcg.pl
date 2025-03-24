parse_math_expr(Input, AST) :-
    string_codes(Input, Codes),
    phrase(expr(AST), Codes).

expr(Tree) --> assign_expr(Tree).
expr(Tree) --> add_expr(Tree).

assign_expr(Tree) --> identifier(Var), [0'=], expr(Expr), { Tree =.. ['=', Var, Expr] }.
add_expr(Tree) --> term(Left), add_op(Op), expr(Right), { Tree =.. [Op, Left, Right] }.
add_expr(Tree) --> term(Tree).

term(Tree) --> factor(Left), mul_op(Op), term(Right), { Tree =.. [Op, Left, Right] }.
term(Tree) --> factor(Left), pow_op, term(Right), { Tree = exp(Left, Right) }.
term(Tree) --> factor(Tree).

factor(Tree) --> number(Tree).
factor(Tree) --> "(", expr(Tree), ")".
factor(Tree) --> sqrt_op, "(", expr(Arg), ")", { Tree =.. [sqrt, Arg] }.
factor(Tree) --> identifier(Tree).

number(N) --> integer(N).
integer(N) --> digits(D), { number_codes(N, D) }.
digits([D|T]) --> digit(D), digits(T).
digits([D]) --> digit(D).
digit(D) --> [D], { code_type(D, digit) }.

identifier(Var) --> 
    [C], { code_type(C, alpha) }, 
    identifier_rest(Cs), 
    { atom_codes(Var, [C|Cs]) }.
identifier_rest([C|Cs]) --> 
    [C], { code_type(C, alnum) }, 
    identifier_rest(Cs).
identifier_rest([]) --> [].

add_op(add) --> "+".
add_op(sub) --> "-".
mul_op(mul) --> "*".
mul_op(dvd) --> "/".
mul_op(mod) --> "mod".
pow_op --> "^".
sqrt_op --> "sqrt".

eval_expr(AST, Env, NewEnv, Result) :-
    nonvar(AST),
    AST =.. [Op, Left, Right],
    member(Op, [add, sub, mul, dvd, mod]),
    eval_expr(Left, Env, Env, LeftResult),
    eval_expr(Right, Env, Env, RightResult),
    apply_op(Op, LeftResult, RightResult, Result),
    NewEnv = Env.

eval_expr(exp(Base, Exponent), Env, Env, Result) :-
    eval_expr(Base, Env, Env, BaseResult),
    eval_expr(Exponent, Env, Env, ExponentResult),
    Result is BaseResult ** ExponentResult.

eval_expr(AST, Env, NewEnv, Result) :-
    nonvar(AST),
    AST =.. ['=', Var, Expr],
    atom(Var),
    eval_expr(Expr, Env, TempEnv, ExprResult),
    ( select((Var, _), TempEnv, RestEnv) ->
        NewEnv = [(Var, ExprResult) | RestEnv]
    ;
        NewEnv = [(Var, ExprResult) | TempEnv]
    ),
    Result = ExprResult.

eval_expr(sqrt(Arg), Env, Env, Result) :-
    eval_expr(Arg, Env, Env, ArgResult),
    ArgResult >= 0,
    Result is sqrt(ArgResult).

eval_expr(Number, Env, Env, Number) :-
    number(Number).

eval_expr(Var, Env, Env, Value) :-
    atom(Var),
    memberchk((Var, Value), Env).

apply_op(add, X, Y, Result) :- Result is X + Y.
apply_op(sub, X, Y, Result) :- Result is X - Y.
apply_op(mul, X, Y, Result) :- Result is X * Y.
apply_op(dvd, X, Y, Result) :- Y =\= 0, Result is X / Y.
apply_op(mod, X, Y, Result) :-
    (float(X) -> XInt is round(X) ; XInt = X),
    (float(Y) -> YInt is round(Y) ; YInt = Y),
    YInt =\= 0,
    Result is XInt mod YInt.
