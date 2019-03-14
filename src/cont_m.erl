-module(cont_m).

-export_type([cont/1]).

-export(['>>='/2, return/1, fail/1, cont/1, run_cont/1]).

-behaviour(monad).

-type cont(A) :: {cont, fun((fun ((A) -> any())) -> any())}.


-spec '>>='(cont(A), fun( (A) -> cont(B) )) -> cont(B).
'>>='({cont, X}, Fun) -> erlang:apply(X, [Fun]).

-spec return(A) -> cont(A).
return(X) ->
    {cont, fun (F) -> erlang:apply(F, [X]) end}.


-spec fail(A) -> cont(A).
fail(X) -> error(X).

-spec cont(fun((fun ((A) -> any())) -> any())) -> cont(A).
cont(F) ->
    {cont, F}.

run_cont({cont, C}) ->
    erlang:apply(C, [fun (A) -> A end]).
