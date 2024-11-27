%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of allNormalForms of Control.Search.SearchTree.Unsafe:
%
% Warning: in contrast to Curry's definition, this implementation
% suspends until the expression does not contain unbound global variables.
% Moreover, it is strict, i.e., it computes always all solutions even if
% only a few are actually demanded!

:- block 'Unsafe.allNormalForms'(?,?,-,?).
'Unsafe.allNormalForms'(Exp,Vals,E0,E) :-
	waitUntilGround(Exp,E0,E1),
	'Unsafe.allNormalForms_exec'(Exp,Vals,E1,E).

:- block 'Unsafe.allNormalForms_exec'(?,?,-,?).
'Unsafe.allNormalForms_exec'(Exp,Vals,E0,E) :-
	hasPrintedFailure
	 -> findall((X,E1),nf(Exp,X,E0,E1),ValEs),
	    unsuspendedSolutions(ValEs,Vals,E0,E)
	  ; asserta(hasPrintedFailure),
	    findall((X,E1),nf(Exp,X,E0,E1),ValEs),
	    retract(hasPrintedFailure),
	    unsuspendedSolutions(ValEs,Vals,E0,E).

% check whether all solutions of encapsulated search are not suspended:
unsuspendedSolutions([],[],E0,E0).
unsuspendedSolutions([(Sol,E)|SolEs],[Sol|Sols],E0,E1) :-
	unsuspendedMoreSolutions(SolEs,Sols,E,E0,E1).

:- block unsuspendedMoreSolutions(?,?,-,?,?).
unsuspendedMoreSolutions(SolEs,Sols,_,E0,E) :-
	unsuspendedSolutions(SolEs,Sols,E0,E).


'Control.Search.SearchTree.Unsafe.prim_lookupVarId'(Term,H) :-
        var(Term), !,
        getFreeVarId(Term,N), H='Prelude.Just'(N).
'Control.Search.SearchTree.Unsafe.prim_lookupVarId'(_,'Prelude.Nothing').

% Gets an integer id for a variable by writing its Prolog representation
% into a string (e.g., "_123") and transforming it into an integer.

:- use_module(library(codesio)). % for write_to_codes/2

getFreeVarId(V,N) :-
        write_to_codes(V,[95|Cs]),
        number_codes(N,Cs), !.
getFreeVarId(V,N) :-
        writeErr('ERROR in Control.Search.SearchTree.Unsafe.prim_lookupVarId:'),
        nlErr,
        writeErr('Cannot get id of variable: '), writeErr(V), nlErr.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
