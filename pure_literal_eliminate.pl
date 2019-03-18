%Pure Literals are literals with only one polarity for all the clauses they appear in. Below is code that describes how to eliminate Pure literals from a CNF formula
%By: Ella Hayashi(V00184392), Charles Yang(V00651139), Lyon Chow (V00896568)

pure_literal_eliminate(Clauses, Vars, El_Clauses, El_Vars) :-
    %change the list of lists into a single list
    flatten(Clauses, FlattenedClauses),
    %remove all duplicates from list
    list_to_set(FlattenedClauses, AllLiterals),
    %remove all pure Literals from the literals list
    purify(AllLiterals,PureLiterals),
    %eliminate the clauses with the pure literals in it
    eliminate_clauses(PureLiterals,Clauses,El_Clauses),
    %set the variables of the pure literals to just their polarity
    set_vars(PureLiterals,Vars,El_Vars).

%BaseCase, where the literal lists are empty
purify([],[]).
%If the literal in the head of the list is a Pure literal, remove it from the head and find it in the tail and remove it
purify([_-Var|Literals], PLiterals) :-
    contains(_-Var, Literals), !,
    deleteall(_-Var, Literals, L),
    purify(L, PLiterals).
%if the literal in the head of the list is not a pure literal, look threw the rest of the tail of the list for a pure literal    
purify([Pol-Var|Literals],[Pol-Var|NewLiterals]) :-
    purify(Literals, NewLiterals).

%BaseCase, all of the lists are empty, therefor done
eliminate_clauses([],[],[]).
%the variables list may contain variables, but the clause lists are empty therefor done
eliminate_clauses(_,[],[]).
%the variables list is empty therefor done
eliminate_clauses([], Elit, Elit).
%the case where the literal list is empty, remove from clause but dont recurse
eliminate_clauses([P|PLit], Clauses, El_Clauses) :-
    %check to see if the rest of the list is empty
  	isEmpty(Plit), !,
  	%delete that literal
    deleteplit(P, Clauses, RClauses),
    addlist(RClauses, El_Clauses), !.
%the case where the literal list isn't empty, remove from clause and recurse
eliminate_clauses([P|PLit], Clauses, El_Clauses):-
    deleteplit(P,Clauses,RClauses),
    eliminate_clauses(PLit, RClauses, El_Clauses).

%
set_vars([],Vars, Vars).
set_vars([Pol-Var|Plit], Vars, Vlist) :-
    isEmpty(Plit),!,
    replace(Var, Pol, Vars, Temp),
    addlist(Temp, Vlist).
set_vars([Pol-Var|Plit], Vars, Vlist):-
    replace(Var, Pol, Vars, Temp),
    set_vars(Plit, Temp, Vlist).

%check to see if a list is empty
isEmpty([]) :- true. 
isEmpty([_|_]) :- false.

%Adding an element to a list
addlist([], []).
addlist([I|L1], [I|L2]):-
    addlist(L1, L2).

deleteall(_-Var, [], []).
deleteall(_-Var, [_-X1|T], L):- 
		Var == X1, !,
    deleteall(_-Var, T, L).
deleteall(_-Var, [X1|T], [X1|L]):- 
  Var \== X1,
  deleteall(_-Var, T, L).

%deleting a literal from a list
deleteplit(_, [], []).
deleteplit(_-Var, [C|Clauses], RClauses) :-
    contains(_-Var, C), !,
    deleteplit(_-Var, Clauses, RClauses).
deleteplit(Lit, [C|Clauses], [C|RClauses]):-
    deleteplit(Lit, Clauses, RClauses).

%check to see wheather or not a variable is contained in a list
contains(_-Var, []) :- Var == X.
contains(_-Var,[_-X|Literals]) :- Var==X, !.
contains(_-Var, [X|Literals]) :- Var \== X, contains(_-Var, Literals).

%replacing a literal with it's polarity (true or false) rather then its variable (e.g., X is replaced with true)
replace(_, _, [], []).
replace(Var, Pol, [H|T], [H|T2]) :- Var \== H ,!, replace(Var, Pol, T, T2).
replace(Var, Pol, [Var|T], [Pol|T2]) :- replace(Var, Pol, T, T2).