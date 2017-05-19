/******************************************************************************************************

   File: types.pro

   Description: This is a type checker for Standard ML written in Prolog. Unification is used 
   extensively within the type checker. The type checker prints that the program passed the type 
   checker if it did and prints that type checking failed if it did not pass. 

   The cut operator is used liberally in this type checker since type checking is deterministic
   and searching for a correct means of type checking a program is not only not necessary, it can lead
   to bad error messages from the type checker. 

   The type inference rules for this entire type checker are in chapter 8 of the text 

   Foundations of Programming Languages
   by Kent D. Lee
   published by Springer
   (c) 2015

   PLEASE NOTE:
   While every Prolog predicate returns True or False, the documentation below will refer to 
   predicates returning values should they unify an unbound Prolog variable to a value. In this way
   we can think of Prolog predicates as returning one or more values even though they actually return
   only True or False.

*******************************************************************************************************/

/* In the latest version of Prolog the print is replaced by write. The print predicate still exists, but
   adds unwanted quotes into the generated output. Redefining print to call write instead fixes this issue.
*/

print(P) :- write(P).

/******************************************************************************************************

   readAST
   =======
   This reads the AST from the file a.term which was written
   by the mlcomp.sml code when a program is compiled. 

   Input: 
     Reads from the file a.term so no input to the predicate.

   Output:
     Returns the AST that was read from the file. 

*******************************************************************************************************/

readAST(AST) :- open('a.term',read,Stream), read(Stream,AST).

/******************************************************************************************************

   printMatch
   ==========
   Used for printing a match when debugging.

   Input: 
     1st - A string of characters used for indentations (i.e. spaces)
     2nd - A match which consists of a pattern and an expression.

   Output: 
     Side-effect of printing a match with an arrow (i.e. =>) between pattern and expression. 


   printMatchList
   ==============
   Takes care of printing a list of matches. There is always at least one match in a list, so printing 
   the match list involves printing each match. 

   Input: 
     1st - A string of spaces to be used for indentation.
     2nd - A list of matches.

   Output:
     Side-effect of printing the list of matches.

*******************************************************************************************************/

printMatch(Indent,match(Pat,Exp)) :- print(Indent), printPat(Pat), print(' => '), printExp('',Exp).

printMatchList(Indent,[M]) :- printMatch(Indent,M).

printMatchList(Indent,[M|T]) :- printMatch(Indent,M), printMatchList(Indent,T).

/******************************************************************************************************

  printExpList
  ============
  Prints a list of expressions with commas between. Used in printing lists and tuples. 

  Input:
    1st - Indentation string.
    2nd - A list of expressions to be printed.

  Output:
    Side-effect of printing an expression list.

*******************************************************************************************************/

printExpList(_,[]).

printExpList(Indent,[H]) :- printExp(Indent,H).

printExpList(Indent,[H|T]) :- printExp(Indent,H), print(','), printExpList(Indent,T).

/******************************************************************************************************

  printExp
  ========
  Prints an expression.

  Input:
    1st - Indentation string.
    2nd - An expression to be printed.

  Output:
    Side-effect of printing an expression.

*******************************************************************************************************/

printExp(_, int(I)) :- print(I), !.

printExp(_, ch(S)) :-  print(S),  !.

printExp(_, str(S)) :-  print(S), !.

printExp(_, bool(B)) :- print(B), !.

printExp(_, id(Name)) :- print(Name), !. 

printExp(Indent, listcon(L)) :- print('['), printExpList(Indent,L), print(']').

printExp(Indent, tuplecon(L)) :- print('('), printExpList(Indent,L), print(')').

printExp(Indent, apply(E1,apply(E2,E3))) :- 
             printExp(Indent, E1), print(' ('), printExp(Indent, apply(E2,E3)), print(')').

printExp(Indent, apply(E1,E2)) :- printExp(Indent, E1), print(' '), printExp(Indent, E2).

printExp(Indent, infixexp(Op,E1,E2)) :- printExp(Indent, E1), print(Op), printExp(Indent,E2).

printExp(Indent, expsequence(L)) :- 
        print('('), nl, concat(Indent, '  ', NewIndent), printExpSequence(NewIndent,L),
        print(Indent), print(') ').

printExp(Indent, letdec(D,L)) :- 
        print(Indent), print('let '), printDec(D), nl, print(Indent), print('in'), nl,
        concat(Indent,'  ',SeqIndent), printExpSequence(SeqIndent,L), nl, 
        print(Indent), print('end').

printExp(Indent, handlexp(E,ML)) :- 
        printExp(Indent, E), nl, print(Indent), print('handle'), nl, printMatchList(ML).

printExp(Indent, andalsoop(E1,E2)) :- printExp(Indent,E1), print(' andalso '), printExp(Indent,E2).

printExp(Indent, orelseop(E1,E2)) :- printExp(Indent,E1), print(' orelse '), printExp(Indent, E2).

printExp(Indent, ifthen(E1,E2,E3)) :- 
        print('if '), printExp(Indent,E1), print(' then '), printExp(Indent, E2), 
        print(' else '), printExp(Indent,E3).

printExp(Indent, whiledo(E1,E2)) :- 
        print('while '), printExp(Indent,E1), print(' do '), printExp(Indent, E2).

printExp(Indent, caseof(E,ML)) :- 
        print('case '), printExp(Indent,E), print(' of '), printMatchList(Indent,ML).

printExp(Indent, func(_,ML)) :- print('fn '), printMatchList(Indent,ML).

printExp(_,E) :- 
        nl, nl, print('Typechecker Error: Unknown expression '), print(E),
        nl, nl, throw(error('printExp: unknown expression')).


/******************************************************************************************************

  getNextVar
  ==========
  The getNextVar(Env,X) predicate is called to return a new type variable, X, that is not already in 
  the Env environment. It calls the findFirstFree predicate to get the first free variable. In this 
  predicate, the environment is a list of type variables that are already allocated to other types
  within the type expression.

  Input:
    Environment which is a list of type variable identifiers. 


  Output:
    A free type variable identifier starting with a, then b, c, d, e, ...


  findFirstFree
  =============
  The findFirstFree does all the work for the getNextVar by looking for the first identifier that is
  not in the list of identifiers in the envrionment. 

  Input:
    1st - A list of candidate type variables. 
    2nd - The list of already allocated type variables. 

  Output:
    The next free type variable or typeerror if one is not availble. However, an error condition is 
    handled by throwing an expression anyway. 

*******************************************************************************************************/

findFirstFree([],_,typeerror) :- 
        print('Out of Type Variables!!!'), throw(error('Out of Type Variables!!!')).

findFirstFree([X|_],Env,X) :- not(member(X,Env)).

findFirstFree([X|Tail],Env,A) :- member(X,Env), findFirstFree(Tail,Env,A).

getNextVar(Env,X) :- findFirstFree([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],Env,X).

/******************************************************************************************************

  printType
  =========
  Following are a series of predicates for printing types. Some of the helper functions appear first.
  It all gets started with the printType predicate below. The printType predicate either directly or
  indirectly calls these helper functions. Printing a type may result in a series of unbound Prolog
  variables being bound. When an unbound Prolog variable is bound to a type variable it is added to 
  the environment so when the rest of the type is printed we know which type variables have already
  been used. 

  Input: 
    T, a type to be printed.

  Output:
    The list of type variables that were bound while printing the expression. 

  printTypeH
  ==========
  This is where most of the work of printing types is done. When a type is printed the unbound
  Prolog variables will be bound to type variables and added to the type variable environment. 
  Otherwise, the printing of types is pretty straightforward. 

  Input:
    1st - The type variable environment which is a list of already used type variables.
    2nd - The type to be printed. 

  Output:
    The new type variable environment which consists of the input environment plus any newly
    allocated type variables. 

  printTupleTypeH
  ===============
  This prints the *, which stands for cross-product of types, between types of a tuple's components.

  Input:
    1st - A list of already allocated type variables.
    2nd - A list of types from a tuple. The list cannot be empty since an empty tuple is printed
    as "unit".

  Output:
    A new type variable environment which consists of the input environment plus any newly 
    allocated type variables. 

  printSubTypeH
  =============
  The printSubTypeH is needed because function and tuple types need parens around them in some 
  circumstances to make the type print correctly.

  Input:
    1st - An environment of previously allocated type variables. 
    2nd - A tuple or function type to be printed. 


  Output:
    A new type variable environment which consists of the input environment plus any newly 
    allocated type variables. 

*******************************************************************************************************/

printSubTypeH(Env,A,NewEnv) :- var(A), printTypeH(Env,A,NewEnv).

printSubTypeH(Env,fn(A,B),NewEnv) :- print('('), printTypeH(Env,fn(A,B),NewEnv), print(')').

printSubTypeH(Env,tuple([H|T]),NewEnv) :- print('('), printTypeH(Env,tuple([H|T]),NewEnv), print(')').

printSubTypeH(Env,A,NewEnv) :- printTypeH(Env,A,NewEnv).

printTupleTypeH(Env,[A],NewEnv) :- printSubTypeH(Env,A,NewEnv).

printTupleTypeH(Env,[H|T],NewEnv) :- printSubTypeH(Env,H,Env1), print(' * '), printTupleTypeH(Env1,T,NewEnv).

/******************************************************************************************************/
/* When printing a type, the first predicate definition below matches
   a type when the type is a Prolog variable. After that unification 
   occurs the A in that predicate definition will also be uninstantiated.
   The var predicate call checks to see that A is an uninstantiated 
   Prolog variable. When this happens the Prolog variable A is unified 
   to a type variable with the getNextVar call and the type is printed 
   again with the new environment.

   Notice that the environment from a previous call to printTypeH is
   passed on to the next call to printTypeH in case Prolog variables
   were unified to a value. In this way all the type variables of a
   type are returned by this call. The type variables are returned
   by printType below. */
/******************************************************************************************************/

printTypeH(Env,typevar(A),NewEnv) :- var(A), getNextVar(Env,A), printTypeH([A|Env],typevar(A),NewEnv), !.

printTypeH(Env,typevar(A),[A|Env]) :- print(''''), print(A), !.

printTypeH(Env,tuple([]),Env) :- print(unit), !.

printTypeH(Env,tuple(L),NewEnv) :- printTupleTypeH(Env,L,NewEnv), !. 

printTypeH(Env,listOf(A),NewEnv) :- printSubTypeH(Env,A,NewEnv), print(' list'), !.

printTypeH(Env,ref(A),NewEnv) :- printSubTypeH(Env,A,NewEnv), print(' ref'), !.

printTypeH(Env,fn(A,B),Env2) :- var(A), printSubTypeH(Env,A,Env1), print(' -> '), printTypeH(Env1,B,Env2), !.

printTypeH(Env,fn(tuple(L),B),NewEnv) :- 
        printTypeH(Env,tuple(L),Env1), print(' -> '), printTypeH(Env1,B,NewEnv), !.

printTypeH(Env,fn(A,B),Env2) :- printSubTypeH(Env,A,Env1), print(' -> '), printTypeH(Env1,B,Env2), !.

printTypeH(Env,typeerror,Env) :- print(typeerror), !.

printTypeH(Env,T,Env) :- simple(T), print(T), !. 
        /* The simple(T) handles int, bool, str, exn, and a possible variable */

printTypeH(Env,T,Env) :- nl, nl, print('Error: Attempt to print unknown type '), 
                         print(T), nl, nl, throw(typeerror('unknown type')), !.

printType(T,TypeVars) :- printTypeH([],T,TypeVars).

/******************************************************************************************************

  printPat
  ========
  Following are a series of predicates for printing patterns in expressions. This is used when printing
  an expression. Some expressions include patterns like a function definition or pattern matching on an
  expression result. A pattern never includes any unbound type variables so we don't have to worry about 
  any type environment when printing a pattern. Notice that some patterns are handled as special cases. 
  For instance, the "nil" pattern is handled as a special case of the id pattern. 

  Input: 
    A pattern to be printed. 

  Output:
    Side-effect of printing the pattern. 

  printPats
  =========
  When there is a list of patterns to print then printPat is called on each element of the list. This 
  occurs in tuple and list patterns. 

  Input:
    A list of patterns 

  Output:
    Side-effect of printing the list of patterns with comma separation.   

*******************************************************************************************************/

printPats([]) :- !.

printPats([Elm]) :- printPat(Elm), !.

printPats([H|T]) :- printPat(H), print(','), printPats(T), !.

printPat(idpat(nil)) :- print(nil), !.

printPat(idpat(Name)) :- print(Name), !.

printPat(infixpat(::,Pat1,Pat2)) :- printPat(Pat1), print('::'), printPat(Pat2), !. 

printPat(tuplepat(L)) :- print('('), printPats(L), print(')'), !.

printPat(listpat(L)) :- print('['), printPats(L), print(']'), !.

printPat(intpat(I)) :- print(I), !.

printPat(boolpat(B)) :- print(B), !.

printPat(strpat(S)) :- print(S), !.

printPat(A) :- 
        nl, nl, print('Typechecker Error: Unknown pattern '), print(A),
        nl, nl, throw(error('printPat: unknown pattern')).

/******************************************************************************************************

  closeFunTypes
  =============
  Closing a type is necessary when there are unbound variables in a type expression. This occurs when 
  a function is polymorphic. Its type contains unbound type variables when its type is determined. 
  For instance, a function

  fun f x = x

  is an 'a -> 'a function. The type variable 'a in this case is unbound after "f" is typechecked. 
  Now consider the following program.

  let fun f x = x
  in 
    f 0;
    f "b"
  end

  When the type of "f" is determined it is an 'a -> 'a function. If that type were not closed, then 
  type checking f 0 would result in instantiating the 'a to int. Now, when the second function 
  application is checked, type checking would fail because "b" is not an int. 

  So, once the type of "f" is determined, the type is closed so that it is an 'a -> 'a function
  with no uninstantiated type variables. The type variables are SML type inference variables (i.e. 
  typevar type variables). 

  Printing a type has the side-effect of instantiating all uninstantiated type variables, so 
  this prediate simply prints the type to close it. 

  Input: 
    A list of functions with (name,type) pairs to be closed.  

  Output:
    Side-effect of printing and closing the type.   

  inst
  ====
  The inverse of the close operation is the instantiation operator which creates an instance of a 
  polymorphic function type to be used in function application. All SML type variables are 
  instantiated by replacing them with Prolog variables. 

  This is needed so the function signature (i.e. its type) can unify with its argument to produce
  the type of the result. For instance, from the example above, the result type of f 0 is int and the
  the result type of f "b" is string. 

  To instantiate a polymorphic function's type an environment is built that maps each SML type variable 
  name to the SML Prolog variable that replaces it in its instantiation.


  Input:
    A type, X, that needs to be instantiated with Prolog variables replacing SML type variables.

  Output:
    The resulting instantiated type.

  instanceOf and instanceOfList
  ==========     ==============

  The instanceOf predicate is recursive over type definitions because the type must be recursively 
  traversed to find all type variables within it. The instanceOfList predicate is used to recursively 
  traverse tuple types. 

  Input:
    1st - A list of all SML type variables and their matching Prolog variable.
    2nd - The type or list of types (for instanceOfList) to be instantiated.

  Output:
    1st - The instantiated type or list of types (for instanceOfList)
    2nd - The resulting mapping (i.e. list) of (SML type variables, Prolog variable) pairs. 

*******************************************************************************************************/

closeFunTypes([]) :- !.

closeFunTypes([(Name,Type)|Tail]) :-
        closeFunTypes(Tail), print('val '), print(Name), print(' = fn : '), printType(Type,_), nl, !.

closeExp(Pat,Type) :-  print('val '), printPat(Pat), print(' : '), printType(Type,_), nl, !.

exists(Env,Name) :- member((Name,_),Env), !.

instanceOfList(Env,[],[],Env).

instanceOfList(Env,[H|T],[G|S],NewEnv) :- instanceOf(Env,H,G,Env1), instanceOfList(Env1,T,S,NewEnv).

instanceOf(Env,A,A,Env) :- var(A), !. /* if we find a Prolog variable, leave it alone */

instanceOf(Env,A,A,Env) :- simple(A), !. /* This handles int, bool, exn, str, and any other simple type */

instanceOf(Env,fn(A,B), fn(AInst,BInst),Env2) :-  instanceOf(Env,A,AInst,Env1), instanceOf(Env1,B,BInst,Env2), !.

instanceOf(Env,listOf(A),listOf(B),NewEnv) :- instanceOf(Env,A,B,NewEnv), !.

instanceOf(Env,ref(A),ref(B),NewEnv) :- instanceOf(Env,A,B,NewEnv), !. 

instanceOf(Env,tuple(L),tuple(M),NewEnv) :- instanceOfList(Env,L,M,NewEnv), !.

instanceOf(Env,typevar(A),B,Env) :- exists(Env,A), find(Env,A,B), !.

/* 
   This is where SML type variables are replaced by Prolog variables to create an instance of a type.
   Note that the mapping from typevar(A) to B is remembered so that when we run across typevar(A) 
   again, we replace it by the same B. See the predicate definition just above this comment for that
   case.
*/
 
instanceOf(Env,typevar(A),B,[(A,B)|Env]) :- !.

instanceOf(_,A,B,_) :- 
        print('Type Error: Type '), printType(B,_), print(' is not an instance of '), 
        printType(A,_), nl, throw(typeerror('type mismatch')), !.

inst(X,Y) :- instanceOf([],X,Y,_). 

/******************************************************************************************************

  find
  ====
  The find predicate looks in the Environment for Name and returns its Type if it is found and raises 
  an error otherwise.

  Input: 
    1st - A list of (name,type) pairs providing a mapping of identifiers to types. 
    2nd - A name to find in the mapping.   

  Output:
    The type associated with the named value or throws a type error exception otherwise if not found.   

*******************************************************************************************************/

find(Env,Name,Type) :- member((Name,Type),Env), !.

find(Env,Name,Type) :- 
        print('Failed to find '), print(Name), print(' with type '), print(Type), 
        print(' in environment : '), print(Env), nl, throw(typeerror('unbound identifier')).

/******************************************************************************************************

  typeCheckMatch
  ==============
  The typecheckMatch code typechecks a match. A match is a pattern and an expression. Type 
  checking a pattern results in a new environment. The environment is a map of identifiers to 
  types. Every match corresponds to a function so the function's identifier is passed to this 
  predicate and the function's type is recorded in the environment. The input type of the function
  must match the pattern type while the output type of the function must match the type of the 
  expression. Each match of a function definition may result in further instantiating a function's
  type. The first match will start to instantiate the type fn(A,B) where A and B are Prolog 
  variables.

  Input: 
    1st - A list of (name,type) pairs providing a mapping of identifiers to types. 
    2nd - The name of the function corresponding to this match. 
    3rd - A (pattern,expression) match. 

  Output:
    While there is no direct output, the side-effect of typeCheckMatch is that the function named
    Id has its type instantiated to its actual SML type by typechecking each of its matches.   

  typeCheckMatches
  ================
  This simply calls typeCheckMatch on each element of a list of matches. 

  Input:
    1st - A list of (name,type) pairs providing a mapping of identifiers to types. 
    2nd - The function identifier.
    3rd - The list of matches. 

  Output:
    Again, no output, but the side-effect is that function identifiers are mapped to their correct
    SML type which has been instantiated by type checking the matches. 

*******************************************************************************************************/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The typecheckMatch predicate goes here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
typecheckMatches(_,_,[]) :- !.

typecheckMatches(Env,Name,[Match|Tail]) :- 
        typecheckMatch(Env,Name,Match), typecheckMatches(Env,Name,Tail), !.

/******************************************************************************************************

  gatherFuns
  ==========
  This is an important predicate. The gatherFuns predicate goes through a list of potentially mutually 
  recursive functions and finds the name of each function. This predicate returns a new environment 
  where each function name is bound to a type with Prolog variables in it. This is the purpose of 
  fn(_,_) in the code below. It builds a function type with both the input and output types as Prolog 
  variables. This is because we don't currently know the type of the function, but we will figure it
  out once the mutually recursive functions are all typechecked. As we figure out the types of the 
  functions, the Prolog variables in the fn(_,_) will unify to their correct types. See the 
  typeCheckMatch predicate above to see how this is accomplished. 

  Input: 
    A list of function definitions. 

  Output:
    A list of (id,fn(_,_)) where id is the name of each function and fn(_,_) is the function's type 
    (including uninstantiated Prolog variables).

*******************************************************************************************************/

gatherFuns([],[]) :- !.
gatherFuns([funmatch(Name,_)|Tail],[(Name,fn(_,_))|FEnv]) :- gatherFuns(Tail,FEnv), !.

/******************************************************************************************************

  typeCheckFun
  ============
  Responsible for typechecking a function definition by calling typecheck on its matches. 
  

  Input: 
    1st - An environment which is a mapping from identifier to type for all identifiers in the scope
    of the function. 
    2nd - The function definition which is the name and a series of matches. 

  Output:
    The output is a side-effect and results in instantiating each function's type to its actual type
    by typechecking each match in the function definition.


  typeCheckFuns
  =============
  Responsible for typechecking each function in a series of mutually recursive function definitions, 
  the typecheckFuns calls the typecheckFun for each function in the list of functions. 

  Input:
    1st - An environment which is a mapping from identifier to type for all identifiers in the scope
    of the function. 
    2nd - The list of function definitions.

  Output:
    The output is a side-effect and results in instantiating each function's type to its actual type
    by typechecking each match in the function definition. 

*******************************************************************************************************/

typecheckFun(Env,funmatch(Name,Matches)) :- typecheckMatches(Env,Name,Matches).

typecheckFuns(_,[]) :- !.
typecheckFuns(Env,[FunMatch|Tail]) :- typecheckFun(Env,FunMatch), typecheckFuns(Env,Tail), !.

/******************************************************************************************************

  typeCheckPat
  ============
  Responsible for typechecking patterns within function definitions and other pattern-matching within
  SML. Since a pattern never refers to an identifier defined outside the pattern, no environment 
  needs to be passed to this predicate. However, typechecking a pattern produces a new environment
  (see the accompanying text Foundations of Programming Languages, by Kent D. Lee, for more details).  

  Input: 
    A pattern to be type checked.

  Output:
    1st - The type of the pattern.
    2nd - The environment produced by the pattern consisting of a list of (identifier, type) pairs for
    each identifier that appeared within the pattern. 


  typeCheckTuplePats and typeCheckListPats
  ==================     =================
  The typecheckTuplePats and typecheckListPats predicates typecheck the list of types in a tuple or a 
  list. In the case of the list, all types must be the same. In the case of the tuple, the list of all 
  the types of the tuple elements is returned. This is because in a list all elements must be of the 
  same type while a tuple may have a different type for each element of the tuple. 

  Input:
    A list of patterns. 

  Output:
    A list of types in the case of typeCheckTuplePats or a single type in the case of typeCheckListPats.

*******************************************************************************************************/

typecheckTuplePats([],[],[]).

typecheckTuplePats([H|T],[HT|TTypes],REnv) :- 
        typecheckPat(H,HT,HEnv), typecheckTuplePats(T,TTypes,TEnv), append(HEnv,TEnv,REnv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type check lists with the typecheckListPats predicate here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

typecheckPat(idpat(nil),listOf(_),[]) :- !.

typecheckPat(idpat(Name),A,[(Name,A)]) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Other patterns go here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

typecheckPat(A,_,_) :- 
        nl, nl, print('Typechecker Error: Unknown pattern '), print(A), 
        nl, nl, throw(error('unknown pattern')).

/******************************************************************************************************

  typeCheckDec
  ============
  A declaration is part of a "let" as in 

   let dec in exp end

   The declaration is typechecked according to the type inference rules for declarations. It is given 
   an environment and returns a new environment of the newly declared item. Note that only one binding
   is declared by a let because a let with multiple bindings is "syntactic sugar" and was transformed
   into a series of one binding let expressions by the compiler. However, a binding may contain multiple
   identifiers since a dec is a pattern and expression as in 

   let pat = exp in exp end

   Declarations may also be function declarations. In the case of either a value binding or a function
   binding, the resulting type may be polymorphic and is therefore closed to transform free type 
   variables within the type to SML type variables. Free type variables correspond to Prolog variables. 

  Input: 
    1st - An environment which consists of a list if (identifier, type) pairs that are in the scope 
    seen by the let expression.
    2nd - The dec declaration that is to be typechecked. 

  Output:
    A new environment containing the newly bound identifiers from the pattern in the declaration. The
    environment consists of a list of (identifier, type) pairs for each identifier pattern found in 
    the declaration. 

*******************************************************************************************************/

typecheckDec(Env,bindval(Pat,E),NewEnv) :- typecheckPat(Pat,ExpType,NewEnv), typecheckExp(Env,E,ExpType), 
    closeExp(Pat,ExpType), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The Bind Val Rec type check goes here
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

typecheckDec(Env,funmatches(L),NewEnv) :- 
    gatherFuns(L,FunsEnv), append(FunsEnv,Env,NewEnv), typecheckFuns(NewEnv,L), closeFunTypes(FunsEnv), !.

/******************************************************************************************************

  typeCheckExp
  ============
  An expression is type checked according to the type inference rules found in 

  Foundations of Programming Languages
  by Kent D. Lee
  published by Springer
  (c) 2015

  There are three helper predicates in this definition that help to check the types of sequential
  execution, tuples, and lists. See the type inference rules for these types of expressions to see
  how these helper functions work.

  Input: 
    1st - An environment which consists of a list if (identifier, type) pairs that are in the scope 
    seen by the let expression.
    2nd - The expression that is to be typechecked. 

  Output:
    The type of the expression. 

*******************************************************************************************************/

typecheckTuple(_,[],[]) :- !.

typecheckTuple(Env,[Exp|T],[ExpT|TailType]) :- typecheckExp(Env,Exp,ExpT), typecheckTuple(Env,T,TailType), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Typechecking sequences goes here with the typecheckSequence predicate that you write here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

typecheckList(_,[],_) :- !.

typecheckList(Env,[H|T],A) :- typecheckExp(Env,H,A), typecheckList(Env,T,A), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The Anonymous Function typecheck goes here. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

typecheckExp(Env,handlexp(Exp1,Matches), T) :- 
        typecheckExp(Env,Exp1,T), typecheckMatches([('handle@',fn(exn,T))|Env],'handle@',Matches), !.

typecheckExp(Env,handlexp(Exp1,Matches), _) :- 
        typecheckExp(Env,Exp1,T), typecheckMatches([('handle@',fn(exn,S))|Env],'handle@',Matches),
        T \= S, print('Error: Expression and exception handler must result in same type.'), nl,
        print('Expression type was '), printType(T, _), print(' and handler type is '), 
        printType(S,_), nl, throw(typeerror('expression and handler typemismatch')), !.

typecheckExp(Env,expsequence(L),T) :- typecheckSequence(Env,L,T), !.

typecheckExp(_,id(nil),listOf(_)) :- !.

typecheckExp(Env,id(Name),Type) :-  find(Env,Name,Type), !. 

typecheckExp(Env,ifthen(Exp1,Exp2,Exp3), RT) :- 
        typecheckExp(Env,Exp1,bool), typecheckExp(Env,Exp2,RT), typecheckExp(Env,Exp3,RT), !.

typecheckExp(Env,ifthen(Exp1,Exp2,Exp3), _) :- 
        typecheckExp(Env,Exp1,bool), typecheckExp(Env,Exp2,ThenType), typecheckExp(Env,Exp3,ElseType), 
        print('Error: Result types of then and else expressions must match.'), nl,
        print('Then Expression type is: '), printType(ThenType,_), nl,
        print('Else Expression type is: '), printType(ElseType,_), nl, 
        throw(typeerror('result type mismatch in if-then-else expression')), !.

typecheckExp(Env,ifthen(Exp1,_,_), _) :- 
        typecheckExp(Env,Exp1,Exp1Type), Exp1Type \= bool,
        print('Error: Condition of if then expression must have bool type.'), nl,
        print('Condition Expression type was: '), printType(Exp1Type,_), nl,     
        throw(typeerror('type not bool in if-then-else expression condition')), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Case and While Do Go here
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

typecheckExp(Env,apply(Exp1,Exp2),ITT) :- 
        typecheckExp(Env,Exp1,fn(FT,TT)), typecheckExp(Env,Exp2,Exp2Type), inst(Exp2Type,Exp2TypeInst),
        catch(inst(fn(FT,TT), fn(Exp2TypeInst,ITT)),_, 
        printApplicationErrorMessage(Exp1,fn(FT,TT),Exp2,Exp2Type,ITT)), !.

typecheckExp(Env,letdec(D,Seq), T) :- 
        typecheckDec(Env,D,Env2), append(Env2, Env, InEnv), typecheckSequence(InEnv, Seq, T), !.

typecheckExp(Env,listcon(L),listOf(T)) :- typecheckList(Env,L,T), !.

typecheckExp(_,int(_),int) :- !.

typecheckExp(_,bool(_),bool) :- !.

typecheckExp(_,str(_),str) :- !.

typecheckExp(Env,tuplecon(L),tuple(T)) :- typecheckTuple(Env,L,T), !.

typecheckExp(_,Exp,_) :- 
        nl, nl, print('Typechecker Error: Unknown expression '), print(Exp),
        nl, nl, throw(error('typecheckExp: unknown expression')), !.

/******************************************************************************************************
  Here we have various error and status messages that may be displayed during type checking.  
*******************************************************************************************************/

printApplicationErrorMessage(Fun,fn(A,B),Arg,_,ResultType) :- B \= ResultType,
        print('Error: Type error in function application.'), nl, print('The function type is'), nl, nl,
        printType(fn(A,B),_), nl,nl, print('and the expected return type is'), nl, nl, printType(ResultType, _),
        nl, nl, print('in expression'), nl, nl, printExp('', apply(Fun,Arg)), 
        throw(typeerror('type incompatibility in expression')),  !.

printApplicationErrorMessage(Fun,FunType,Arg,ArgType, _) :- 
        print('Error: Type error in function application.'), nl, print('The function type is'), nl, nl,
        printType(FunType,_), nl, nl, print('and the argument type is'), nl, nl, printType(ArgType,_), nl, nl,
        print('in function application:'), nl, nl, printExp('',apply(Fun,Arg)), nl,
        throw(typeerror('type incompatibility in function application')), !.

finalStatus(typeerror) :- print('The program failed to pass the typechecker.'), nl, !.

finalStatus(_) :- print('The program passed the typechecker.'), nl, !.

warning([],_) :- !.

warning(_,fn(_,_)) :- !.

warning([_|_],_) :- 
        print('Warning: type vars not instantiated in result type initialized to dummy types!'), nl, nl, !.

/******************************************************************************************************
  Below is the initial environment that is used when typechecking a program. This environment includes
  all built-in operators and functions that are available to the Small language. 
*******************************************************************************************************/

typecheckProgram(Expression,Type) :- 
    typecheckExp([('Exception',fn(typevar(a),exn)),
                  ('raise',fn(exn,typevar(a))),
                  ('andalso',fn(tuple([bool,bool]),bool)),
                  ('orelse',fn(tuple([bool,bool]),bool)),
                  (':=',fn(tuple([ref(typevar(a)),typevar(a)]),tuple([]))),
                  ('!',fn(ref(typevar(a)),typevar(a))),
                  ('ref',fn(typevar(a),ref(typevar(a)))),
                  ('::',fn(tuple([typevar(a),listOf(typevar(a))]),listOf(typevar(a)))),
                  ('>', fn(tuple([typevar(a),typevar(a)]),bool)),
                  ('=', fn(tuple([typevar(a),typevar(a)]),bool)),
                  ('<', fn(tuple([typevar(a),typevar(a)]),bool)),
                  (@,fn(tuple([listOf(typevar(a)),listOf(typevar(a))]),listOf(typevar(a)))),
                  ('Int.fromString',fn(str,int)),
                  ('input',fn(str,str)),
                  ('explode',fn(str,listOf(str))),
                  ('implode',fn(listOf(str),str)),
                  ('println',fn(typevar(a),tuple([]))),
                  ('print',fn(typevar(a),tuple([]))),
                  ('cprint',fn(typevar(a),cprint)),
                  ('type',fn(typevar(a), str)),
                  (+,fn(tuple([int,int]),int)),
                  (-,fn(tuple([int,int]),int)),
                  (*,fn(tuple([int,int]),int)),
                  ('div',fn(tuple([int,int]),int))],
           Expression,Type). 

errorOut(error(E)) :- 
        nl, nl, print('Error: Typechecking failed. Message was : '), nl, print(E), nl, nl, halt(0).

errorOut(typeerror(E)) :- 
        nl, nl, print('Error: Typechecking failed due to type error. Message was : '), nl, print(E), 
        nl, nl, halt(0).

errorOut(E) :- 
        nl, nl, print('Error: Typechecking failed for unknown reason : '), nl, print(E), nl, nl, halt(0).

/******************************************************************************************************
  The run predicate starts everything. Some informational things are printed, like the AST. The 
  runNonInteractive can be used to run it from the command-line as the mlcomp script calls this 
  type checker. 
*******************************************************************************************************/


run :- print('Typechecking is commencing...'), nl, 
       readAST(AST), print('Here is the AST'), nl, print(AST), nl, nl, nl,
       catch(typecheckProgram(AST,Type),E,errorOut(E)),
       nl, nl, print('val it : '), printType(Type,TypeVars), nl, nl, warning(TypeVars,Type), finalStatus(Type).

runNonInteractive :- run, halt(0).
