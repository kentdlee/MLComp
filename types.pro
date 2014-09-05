/******************************************************************************************************/
/* This reads the AST from the file a.term which was written
   by the mlcomp.sml code when a program is compiled. */
/******************************************************************************************************/

readAST(AST) :- open('a.term',read,Stream), read(Stream,AST).

/******************************************************************************************************/
/* There is always at least one match in a list, so printing 
   the match list involves printing each match. The indent
   is the spaces to indent for pretty formatting. */
/******************************************************************************************************/

printMatch(Indent,match(Pat,Exp)) :- print(Indent), printPat(Pat), print(' => '), printExp('',Exp).

printMatchList(Indent,[M]) :- printMatch(Indent,M).

printMatchList(Indent,[M|T]) :- printMatch(Indent,M), printMatchList(Indent,T).

/******************************************************************************************************/
/* Here we print various expressions */
/******************************************************************************************************/

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

/******************************************************************************************************/
/* The getNextVar(Env,X) predicate is called to return a new type variable, X,
   that is not already in the Env environment. */
/******************************************************************************************************/

findFirstFree([],_,typeerror) :- 
        print('Out of Type Variables!!!'), throw(error('Out of Type Variables!!!')).

findFirstFree([X|_],Env,X) :- not(member(X,Env)).

findFirstFree([X|Tail],Env,A) :- member(X,Env), findFirstFree(Tail,Env,A).

getNextVar(Env,X) :- findFirstFree([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],Env,X).

/******************************************************************************************************/
/* The printSubTypeH is needed because function and tuple types need parens 
   around them in some circumstances to make the type print correctly. */
/******************************************************************************************************/

printSubTypeH(Env,A,NewEnv) :- var(A), printTypeH(Env,A,NewEnv).

printSubTypeH(Env,fn(A,B),NewEnv) :- print('('), printTypeH(Env,fn(A,B),NewEnv), print(')').

printSubTypeH(Env,tuple([H|T]),NewEnv) :- print('('), printTypeH(Env,tuple([H|T]),NewEnv), print(')').

printSubTypeH(Env,A,NewEnv) :- printTypeH(Env,A,NewEnv).

/******************************************************************************************************/
/* This prints the * between tuple members. */
/******************************************************************************************************/

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

/******************************************************************************************************/
/* This dictates how patterns are printed. */
/******************************************************************************************************/

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

printPat(A,[],_) :- 
        nl, nl, print('Typechecker Error: Unknown pattern '), print(A),
        nl, nl, throw(error('printPat: unknown pattern')).

/******************************************************************************************************/
/* This is used when a function is type checked to print its type. It has the effect of closing the   */
/* function's type if it still had uninstantiated variables in it.                                    */
/******************************************************************************************************/

closeFunTypes([]) :- !.

closeFunTypes([(Name,Type)|Tail]) :-
        closeFunTypes(Tail), print('val '), print(Name), print(' = fn : '), printType(Type,_), nl, !.

/******************************************************************************************************/
/* The inst predicate below makes an instance of a type. 
   This means that any type variables within the type are replaced
   with Prolog variables so the instance of the type can be unified
   with other types in Prolog. The inst predicate below corresponds to 
   inst in the type inference rules. */
/******************************************************************************************************/

exists(Env,Name) :- member((Name,_),Env).

instanceOfList(Env,[],[],Env).

instanceOfList(Env,[H|T],[G|S],NewEnv) :- instanceOf(Env,H,G,Env1), instanceOfList(Env1,T,S,NewEnv).

instanceOf(Env,A,A,Env) :- var(A), !.

instanceOf(Env,A,A,Env) :- simple(A), !. /* This handles int, bool, exn, str, and any other simple type */

instanceOf(Env,fn(A,B), fn(AInst,BInst),Env2) :-  instanceOf(Env,A,AInst,Env1), instanceOf(Env1,B,BInst,Env2), !.

instanceOf(Env,listOf(A),listOf(B),NewEnv) :- instanceOf(Env,A,B,NewEnv), !.

instanceOf(Env,ref(A),ref(B),NewEnv) :- instanceOf(Env,A,B,NewEnv), !. 

instanceOf(Env,tuple(L),tuple(M),NewEnv) :- instanceOfList(Env,L,M,NewEnv), !.

instanceOf(Env,typevar(A),B,Env) :- exists(Env,A), find(Env,A,B), !.

instanceOf(Env,typevar(A),B,[(A,B)|Env]) :- !.

instanceOf(_,A,B,_) :- 
        print('Type Error: Type '), printType(B,_), print(' is not an instance of '), 
        printType(A,_), nl, throw(typeerror('type mismatch')), !.

inst(X,Y) :- instanceOf([],X,Y,_). 

/******************************************************************************************************/
/* The find predicate looks in the Environment for Name and returns its Type
   if it is found and raises an error otherwise. */ 
/******************************************************************************************************/

find(Env,Name,Type) :- member((Name,Type),Env), !.

find(Env,Name,Type) :- 
        print('Failed to find '), print(Name), print(' with type '), print(Type), 
        print(' in environment : '), print(Env), nl, throw(typeerror('unbound identifier')).

/******************************************************************************************************/
/* The typecheckMatches code typechecks each match. */
/******************************************************************************************************/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The typecheckMatch predicate goes here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
typecheckMatches(_,_,[]).

typecheckMatches(Env,Name,[Match|Tail]) :- 
        typecheckMatch(Env,Name,Match), typecheckMatches(Env,Name,Tail).

/******************************************************************************************************/
/* This is an important predicate. The gatherFuns predicate goes
   through a list of potentially mutually recursive functions and 
   finds the name of each function. This predicate returns a new 
   environment where each function name is bound to a type with 
   Prolog variables in it. This is the purpose of fn(_,_) in
   the code below. It builds a function type with both the input
   and output types as Prolog variables. This is because we don't
   currently know the type of the function, but we will figure it
   out ounce the mutually recursive functions are all typechecked. 
   As we figure out the types of the functions, the Prolog variables
   in the fn(_,_) will unify to their correct types. */ 
/******************************************************************************************************/

gatherFuns([],[]).
gatherFuns([funmatch(Name,_)|Tail],[(Name,fn(_,_))|FEnv]) :- gatherFuns(Tail,FEnv).

/******************************************************************************************************/
/* Responsible for typechecking each function in a series of function definitions, the 
   typecheckFuns calls the typecheckFun for each function in the list of functions. */
/******************************************************************************************************/

typecheckFun(Env,funmatch(Name,Matches)) :- typecheckMatches(Env,Name,Matches).

typecheckFuns(_,[]).

typecheckFuns(Env,[FunMatch|Tail]) :- typecheckFun(Env,FunMatch), typecheckFuns(Env,Tail).

/******************************************************************************************************/
/* The typecheckTuplePats and typecheckListPats predicates typecheck the list of types in a tuple
   or a list. In the case of the list, all types must be the same. In the case of the tuple, the
   list of all the types of the tuple elements is returned. */ 
/******************************************************************************************************/

typecheckTuplePats([],[],[]).

typecheckTuplePats([H|T],[HT|TTypes],REnv) :- 
        typecheckPat(H,HT,HEnv), typecheckTuplePats(T,TTypes,TEnv), append(HEnv,TEnv,REnv).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type check lists with the typecheckListPats predicate here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/******************************************************************************************************/
/* typechecking a pattern does not need an environment, but instead returns a new environment
   of the identifiers in the pattern. The typecheckPat predicate is called as  
   typecheckPat(Pat,PatType,PatEnv) where PatType and PatEnv are the type and environment 
   of the pattern. */
/******************************************************************************************************/

typecheckPat(idpat(nil),listOf(_),[]) :- !.

typecheckPat(idpat(Name),A,[(Name,A)]) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Other patterns go here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

typecheckPat(A,_,_) :- 
        nl, nl, print('Typechecker Error: Unknown pattern '), print(A), 
        nl, nl, throw(error('unknown pattern')).

/******************************************************************************************************/
/* A declaration is part of a "let" as in 

   let dec in exp end

   The declaration is typechecked according to the type inference rules for
   declarations. It is given an environment and returns a new environment of
   the newly declared items. */
/******************************************************************************************************/

typecheckDec(Env,bindval(Pat,E),NewEnv) :- typecheckPat(Pat,ExpType,NewEnv), typecheckExp(Env,E,ExpType), 
    print('val '), printPat(Pat), print(' : '), copy_term(ExpType, Printable), printType(Printable,_), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The Bind Val Rec type check goes here
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

typecheckDec(Env,funmatches(L),NewEnv) :- 
    gatherFuns(L,FunsEnv), append(FunsEnv,Env,NewEnv), typecheckFuns(NewEnv,L), closeFunTypes(FunsEnv).

/******************************************************************************************************/
/* Typechecking an expression is called as 

   typecheckExp(Env,Exp,Type) 

   where Env is the environment, Exp is the expression to be typechecked and
   Type is the type of the expression. Typechecking a Tuple, Sequence, and a List are called when
   typechecking an expression that contains these expressions. */
/******************************************************************************************************/

typecheckTuple(_,[],[]).

typecheckTuple(Env,[Exp|T],[ExpT|TailType]) :- typecheckExp(Env,Exp,ExpT), typecheckTuple(Env,T,TailType).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Typechecking sequences goes here with the typecheckSequence predicate that you write here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

typecheckList(_,[],_).

typecheckList(Env,[H|T],A) :- typecheckExp(Env,H,A), typecheckList(Env,T,A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The Anonymous Function typecheck goes here. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

typecheckExp(Env,handlexp(Exp1,Matches), T) :- 
        typecheckExp(Env,Exp1,T), typecheckMatches([('handle@',fn(exn,T))|Env],'handle@',Matches), !.

typecheckExp(Env,handlexp(Exp1,Matches), _) :- 
        typecheckExp(Env,Exp1,T), typecheckMatches([('handle@',fn(exn,S))|Env],'handle@',Matches),
        T \= S, print('Error: Expression and exception handler must result in same type.'), nl,
        print('Expression type was '), printType(T, _), print(' and handler type is '), 
        printType(S,_), nl, throw(typeerror('expression and handler typemismatch')).

typecheckExp(Env,expsequence(L),T) :- typecheckSequence(Env,L,T).

typecheckExp(_,id(nil),listOf(_)) :- !.

typecheckExp(Env,id(Name),Type) :-  find(Env,Name,Type). 

typecheckExp(Env,ifthen(Exp1,Exp2,Exp3), RT) :- 
        typecheckExp(Env,Exp1,bool), typecheckExp(Env,Exp2,RT), typecheckExp(Env,Exp3,RT), !.

typecheckExp(Env,ifthen(Exp1,Exp2,Exp3), _) :- 
        typecheckExp(Env,Exp1,bool), typecheckExp(Env,Exp2,ThenType), typecheckExp(Env,Exp3,ElseType), 
        print('Error: Result types of then and else expressions must match.'), nl,
        print('Then Expression type is: '), printType(ThenType,_), nl,
        print('Else Expression type is: '), printType(ElseType,_), nl, 
        throw(typeerror('result type mismatch in if-then-else expression')).

typecheckExp(Env,ifthen(Exp1,_,_), _) :- 
        typecheckExp(Env,Exp1,Exp1Type), Exp1Type \= bool,
        print('Error: Condition of if then expression must have bool type.'), nl,
        print('Condition Expression type was: '), printType(Exp1Type,_), nl,     
        throw(typeerror('type not bool in if-then-else expression condition')).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Case and While Do Go here
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

typecheckExp(Env,apply(Exp1,Exp2),ITT) :- 
        typecheckExp(Env,Exp1,fn(FT,TT)), typecheckExp(Env,Exp2,Exp2Type), 
        catch(inst(fn(FT,TT), fn(Exp2Type,ITT)),_, 
        printApplicationErrorMessage(Exp1,fn(FT,TT),Exp2,Exp2Type,ITT)), !.

typecheckExp(Env,letdec(D,Seq), T) :- 
        typecheckDec(Env,D,Env2), append(Env2, Env, InEnv), typecheckSequence(InEnv, Seq, T).

typecheckExp(Env,listcon(L),listOf(T)) :- typecheckList(Env,L,T).

typecheckExp(_,int(_),int).

typecheckExp(_,bool(_),bool).

typecheckExp(_,str(_),str).

typecheckExp(Env,tuple(L),tuple(T)) :- typecheckTuple(Env,L,T). 

typecheckExp(_,Exp,_) :- 
        nl, nl, print('Typechecker Error: Unknown expression '), print(Exp),
        nl, nl, throw(error('typecheckExp: unknown expression')).

/******************************************************************************************************/
/* Here we have various error and status messages that may be displayed during type checking. */ 
/******************************************************************************************************/

printApplicationErrorMessage(Fun,fn(A,B),Arg,_,ResultType) :- B \= ResultType,
        print('Error: Type error in function application.'), nl, print('The function type is'), nl, nl,
        printType(fn(A,B),_), nl,nl, print('and the expected return type is'), nl, nl, printType(ResultType, _),
        nl, nl, print('in expression'), nl, nl, printExp('', apply(Fun,Arg)), 
        throw(typeerror('type incompatibility in expression')),  !.

printApplicationErrorMessage(Fun,FunType,Arg,ArgType, _) :- 
        print('Error: Type error in function application.'), nl, print('The function type is'), nl, nl,
        printType(FunType,_), nl, nl, print('and the argument type is'), nl, nl, printType(ArgType,_), nl, nl,
        print('in function application:'), nl, nl, printExp('',apply(Fun,Arg)), nl,
        throw(typeerror('type incompatibility in function application')).

finalStatus(typeerror) :- print('The program failed to pass the typechecker.'), nl, !.

finalStatus(_) :- print('The program passed the typechecker.'), nl, !.

warning([],_) :- !.

warning(_,fn(_,_)) :- !.

warning([_|_],_) :- 
        print('Warning: type vars not instantiated in result type initialized to dummy types!'), nl, nl, !.

/******************************************************************************************************/
/* Below is the initial environment that is used when typechecking a program. This environment includes
   all built-in operators and functions that are available to the Small language. */
/******************************************************************************************************/

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

/******************************************************************************************************/
/* The run predicate starts everything. Some informational things are printed, like the AST. The 
   runNonInteractive can be used to run it from the command-line as the mlcomp script calls this 
   type checker. */
/******************************************************************************************************/


run :- print('Typechecking is commencing...'), nl, 
       readAST(AST), print('Here is the AST'), nl, print(AST), nl, nl, nl,
       catch(typecheckProgram(AST,Type),E,errorOut(E)),
       nl, nl, print('val it : '), printType(Type,TypeVars), nl, nl, warning(TypeVars,Type), finalStatus(Type).

runNonInteractive :- run, halt(0).
