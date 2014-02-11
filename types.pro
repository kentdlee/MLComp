readAST(AST) :- open('a.term',read,Stream), read(Stream,AST).

writeMsg([]).
writeMsg([H|T]) :- write(H), writeMsg(T).

printMatchList(Indent,[M]) :- printMatch(Indent,M).
printMatchList(Indent,[M|T]) :- printMatch(Indent,M), printMatchList(Indent,T).

printExp(_, num(I)) :- print(I), !.
printExp(_, ch(S)) :-  print(S),  !.
printExp(_, str(S)) :-  print(S), !.
printExp(_, bool(B)) :- print(B), !.
printExp(_, id(Name)) :- print(Name), !. 
printExp(Indent, listcon(L)) :- print('['), printExpList(Indent,L), print(']').
printExp(Indent, tuplecon(L)) :- print('('), printExpList(Indent,L), print(')').
printExp(Indent, apply(E1,apply(E2,E3))) :- printExp(Indent, E1), print(' ('), printExp(Indent, apply(E2,E3)), print(')').
printExp(Indent, apply(E1,E2)) :- printExp(Indent, E1), print(' '), printExp(Indent, E2).
printExp(Indent, infixexp(Op,E1,E2)) :- printExp(Indent, E1), print(Op), printExp(Indent,E2).
printExp(Indent, expsequence(L)) :- print('('), nl,
                                    concat(Indent, '  ', NewIndent),
                                    printExpSequence(NewIndent,L),
                                    print(Indent), print(') ').
printExp(Indent, letdec(D,L)) :- print(Indent), print('let '), printDec(D), nl, 
                                 print(Indent), print('in'), nl,
                                 concat(Indent,'  ',SeqIndent), printExpSequence(SeqIndent,L), nl, 
                                 print(Indent), print('end').
printExp(Indent, handlexp(E,ML)) :- printExp(Indent, E), nl, print(Indent), print('handle'), nl,
                                    printMatchList(ML).
printExp(Indent, andalsoop(E1,E2)) :- printExp(Indent,E1), print(' andalso '), printExp(Indent,E2).
printExp(Indent, orelseop(E1,E2)) :- printExp(Indent,E1), print(' orelse '), printExp(Indent, E2).
printExp(Indent, ifthen(E1,E2,E3)) :- print('if '), printExp(Indent,E1), print(' then '),
                                      printExp(Indent, E2), print(' else '), printExp(Indent,E3).
printExp(Indent, whiledo(E1,E2)) :- print('while '), printExp(Indent,E1), print(' do '), printExp(Indent, E2).
printExp(Indent, caseof(E,ML)) :- print('case '), printExp(Indent,E), print(' of '), printMatchList(Indent,ML).
printExp(Indent, func(_,ML)) :- print('fn '), printMatchList(Indent,ML).

findFirstFree([],_,typeerror) :- print('Out of Type Variables!!!'), throw(error('Out of Type Variables!!!')).
findFirstFree([X|_],Env,X) :- not(member(X,Env)).
findFirstFree([X|Tail],Env,A) :- member(X,Env), findFirstFree(Tail,Env,A).

getNextVar(Env,X) :- findFirstFree([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z],Env,X).

printSubTypeH(Env,A,NewEnv) :- var(A), printTypeH(Env,A,NewEnv).
printSubTypeH(Env,fn(A,B),NewEnv) :- print('('), printTypeH(Env,fn(A,B),NewEnv), print(')').
printSubTypeH(Env,tuple([H|T]),NewEnv) :- print('('), printTypeH(Env,tuple([H|T]),NewEnv), print(')').
printSubTypeH(Env,A,NewEnv) :- printTypeH(Env,A,NewEnv).

printTupleTypeH(Env,[A],NewEnv) :- printSubTypeH(Env,A,NewEnv).
printTupleTypeH(Env,[H|T],NewEnv) :- printSubTypeH(Env,H,Env1), print(' * '), printTupleTypeH(Env1,T,NewEnv).

printTypeH(Env,typevar(A),NewEnv) :- var(A), getNextVar(Env,A), printTypeH([A|Env],typevar(A),NewEnv), !.
printTypeH(Env,typevar(A),[A|Env]) :- print(''''), print(A), !.
printTypeH(Env,tuple([]),Env) :- print(unit), !.
printTypeH(Env,tuple(L),NewEnv) :- printTupleTypeH(Env,L,NewEnv), !. 
printTypeH(Env,listOf(A),NewEnv) :- printSubTypeH(Env,A,NewEnv), print(' list'), !.
printTypeH(Env,ref(A),NewEnv) :- printSubTypeH(Env,A,NewEnv), print(' ref'), !.
printTypeH(Env,fn(A,B),Env2) :- var(A), printSubTypeH(Env,A,Env1), print(' -> '), printTypeH(Env1,B,Env2), !.
printTypeH(Env,fn(tuple(L),B),NewEnv) :- printTypeH(Env,tuple(L),Env1), print(' -> '), printTypeH(Env1,B,NewEnv), !.
printTypeH(Env,fn(A,B),Env2) :- printSubTypeH(Env,A,Env1), print(' -> '), printTypeH(Env1,B,Env2), !.
printTypeH(Env,typeerror,Env) :- print(typeerror), !.
printTypeH(Env,T,Env) :- simple(T), print(T), !. /* This handles int, bool, str, exn, and a possible variable */
printTypeH(Env,T,Env) :- nl, nl, print('Error: Attempt to print unknown type '), print(T), nl, nl, throw(typeerror('unknown type')), !.

printType(T,TypeVars) :- printTypeH([],T,TypeVars).

printPats([]).
printPats([Elm]) :- printPat(Elm), !.
printPats([H|T]) :- printPat(H), print(','), printPats(T), !.

printPat(idpat(nil)) :- print(nil), !.
printPat(idpat(Name)) :- print(Name), !.
printPat(infixpat(::,Pat1,Pat2)) :- printPat(Pat1), print('::'), printPat(Pat2). 
printPat(tuplepat(L)) :- print('('), printPats(L), print(')'), !.
printPat(numpat(i)) :- print(i), !.
printPat(boolpat(b)) :- print(b), !.
printPat(strpat(s)) :- print(s), !.
printPat(A,[],_) :- writeMsg([nl,nl,'Typechecker Error: Unknown pattern ',print(A),nl,nl]), throw(error('unknown pattern')).

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
instanceOf(_,A,B,_) :- print('Type Error: Type '), printType(B,_), print(' is not an instance of '), printType(A,_), nl, throw(typeerror('type mismatch')), !.

makeInstance(X,Y) :- instanceOf([],X,Y,_). 

exists(Env,Name) :- member((Name,_),Env), !.

find(Env,Name,Type) :- member((Name,Type),Env), !.
find(Env,Name,Type) :- writeMsg(['Failed to find ',Name,' with type ',Type,' in environment : ']), print(Env), nl, throw(typeerror('unbound identifier')).

% The typecheckMatch predicate goes here.

typecheckMatches(_,_,[]).
typecheckMatches(Env,Name,[Match|Tail]) :- typecheckMatch(Env,Name,Match), typecheckMatches(Env,Name,Tail).

typecheckFun(Env,funmatch(Name,Matches)) :- typecheckMatches(Env,Name,Matches).

gatherFuns(Env,[],Env).
gatherFuns(Env,[funmatch(Name,_)|Tail],NewEnv) :- gatherFuns([(Name,fn(_,_))|Env],Tail,NewEnv).

typecheckFuns(_,[]).
typecheckFuns(Env,[FunMatch|Tail]) :- typecheckFun(Env,FunMatch), typecheckFuns(Env,Tail).

typePats([],[],[]).
typePats([H|T],REnv,[HT|TTypes]) :- typePat(H,HEnv,HT), typePats(T,TEnv,TTypes), append(HEnv,TEnv,REnv).

typePat(idpat(nil),[],listOf(_)) :- !.
typePat(idpat(Name),[(Name,A)],A) :- !.

% Other Patterns go here

typePat(A,[],_) :- writeMsg([nl,nl,'Typechecker Error: Unknown pattern ',print(A),nl,nl]), throw(error('unknown pattern')).

printFunTypes([]).
printFunTypes([(Name,Type)|Tail]) :- printFunTypes(Tail), print('val '), print(Name), print(' = fn : '), printType(Type,_), nl.

typeDec(Env,bindval(Pat,E),NewEnv) :- typePat(Pat,NewEnv,ExpType), typecheck(Env,E,ExpType), 
    print('val '), printPat(Pat), print(' : '), copy_term(ExpType, Printable), printType(Printable,_), nl.

% Bind Val Rec goes here 

typeDec(Env,funmatches(L),NewEnv) :- gatherFuns([],L,NewEnv), append(NewEnv,Env,FunEnv), typecheckFuns(FunEnv,L), printFunTypes(NewEnv).

% Anonymous Function Type Check goes here

typecheck(Env,handlexp(Exp1,Matches), T) :- typecheck(Env,Exp1,T), typecheckMatches([('handle@',fn(exn,T))|Env],'handle@',Matches), !.

typecheck(Env,handlexp(Exp1,Matches), _) :- typecheck(Env,Exp1,T), typecheckMatches([('handle@',fn(exn,S))|Env],'handle@',Matches),
                                            T \= S, print('Error: Expression and exception handler must result in same type.'), nl,
                                            print('Expression type was '), printType(T, _), print(' and handler type is '), 
                                            printType(S,_), nl, throw(typeerror('expression and handler typemismatch')).

typecheck(Env,expsequence(L),T) :- typecheckSequence(Env,L,T).

typecheck(_,id(nil),listOf(_)) :- !.

typecheck(Env,id(Name),Type) :-  find(Env,Name,Type). 

typecheck(Env,ifthen(Exp1,Exp2,Exp3), RT) :- typecheck(Env,Exp1,bool), typecheck(Env,Exp2,RT), typecheck(Env,Exp3,RT), !.

typecheck(Env,ifthen(Exp1,Exp2,Exp3), _) :- typecheck(Env,Exp1,bool), typecheck(Env,Exp2,ThenType), typecheck(Env,Exp3,ElseType), 
                                                    print('Error: Result types of then and else expressions must match.'), nl,
                                                    print('Then Expression type is: '), printType(ThenType,_), nl,
                                                    print('Else Expression type is: '), printType(ElseType,_), nl, 
                                                    throw(typeerror('result type mismatch in if-then-else expression')).
                                                    

typecheck(Env,ifthen(Exp1,_,_), _) :- typecheck(Env,Exp1,Exp1Type), Exp1Type \= bool,
                                                    print('Error: Condition of if then expression must have bool type.'), nl,
                                                    print('Condition Expression type was: '), printType(Exp1Type,_), nl,     
                                                    throw(typeerror('type not bool in if-then-else expression condition')).
                             
% Case and While Do Go here

typecheck(Env,apply(Exp1,Exp2),ITT) :- 
     typecheck(Env,Exp1,fn(FT,TT)), typecheck(Env,Exp2,Exp2Type), 
     catch(makeInstance(fn(FT,TT), fn(Exp2Type,ITT)),_, printApplicationErrorMessage(Exp1,fn(FT,TT),Exp2,Exp2Type,ITT)), !.

typecheck(Env,letdec(D,Seq), T) :- typeDec(Env,D,Env2), append(Env2, Env, InEnv), typecheckSequence(InEnv, Seq, T).

typecheck(Env,listcon(L),listOf(T)) :- typeCheckListElements(Env,L,T).

typecheck(_,num(_),int).

typecheck(_,bool(_),bool).

typecheck(_,str(_),str).

typecheck(Env,tuple(L),tuple(T)) :- typecheckTuple(Env,L,T). 

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
      
typecheckTuple(_,[],[]).
typecheckTuple(Env,[Exp|T],[ExpT|TailType]) :- typecheck(Env,Exp,ExpT), typecheckTuple(Env,T,TailType).


typeCheckListElements(_,[],_).
typeCheckListElements(Env,[H|T],A) :- typecheck(Env,H,A), typeCheckListElements(Env,T,A).

% Expression sequence type checking goes here

finalStatus(typeerror) :- print('The program failed to pass the typechecker.'), nl, !.
finalStatus(_) :- print('The program passed the typechecker.'), nl, !.

warning([],_) :- !.
warning(_,fn(_,_)) :- !.
warning([_|_],_) :- print('Warning: type vars not instantiated in result type initialized to dummy types!'), nl, nl, !.

typecheckProgram(Expression,Type) :- 
       typecheck([('Exception',fn(typevar(a),exn)),
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

errorOut(error(E)) :- nl, nl, print('Error: Typechecking failed. Message was : '), nl, print(E), nl, nl, halt(0).
errorOut(typeerror(E)) :- nl, nl, print('Error: Typechecking failed due to type error. Message was : '), nl, print(E), nl, nl, halt(0).
errorOut(E) :- nl, nl, print('Error: Typechecking failed for unknown reason : '), nl, print(E), nl, nl, halt(0).


run :- print('Typechecking is commencing...'), nl, 
       readAST(AST), print('Here is the AST'), nl, print(AST), nl, nl, nl,
       catch(typecheckProgram(AST,Type),E,errorOut(E)),
       nl, nl, print('val it : '), printType(Type,TypeVars), nl, nl, warning(TypeVars,Type), finalStatus(Type).


runNonInteractive :- run, halt(0).
