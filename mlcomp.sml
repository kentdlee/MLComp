structure mlcomp =
struct
open MLAS;
    
     structure mlcompLrVals = mlcompLrValsFun(structure Token = LrParser.Token) 
               
     structure mlcompLex = mlcompLexFun(structure Tokens = mlcompLrVals.Tokens)

     structure mlcompParser = Join(structure Lex= mlcompLex
                                structure LrParser = LrParser
                                structure ParserData = mlcompLrVals.ParserData)
              
     exception nameMismatch;

     val debug = false;

     val input_line =
       fn f =>
          let val sOption = TextIO.inputLine f
          in
            if isSome(sOption) then
               Option.valOf(sOption)
            else
               ""
          end

     val parse = 
         fn filename =>
           let val instrm = TextIO.openIn filename
               val lexer = mlcompParser.makeLexer(fn i => input_line instrm)
               val _ = mlcompLex.UserDeclarations.pos := 1
               val error = fn (e,i:int,_) => 
                               TextIO.output(TextIO.stdOut," line " ^ (Int.toString i) ^ ", Error: " ^ e ^ "\n")
           in 
                mlcompParser.parse(30,lexer,error,()) before TextIO.closeIn instrm
           end

     (* These are the operators in the order the CoCo virtual machine has them for index values *)

     val cmp_op = ["<","<=","=","<>",">",">=","in","notin","is","isnot","excmatch","BAD"];

     (* Labels are needed in the code generation when a jump is required. The nextLabel
        function returns a unique string that can be used as a label. *)
     val label = ref 0;

     fun nextLabel() = 
         let val lab = !label
         in 
           label := !label + 1;
           "L"^Int.toString(lab)
         end

     (* Here are some utility functions that are used for the lists of locals, globals,
        free variables, cell variables, and bindings *)

     fun commaSepList nil = ""
       | commaSepList [h] = h
       | commaSepList (h::t) = h ^ ", " ^ (commaSepList t)

     exception notFound;

     fun print text = TextIO.output(TextIO.stdOut,text^"\n");
     fun printList lst = TextIO.output(TextIO.stdOut,"["^(commaSepList lst)^"]\n");

     fun raiseit(e,msg) = 
         (if debug then print(msg) else ();
          raise e)

     fun boundTo(v:string,nil) = raiseit(notFound, "Unbound variable "^v^" in bindings")
       | boundTo(v,(x,y)::t) = if v = x then y else boundTo(v,t);

     fun bindingsToList nil = []
       | bindingsToList ((x,y)::t) = (x ^ "->" ^ y)::(bindingsToList t) 

     fun bindingsToString bindings = "[" ^ (commaSepList (bindingsToList bindings)) ^ "]"
 
     fun printBindings bindings = TextIO.output(TextIO.stdOut,(bindingsToString bindings)^"\n");

     fun indexOf ((v:string),nil)  = raiseit(notFound,"The item "^v^" was not found in the list.")
       | indexOf (v,h::t) = if h = v then 0 else 1+(indexOf(v,t))

     fun lookupIndex(name,lst) = 
         (Int.toString(indexOf(name,lst))) handle notFound =>
          (TextIO.output(TextIO.stdOut,name^" was not found in the list [" ^ (commaSepList lst) ^ "].\n");
           raise notFound)

     fun exists (x:string) L = List.exists (fn y => x = y) L

     fun removeDups nil = nil
       | removeDups (h::t) = 
            if (exists h t) then removeDups t else h::(removeDups t)

     fun member (x:string) nil = false
       | member x (h::t) = x = h orelse (member x t)

     fun listdiff L1 L2 = List.filter (fn x => not (member x L2)) L1

     fun concat nil = nil
       | concat (h::t) = h @ (concat t)

     (* These names make debugging easier when something is not yet implemented. *)

     fun nameOf(int(i)) = "int"
       | nameOf(ch(c)) = "ch"
       | nameOf(boolval(b)) = "bool"
       | nameOf(id(name)) = "id"
       | nameOf(apply(e1,e2)) = "apply"
       | nameOf(listcon(L)) = "listcon"
       | nameOf(str(s)) = "str"
       | nameOf(infixexp(operator,e1,e2)) = operator
       | nameOf(tuplecon(L)) = "tuplecon"
       | nameOf(expsequence(L)) = "expsequence"
       | nameOf(letdec(L1,L2)) = "letdec"
       | nameOf(handlexp(e,L)) = "handlexp"
       | nameOf(raisexp(e)) = "raisexp"
       | nameOf(ifthen(e1,e2,e3)) = "ifthen"
       | nameOf(whiledo(e1,e2)) = "whiledo"
       | nameOf(func(idnum,L)) = "func"^Int.toString(idnum);

     fun writeTerm(outFile,ast) = 
         let fun print(s) = TextIO.output(outFile,s)

             fun println(s) = print(s^"\n")

             fun printList(write,indent,nil) = ()
               | printList(write,indent,[h]) = write(indent,h)
               | printList(write,indent,h::t) = (write(indent,h); print(indent^","); printList(write,indent,t))

             fun writeExp(indent,int(i)) = print("int('"^i^"')")

               | writeExp(indent,boolval(b)) = print("bool('"^b^"')")

               | writeExp(indent,ch(c)) = print("ch('"^c^"')")

               | writeExp(indent,str(s)) = print("str('"^s^"')")

               | writeExp(indent,id(name)) = print("id('"^name^"')")

               | writeExp(indent,listcon(L)) = 
                     (print("listcon([");
                      printList(writeExp,indent,L);
                      print("])"))

               | writeExp(indent,tuplecon(L)) = 
                     (print("tuplecon([");
                      printList(writeExp,indent,L);
                      print("])"))

               | writeExp(indent,apply(exp1,exp2)) = 
                     (print("apply("); 
                      writeExp(indent,exp1); 
                      print(","); 
                      writeExp(indent,exp2);
                      print(")"))

               | writeExp(indent,infixexp(opstr,exp1,exp2)) = 
                     (print("apply(id('"^opstr^"'),"); 
                      writeExp(indent,tuplecon([exp1,exp2]));
                      print(")"))

               | writeExp(indent,expsequence(L)) = 
                     (println(indent^"expsequence(["); 
                      printList(writeExp,indent^"   ",L); 
                      println(indent^"])"))

               | writeExp(indent,letdec(dec,L)) = 
                     (println(indent^"letdec("); 
                      writeDec(indent^"   ",dec); 
                      println(",\n"^indent^"   ["); 
                      printList(writeExp,indent^"    ",L); 
                      println("\n"^indent^"   ])"))

               | writeExp(indent,raisexp(exp)) = 
                     (print("apply(id('raise'),"); 
                      writeExp(indent,exp); 
                      print(")"))

               | writeExp(indent,handlexp(exp,L)) = 
                     (println(indent^"handlexp("); 
                      writeExp(indent^"   ",exp); 
                      println("\n"^indent^", ["); 
                      printList(writeMatch,indent^"   ",L); 
                      println(indent^"])"))


               | writeExp(indent,ifthen(exp1,exp2,exp3)) = 
                     (println(indent^"ifthen("); 
                      writeExp(indent^"   ",exp1); 
                      println("\n"^indent^","); 
                      writeExp(indent^"   ",exp2);
                      println("\n"^indent^","); 
                      writeExp(indent^"   ",exp3);
                      println("\n"^indent^")"))
 
               | writeExp(indent,whiledo(exp1,exp2)) = 
                     (println(indent^"whiledo("); 
                      writeExp(indent^"   ",exp1); 
                      println("\n"^indent^","); 
                      writeExp(indent^"   ",exp2);
                      println("\n"^indent^")"))
               
               | writeExp(indent,func(i,L)) = 
                     (println(indent^"func('anon@"^Int.toString(i)^"',["); 
                      printList(writeMatch,indent^"   ",L); 
                      print("\n"^indent^"])"))

             and writeMatch(indent,match(pat,exp)) = 
                     (print(indent^"match(");
                      writePat(indent,pat);
                      print(indent^",");
                      writeExp(indent,exp);
                      print(indent^")"))

             and writePat(indent,intpat(i)) = print("intpat("^i^")")

               | writePat(indent,boolpat(b)) = print("boolpat("^b^")")

               | writePat(indent,chpat(c)) = print("chpat('"^c^"')")
              
               | writePat(indent,strpat(s)) = print("strpat('"^s^"')")

               | writePat(indent,idpat(name)) = print("idpat('"^name^"')")
           
               | writePat(indent,wildcardpat) = print("wildcardpat")
    
               | writePat(indent,infixpat(opstr,pat1,pat2)) = 
                     (print("infixpat('"^opstr^"',"); 
                      writePat(indent,pat1); 
                      print(","); 
                      writePat(indent,pat2);
                      print(")"))

               | writePat(indent,listpat(L)) = 
                     (print("listpat([");
                      printList(writePat,indent,L);
                      print("])"))

               | writePat(indent,tuplepat(L)) = 
                     (print("tuplepat([");
                      printList(writePat,indent,L);
                      print("])"))

               | writePat(indent,aspat(name,pat)) = 
                     (print("aspat('"^name^"',");
                      writePat(indent,pat);
                      print(")"))

            and writeDec(indent,bindval(pat,exp)) = 
                     (print(indent^"bindval(");
                      writePat(indent,pat);
                      print(",");
                      writeExp(indent,exp);
                      print(")"))

              | writeDec(indent,bindvalrec(pat,exp)) = 
                     (print(indent^"bindvalrec(");
                      writePat(indent,pat);
                      print(",");
                      writeExp(indent,exp);
                      print(indent^")"))

              | writeDec(indent,funmatch(name,L)) = 
                     (println(indent^"funmatch('"^name^"',[");
                      printList(writeMatch,indent^"   ",L);
                      println(indent^"])"))

              | writeDec(indent,funmatches(L)) = 
                     (println(indent^"funmatches([");
                      printList(writeDec,indent,List.map (fn (x,y) => funmatch(x,y)) L);
                      println(indent^"])"))
         in
           writeExp("",ast);
           println(".")
         end

     exception Unimplemented; 

    (* The constants function is responsible for returning a list of all literal values in a program. 
       Literals are things like integers, strings, characters, etc. *)
     fun patConsts(intpat(s)) = [s]
       | patConsts(boolpat(b)) = [if b = "true" then "True" else "False"]
       | patConsts(chpat(s)) = [s]
       | patConsts(strpat(s)) = [s]
       | patConsts(idpat(n)) = []
       | patConsts(aspat(n,pat)) = patConsts(pat)
       | patConsts(wildcardpat) = []
       | patConsts(infixpat(oper,pat1,pat2)) = patConsts(pat1) @ patConsts(pat2)
       | patConsts(tuplepat(L)) = List.foldr (fn (x,y) => (patConsts x) @ y) [] L
       | patConsts(listpat(L)) =  List.foldr (fn (x,y) => (patConsts x) @ y) [] L

     fun constants(ast) = 
         let fun decconsts(bindval(_,exp)) = con exp
               | decconsts(bindvalrec(idpat(name),func(idnum,L))) = ["code("^name^")"]
               | decconsts(bindvalrec(_,_)) = 
                 (TextIO.output(TextIO.stdOut,"val rec construct must be val rec id = (fn <x> => ...)\n");
                  raise Unimplemented)
               | decconsts(funmatch(name,nil)) = []
               | decconsts(funmatch(name,L)) = ["code("^name^")"]
               | decconsts(funmatches(L)) = List.foldr (fn (x,y) => (decconsts (funmatch(x))) @ y) [] L

             and con(int(i)) = [i]
               | con(boolval(b)) = [if b = "true" then "True" else "False"]
               | con(id(name)) = []
               | con(infixexp(operator,t1,t2)) = (con t1) @ (con t2)
               | con(expsequence(L)) = (List.foldr (fn (x,y) => (con x) @ y) [] L)
               | con(letdec(d,L2)) = (decconsts d) @ (List.foldr (fn (x,y) => (con x) @ y) [] L2)   
               | con(apply(t1,t2)) = (con t1) @ (con t2)
               | con(raisexp(t)) = (con t)
               | con(listcon(L)) = (List.foldr (fn (x,y) => (con x)@y) [] L)
               | con(func(idnum,matchlist)) = ["code(anon@"^Int.toString(idnum)^")"]  
               | con(handlexp(t1,L)) = (con t1) @ ((List.foldr (fn (match(pat,exp),y) => (patConsts pat) @ (con exp) @ y) []) L)                   
               | con(tuplecon(L)) = List.foldr (fn (x,y) => (con x) @ y) [] L

               | con(str(s)) = [s]
               | con(other) = 
                  (TextIO.output(TextIO.stdOut, "\nAttempt to get constants for expression not currently supported!\n");
                   TextIO.output(TextIO.stdOut, "Expression was: " ^ nameOf(other) ^ "\n");
                   raise Unimplemented) 
         in
             removeDups (con ast)
         end

     (* The function localBindings deserves some explanation. This function is given a program/ast/expression and 
        computes all the bindings that are created in that expression. Along the way, these bindings are used to 
        determine any free variables that appear in the expression. When such a free variable is found, the variable 
        is added to the list of free variables. A free variable is any variable that is referenced but does not appear
        in the bindings. The cellVars are also computed. A cellVar is any binding that appears as a freevar in some
        nested function of this function. 

        The nested free variables must be computed from the global bindings and pattern bindings only. Bindings 
        created in the outer scope are not used when finding the list of free variables. This is because free variables
        must include any variables defined in the outer scope. 

        The return value from this function is a tuple of the bindings created in this expression, the free variables, 
        and the cell variables that appear in the expression. The free variables that are returned 
        are a list of the source names of the free variables, not the target names. This is because the free 
        variable target names cannot be determined without examining the enclosing bindings which are not available 
        in this function as it is written. 

        The cell variables that are returned are a list of the target program variables. *)

     fun patBindings(idpat("nil"),scope) = [] 
       | patBindings(intpat(v),scope) = []
       | patBindings(boolpat(v),scope) = []
       | patBindings(strpat(v),scope) = []
       | patBindings(idpat(name),scope) = [(name,name^"@"^Int.toString(scope))]
       | patBindings(infixpat("::",pat1,pat2),scope) = (patBindings(pat1,scope)) @ (patBindings(pat2,scope))
       | patBindings(tuplepat(L),scope) = List.foldr (fn (x,y) => patBindings(x,scope)@y) [] L
       | patBindings(_,scope) = 
         (TextIO.output(TextIO.stdOut, "\nAttempt to gather locals for unsupported pattern!\n");
          raise Unimplemented) 

     and localBindings(ast,pats,globalBindings,scope) = 
         (* The freeVars is a list of the identifiers from the source program. 
            The cellVars is a list of the identifiers from the target (i.e. CoCo) program. 
            The theBindings is a list of source program to target program identifier tuples mapping 
            source program identifiers to target program identifiers. 
         *)
         let val freeVars = ref []

             val cellVars = ref []

             val theBindings = ref []

             fun check(name,bindings) = 
                  (boundTo(name,bindings);()) handle notFound => (boundTo(name,pats);()) handle notFound => (freeVars := name::(!freeVars))

             fun addIt(value,var) = (var := (value::(!var)); value)

             fun bindingsOf(int(n),bindings,scope) = ()
               | bindingsOf(boolval(n),bindings,scope) = ()
               | bindingsOf(ch(c),bindings,scope) = ()
               | bindingsOf(str(s),bindings,scope) = ()
               | bindingsOf(id("nil"),bindings,scope) = ()
               | bindingsOf(id(name),bindings,scope) = check(name,bindings)
               | bindingsOf(listcon(L),bindings,scope) = (List.map (fn x => (bindingsOf(x,bindings,scope))) L; ())
               | bindingsOf(tuplecon(L),bindings,scope) = (List.map (fn x => (bindingsOf(x,bindings,scope))) L; ())
               | bindingsOf(apply(exp1,exp2),bindings,scope) = (bindingsOf(exp1,bindings,scope); bindingsOf(exp2,bindings,scope))
               | bindingsOf(infixexp(operator,exp1,exp2),bindings,scope) = (bindingsOf(exp1,bindings,scope); bindingsOf(exp2,bindings,scope))
               | bindingsOf(handlexp(exp,L),bindings,scope) =  
                 (bindingsOf(exp,bindings,scope); 
                  List.map (fn match(pat,exp) => 
                     let val patBs = patBindings(pat,scope+1)
                     in
                       bindingsOf(exp,patBs@bindings,scope+1);
                       List.map (fn b => addIt(b,theBindings)) patBs
                     end) L;
                  ())
              
               | bindingsOf(raisexp(exp),bindings,scope) = bindingsOf(exp,bindings,scope)
               | bindingsOf(expsequence(L),bindings,scope) = (List.map (fn x => (bindingsOf(x,bindings,scope))) L; ())
               | bindingsOf(letdec(d,L2),bindings,scope) = 
                 let val newbindings = decbindingsOf(d,bindings,scope)
                 in 
                   List.map (fn x => (bindingsOf(x, newbindings @ bindings,scope+1))) L2; 
                   ()
                 end
               | bindingsOf(func(idnum,L),bindings,scope) = 
                 let val varNames = List.map (fn (x,y) => x) (pats@bindings)
                 in 

                     List.map (fn match(pat,exp) =>
                                  let val patB = patBindings(pat,scope+1)
                                      val lb = localBindings(exp,patB,globalBindings,scope+1)
                                      val newfreevars = removeDups (#2(lb))
                                  in
                                      List.map (fn freevar => if exists freevar varNames 
                                                              then (addIt(boundTo(freevar,pats@bindings),cellVars)) 
                                                              else (addIt(freevar,freeVars))
                                               ) newfreevars
                                  end) L;
                     ()
                 end 

               | bindingsOf(other,bindings,scope) = 
                  (TextIO.output(TextIO.stdOut, "\nAttempt to call localBindings for expression not currently supported!\n");
                   TextIO.output(TextIO.stdOut, "Expression was: " ^ nameOf(other) ^ "\n");
                   raise Unimplemented) 



             and decbindingsOf(bindval(pat,exp),bindings,scope) = 
                 let val newbindings = patBindings(pat,scope)
                 in
                   bindingsOf(exp,newbindings@bindings,scope+1);
                   List.map (fn x => addIt(x,theBindings)) newbindings
                 end

               | decbindingsOf(bindvalrec(idpat(name),exp as func(idnum,L)),bindings,scope) = 
                 let val locals = (name,name)::(pats@bindings)
                     val idNames = List.map (fn (x,y) => x) locals
                 in
                     List.map (fn match(pat,exp) =>
                                  let val patB = patBindings(pat,scope+1)
                                      val lb = localBindings(exp,patB,globalBindings,scope+1)
                                      val freevars = #2(lb)
                                  in
                                      List.map (fn var => if exists var idNames 
                                                              then (addIt(boundTo(var, locals), cellVars)) 
                                                              else (addIt(var,freeVars))
                                               ) freevars
                                  end) L;
                     [addIt((name,name),theBindings)]
                 end

               | decbindingsOf(bindvalrec(_,exp),bindings,scope) =
                 (print "This should never happen";
                  raise Unimplemented)

               | decbindingsOf(funmatch(name,nil),bindings,scope) = []
               | decbindingsOf(funmatch(name,L),bindings,scope) = 
                 let val locals = (name,name)::(pats@bindings)
                     val idNames = List.map (fn (x,y) => x) ((name,name)::(pats@bindings))
                 in
                     List.map (fn match(pat,exp) =>
                                  let val patB = patBindings(pat,scope+1)
                                      val lb = localBindings(exp,patB,globalBindings,scope+1)
                                      val freevars = #2(lb)
                                  in
                                      List.map (fn var => if exists var idNames 
                                                          then (addIt(boundTo(var, locals), cellVars)) 
                                                          else (addIt(var,freeVars))
                                               ) freevars
                                  end) L;
                     [addIt((name,name),theBindings)]
                 end 

               | decbindingsOf(funmatches(L),bindings,scope) = 
                 let val nameList = List.map (fn (name,matchlist) => name) L
                 in 
                   List.foldr (fn ((name,matchlist),y) => 
                       (let val adjustedBindings = List.map (fn x => (x,x)) (listdiff nameList [name])
                       in
                         decbindingsOf(funmatch(name,matchlist),adjustedBindings@bindings,scope)
                       end) @ y) [] L                       
                 end

         in
           if debug then print("Entering localBindings function for this AST") else ();
           if debug then writeTerm(TextIO.stdOut, ast) else ();
           bindingsOf(ast,globalBindings,scope);
           if debug then ( 
              print("Exiting localBindings function for this ast");
              writeTerm(TextIO.stdOut, ast);
              print("The cellVars are: ");
              printList(!cellVars);
              print("The freeVars are: ");
              printList(!freeVars);
              print("The bindings are:");
              printBindings(!theBindings)
           ) else ();
           (!theBindings, !freeVars, !cellVars)
         end

     (* How a value is stored or loaded depends on the type of variable it is being
        stored from or loaded into. A local can be stored or loaded with a store_fast
        or load_fast respectively. A free variable or a cell varaible is stored or loaded
        with a store_deref or load_deref. A global is stored or loaded with a store_global
        or a load_global. These functions are given a name and find that name in one
        of the lists for locals, cell variable, free variables, or globals and then
        generate (write) the appropriate store or load instruction. *)
      
     fun store(name,outFile,indent,locals,freeVars,cellVars,globals,bindings) =
         let val realName = boundTo(name,bindings)
         in
           let val index = indexOf(realName, locals)
           in
             TextIO.output(outFile,indent^"STORE_FAST "^Int.toString(index)^"\n")
           end 

           handle notFound =>
           
           let val index = indexOf(realName,freeVars) + length(cellVars)
           in
             TextIO.output(outFile,indent^"STORE_DEREF "^Int.toString(index)^"\n")
           end

           handle notFound =>

           let val index = indexOf(realName,cellVars) 
           in
             TextIO.output(outFile,indent^"STORE_DEREF "^Int.toString(index)^"\n")
           end

           handle notFound =>

           let val index = indexOf(realName, globals)
           in
             TextIO.output(outFile,indent^"STORE_GLOBAL "^Int.toString(index)^"\n")
           end
        
           handle notFound => 
         
           (TextIO.output(TextIO.stdOut,name^" not found in locals, freeVars, cellVars, or globals during store instruction.\n");
            print("The locals are: ["^(commaSepList locals)^"]");
            print("The freeVars are: ["^(commaSepList freeVars)^"]");
            print("The cellVars are: ["^(commaSepList cellVars)^"]");
            print("The globals are: ["^(commaSepList globals)^"]");
            print("The bindings are: ");
            printBindings(bindings);
            raise notFound)
         end 

         handle notFound =>
           (TextIO.output(TextIO.stdOut,name^" not found in bindings during store instruction.\n");
            print("The locals are: ["^(commaSepList locals)^"]");
            print("The freeVars are: ["^(commaSepList freeVars)^"]");
            print("The cellVars are: ["^(commaSepList cellVars)^"]");
            print("The globals are: ["^(commaSepList globals)^"]");
            print("The bindings are: ");
            printBindings(bindings);
            raise notFound)

     fun load(name,outFile,indent,locals,freeVars,cellVars,globals,bindings) =
         let val realName = boundTo(name,bindings)
         in
           let val index = indexOf(realName, locals)
           in
             TextIO.output(outFile,indent^"LOAD_FAST "^Int.toString(index)^"\n")
           end 

           handle notFound =>
           
           let val index = indexOf(realName,freeVars) + length(cellVars)
           in
             TextIO.output(outFile,indent^"LOAD_DEREF "^Int.toString(index)^"\n")
           end

           handle notFound =>

           let val index = indexOf(realName,cellVars) 
           in
             TextIO.output(outFile,indent^"LOAD_DEREF "^Int.toString(index)^"\n")
           end

           handle notFound =>

           let val index = indexOf(realName, globals)
           in
             TextIO.output(outFile,indent^"LOAD_GLOBAL "^Int.toString(index)^"\n")
           end
        
           handle notFound => 
         
           (TextIO.output(TextIO.stdOut,name^" not found in locals, freeVars, cellVars, or globals during load instruction.\n");
            print("The locals are: ["^(commaSepList locals)^"]");
            print("The freeVars are: ["^(commaSepList freeVars)^"]");
            print("The cellVars are: ["^(commaSepList cellVars)^"]");
            print("The globals are: ["^(commaSepList globals)^"]");
            print("The bindings are: ");
            printBindings(bindings);
            raise notFound)
         end 

         handle notFound =>
           (TextIO.output(TextIO.stdOut,name^" not found in bindings during load instruction.\n");
            print("The locals are: ["^(commaSepList locals)^"]");
            print("The freeVars are: ["^(commaSepList freeVars)^"]");
            print("The cellVars are: ["^(commaSepList cellVars)^"]");
            print("The globals are: ["^(commaSepList globals)^"]");
            print("The bindings are: ");
            printBindings(bindings);
            raise notFound)

     (* The code generation function found here traverses the abstract syntax tree for a program
        as a recursive descent of the tree, generating the appropriate instructions as it traverses
        the tree. *)
           
     fun codegen(int(i),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         let val index = lookupIndex(i,consts)
         in
           TextIO.output(outFile,indent^"LOAD_CONST "^index^"\n")
         end

       | codegen(boolval(b),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         let val value = if b = "true" then "True" else "False"
             val index = lookupIndex(value,consts)
         in
           TextIO.output(outFile,indent^"LOAD_CONST "^index^"\n")
         end

       | codegen(str(s),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         let val index = lookupIndex(s,consts)
         in
           TextIO.output(outFile,indent^"LOAD_CONST "^index^"\n")
         end

       | codegen(id("nil"),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         TextIO.output(outFile, indent^"BUILD_FUNLIST 0\n")
            
       | codegen(id(name),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         load(name,outFile,indent,locals,freeVars,cellVars,globals,env)

       | codegen(apply(t1,t2),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) =
         let val _ = codegen(t1,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)
             val _ = codegen(t2,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)              
         in
           TextIO.output(outFile,indent^"CALL_FUNCTION 1\n")
         end  

       | codegen(listcon(L),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         let val size = length(L)
         in
           List.map (fn x => codegen(x,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)) L;
           TextIO.output(outFile,indent^"BUILD_FUNLIST "^Int.toString(size)^"\n")
         end

       | codegen(infixexp("@",t1,t2),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) =
         let val _ = codegen(t1,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)
             val _ = codegen(t2,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)              
         in
           TextIO.output(outFile,indent^"BINARY_ADD\n")
         end

       | codegen(infixexp("::",t1,t2),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) =
         let val _ = codegen(t1,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)
             val _ = codegen(t2,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)              
         in
           TextIO.output(outFile,indent^"CONS_FUNLIST\n")
         end


       | codegen(infixexp("-",t1,t2),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         let val _ = codegen(t1,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)
             val _ = codegen(t2,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)              
         in
           TextIO.output(outFile,indent^"BINARY_SUBTRACT\n")
         end

       | codegen(infixexp("+",t1,t2),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         let val _ = codegen(t1,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)
             val _ = codegen(t2,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)              
         in
           TextIO.output(outFile,indent^"BINARY_ADD\n")
         end

       | codegen(infixexp("*",t1,t2),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         let val _ = codegen(t1,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)
             val _ = codegen(t2,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)              
         in
           TextIO.output(outFile,indent^"BINARY_MULTIPLY\n")
         end

       | codegen(infixexp("div",t1,t2),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         let val _ = codegen(t1,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)
             val _ = codegen(t2,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)              
         in
           TextIO.output(outFile,indent^"BINARY_FLOOR_DIVIDE\n")
         end

       | codegen(infixexp("<",t1,t2),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         let val _ = codegen(t1,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)
             val _ = codegen(t2,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)   
             val index = lookupIndex("<",cmp_op)           
         in
           TextIO.output(outFile,indent^"COMPARE_OP "^index^"\n")
         end

       | codegen(infixexp(">",t1,t2),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         let val _ = codegen(t1,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)
             val _ = codegen(t2,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)   
             val index = lookupIndex(">",cmp_op)           
         in
           TextIO.output(outFile,indent^"COMPARE_OP "^index^"\n")
         end

       | codegen(raisexp(t),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         let val _ = codegen(t,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)      
         in
           TextIO.output(outFile,indent^"RAISE_VARARGS 1\n")
         end

       | codegen(handlexp(t1,L),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         let val L0 = nextLabel()
             val L1 = nextLabel()
             val L2 = nextLabel()
             val excIndex = lookupIndex("Exception",globals)
             val excmpIdx = lookupIndex("excmatch",cmp_op)
         in
           TextIO.output(outFile,indent^"SETUP_EXCEPT "^L0^"\n");
           codegen(t1,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope);         
           TextIO.output(outFile,indent^"POP_BLOCK\n");
           TextIO.output(outFile,indent^"JUMP_FORWARD "^L2^"\n");
           TextIO.output(outFile,L0^":\n");
           TextIO.output(outFile,indent^"DUP_TOP\n");
           TextIO.output(outFile,indent^"LOAD_GLOBAL "^excIndex^"\n");
           TextIO.output(outFile,indent^"COMPARE_OP "^excmpIdx^"\n");
           TextIO.output(outFile,indent^"POP_JUMP_IF_FALSE "^L1^"\n");
           (* At this point the stack has Exception, argument to Exception, and traceback on the stack.
              We will throw away the argument and the traceback *)
           TextIO.output(outFile,indent^"ROT_TWO\n");
           TextIO.output(outFile,indent^"POP_TOP\n");
           TextIO.output(outFile,indent^"ROT_TWO\n");
           TextIO.output(outFile,indent^"POP_TOP\n");

           List.map (fn (match(pat,exp)) => 
                      let val endpatternlab = nextLabel()
                      in
                        TextIO.output(outFile,indent^"DUP_TOP\n");
                        let val newbindings = patmatch(pat,outFile,indent,consts,locals,freeVars,cellVars,globals,env,scope+1,endpatternlab)
                        in
                          TextIO.output(outFile,indent^"POP_TOP\n");
                          codegen(exp,outFile,indent,consts,locals,freeVars,cellVars,globals,newbindings@env,globalBindings,scope+1);
                          TextIO.output(outFile,indent^"JUMP_FORWARD "^L2^"\n");
                          TextIO.output(outFile,endpatternlab^":\n")
                        end
                      end) L;
           (* Exception pattern was not matched so reraise it *)
           TextIO.output(outFile,L1^":\n");
           TextIO.output(outFile,indent^"RAISE_VARARGS 1\n");
           (* Otherwise, we continue on from here *)
           TextIO.output(outFile,L2^":\n")
         end

       | codegen(letdec(d,L2),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         let val newbindings = decgen(d,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)
         in
           codegenseq(L2,outFile,indent,consts,locals,freeVars,cellVars,globals,newbindings@env,globalBindings,scope+1)
         end

       | codegen(expsequence(L),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
           codegenseq(L,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)

       | codegen(tuplecon(L),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         let val size = List.length(L)
         in
           codegenlist(L,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope);
           TextIO.output(outFile,indent^"BUILD_TUPLE "^Int.toString(size)^"\n")
         end

         (* when a func is encountered, it is for an anonymous function. The function is made here
            and then called in the corresponding apply that encloses this func *)
       | codegen(func(idnum,L),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         let val name = "anon@"^(Int.toString(idnum))
         in
           makeFunction(name,L,outFile,indent,consts,locals,freeVars,cellVars,env,globalBindings,scope)
         end
         
       | codegen(other,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) =
         (TextIO.output(TextIO.stdOut, "\nAttempt to compile expression not currently supported!\n");
          TextIO.output(TextIO.stdOut, "Expression was: " ^ nameOf(other) ^ "\n");
          raise Unimplemented) 

     and codegenlist(nil,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = ()
       | codegenlist(h::t,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
             (codegen(h,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope);
              codegenlist(t,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope))

     and codegenseq(nil,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = ()
       | codegenseq([x],outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
             codegen(x,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope)
       | codegenseq(h::t,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
             (codegen(h,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope);
              TextIO.output(outFile,indent^"POP_TOP\n");
              codegenseq(t,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope))

     (* The dec gen function is called to generate the code necessary for a declaration of a binding
        in the code generator *)

         (* This first decgen is an optimization for patterns like x = val. The idpat(name) pat 
            will always match so no exception handling or pattern checking is necessary *)
     and decgen(bindval(idpat(name),exp),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         let val variable = name^"@"^Int.toString(scope) 
         in
           codegen(exp,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope);
           store(name,outFile,indent,locals,freeVars,cellVars,globals,(name,variable)::env);
           [(name,variable)]
         end
 
       | decgen(bindval(pat,exp),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         let val exceptionidx = lookupIndex("Exception",globals)
             val mismatch = lookupIndex("'Match Not Found'",consts)
             val lab = nextLabel()
             val next = nextLabel()
         in
           codegen(exp,outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope);
           let val binding = patmatch(pat,outFile,indent,consts,locals,freeVars,cellVars,globals,env,scope,lab)
           in
             TextIO.output(outFile,indent^"JUMP_FORWARD "^next^"\n");
             TextIO.output(outFile,lab^":\n");       
             TextIO.output(outFile,indent^"LOAD_GLOBAL "^exceptionidx^"\n");
             TextIO.output(outFile,indent^"LOAD_CONST "^mismatch^"\n");
             TextIO.output(outFile,indent^"CALL_FUNCTION 1\n");
             TextIO.output(outFile,indent^"RAISE_VARARGS 1\n");
             TextIO.output(outFile,next^":\n");
             binding
           end
         end

       | decgen(bindvalrec(idpat(name),exp),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         let val targetName = name
         in
           [(name,targetName)]
         end

       | decgen(bindvalrec(pat,exp),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         (TextIO.output(TextIO.stdOut,"Use of val rec without an identifier pattern not allowed.\n");
          raise Unimplemented)

       | decgen(funmatch(name,nil),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = 
         (TextIO.output(TextIO.stdOut,"Empty Function Definition??? This shouldn't happen\n");
          raise nameMismatch)

       | decgen(funmatch(name,L),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) = [(name,name)]

       | decgen(funmatches(L),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) =
         List.foldr (fn (x,y) => decgen(funmatch(x),outFile,indent,consts,locals,freeVars,cellVars,globals,env,globalBindings,scope) @ y) [] L

          
     (* The patmatch function is given a label to jump to if the pattern does not match. For any 
        pattern, the generated code checks to see if the value on the stop of the stack will 
        match the pattern. If it does, the variables for the pattern bindings are initialized with the 
        appropriate values for the pattern. 

        Precondition: Value to be pattern matched is on the top of the stack.
                      The "label" is the label to jump to if the pattern does not match.

        Postcondition: The value to be matched should no longer be on the stack. 
                       A list of all the bindings of identifiers in the pattern to
                       their storage locations should be return. All values in the 
                       pattern should have been stored in their respective storage 
                       locations for the pattern bindings. See the patmatch for idpat for this.  *)
  
     and patmatch(idpat("nil"),outFile,indent,consts,locals,freeVars,cellVars,globals,env,scope,label) =
         let val lenIndex = lookupIndex("len",globals)
             val equalIndex = lookupIndex("=",cmp_op)
             val zeroIndex = lookupIndex("0",consts)
         in
           TextIO.output(outFile,indent^"LOAD_GLOBAL "^lenIndex^"\n");
           TextIO.output(outFile,indent^"ROT_TWO\n");
           TextIO.output(outFile,indent^"CALL_FUNCTION 1\n");
           TextIO.output(outFile,indent^"LOAD_CONST "^zeroIndex^"\n");
           TextIO.output(outFile,indent^"COMPARE_OP "^equalIndex^"\n");
           TextIO.output(outFile,indent^"POP_JUMP_IF_FALSE "^label^"\n");
           []
         end

       | patmatch(intpat(v), outFile,indent,consts,locals,freeVars,cellVars,globals,env,scope,label) = 
         let val numIndex = lookupIndex(v,consts)
             val equalIndex = lookupIndex("=",cmp_op)
         in
           TextIO.output(outFile,indent^"LOAD_CONST "^numIndex^"\n");
           TextIO.output(outFile,indent^"COMPARE_OP "^equalIndex^"\n");
           TextIO.output(outFile,indent^"POP_JUMP_IF_FALSE "^label^"\n");
           []
         end
           
       | patmatch(boolpat(v), outFile,indent,consts,locals,freeVars,cellVars,globals,env,scope,label) = 
         let val value = if v = "true" then "True" else "False"
             val boolIndex = lookupIndex(value,consts)
             val equalIndex = lookupIndex("=",cmp_op)
         in
           TextIO.output(outFile,indent^"LOAD_CONST "^boolIndex^"\n");
           TextIO.output(outFile,indent^"COMPARE_OP "^equalIndex^"\n");
           TextIO.output(outFile,indent^"POP_JUMP_IF_FALSE "^label^"\n");
           []
         end
           
       | patmatch(strpat(v), outFile,indent,consts,locals,freeVars,cellVars,globals,env,scope,label) = 
         let val strIndex = lookupIndex(v,consts)
             val equalIndex = lookupIndex("=",cmp_op)
         in
           TextIO.output(outFile,indent^"LOAD_CONST "^strIndex^"\n");
           TextIO.output(outFile,indent^"COMPARE_OP "^equalIndex^"\n");
           TextIO.output(outFile,indent^"POP_JUMP_IF_FALSE "^label^"\n");
           []
         end
                  
       | patmatch(idpat(name),outFile,indent,consts,locals,freeVars,cellVars,globals,env,scope,label) = 
         let val variable = name^"@"^Int.toString(scope) 
         in
           store(name,outFile,indent,locals,freeVars,cellVars,globals,(name,variable)::env);
           [(name,variable)]
         end

       | patmatch(infixpat("::",pat1,pat2),outFile,indent,consts,locals,freeVars,cellVars,globals,env,scope,label) =
         let val zeroIndex = lookupIndex("0",consts)
             val lenIndex = lookupIndex("len",globals)
             val gtIndex = lookupIndex(">",cmp_op)
         in
           TextIO.output(outFile,indent^"DUP_TOP\n");
           TextIO.output(outFile,indent^"LOAD_GLOBAL "^lenIndex^"\n");
           TextIO.output(outFile,indent^"ROT_TWO\n");
           TextIO.output(outFile,indent^"CALL_FUNCTION 1\n");
           TextIO.output(outFile,indent^"LOAD_CONST "^zeroIndex^"\n");
           TextIO.output(outFile,indent^"COMPARE_OP "^gtIndex^"\n");
           TextIO.output(outFile,indent^"POP_JUMP_IF_FALSE " ^ label ^ "\n");
           TextIO.output(outFile,indent^"SELECT_FUNLIST\n");
           let val head = patmatch(pat1,outFile,indent,consts,locals,freeVars,cellVars,globals,env,scope,label)
               val tail = patmatch(pat2,outFile,indent,consts,locals,freeVars,cellVars,globals,env,scope,label)
           in
             head @ tail
           end
         end

       | patmatch(tuplepat(L),outFile,indent,consts,locals,freeVars,cellVars,globals,env,scope,label) =
         (TextIO.output(outFile,indent^"SELECT_TUPLE "^Int.toString(length(L))^"\n");
          List.foldl (fn (x,y) => patmatch(x,outFile,indent,consts,locals,freeVars,cellVars,globals,env,scope,label) @ y) [] L)

       | patmatch(_,outFile,indent,consts,locals,freeVars,cellVars,globals,env,scope,label) =
         (TextIO.output(TextIO.stdOut, "\nAttempt to compile unsupported pattern match!\n");
          raise Unimplemented)   

     (* The nestedfuns function is necessary because the CoCo assembly language insists that nested functions (whether anonymous
        or named) be defined before the body of the code that uses them. This code traverses the abstract syntax tree of an
        expression and looks for nested functions. When one is found, nestedfun is called on the nested function to
        generate its code. This function is called right after the first line (the line containing the FUNCTION keyword) is 
        written for the enclosing function's definition. *)

     and nestedfun(name,expList,outFile,indent,globals,env,globalBindings,scope) = 
         let val consts = (removeDups ("None"::"'Match Not Found'"::"0"::(List.foldr (fn (match(pat,exp),y) => (patConsts pat)@(constants exp)@y) [] expList)))
             val patBs = concat (List.map (fn (match(pat,exp)) => patBindings(pat,scope+1)) expList)
             val bindingsandvars = List.map (fn (match(pat,exp)) =>
                                        let val patB = patBindings(pat,scope+1)
                                        in
                                          localBindings(exp,patB,globalBindings,scope+1)
                                        end) expList   
             val newbindings = (concat (List.map (fn x => (#1(x))) bindingsandvars)) @ patBs 
             val bindingVars = name^"@Param"::(removeDups (List.map (fn x => #2(x)) newbindings))
             val freeVars = List.map (fn x => boundTo(x,(name,name)::(newbindings@env@globalBindings))) (removeDups (concat (List.map (fn x => (#2(x))) bindingsandvars)))
             val cellVars = (removeDups (concat (List.map (fn x => (#3(x))) bindingsandvars)))
             val locals = listdiff (listdiff bindingVars freeVars) cellVars
             val exceptionidx = lookupIndex("Exception",globals)
             val mismatch = lookupIndex("'Match Not Found'",consts)   

      
         in
            TextIO.output(outFile,indent^"Function: "^name^"/1\n");
            List.map (fn (match(pat,ast)) => nestedfuns(ast,outFile,indent^"    ",globals,patBindings(pat,scope+1)@[(name,name)]@env,globalBindings,scope+1)) expList;
            TextIO.output(outFile,indent^"Constants: "^(commaSepList consts) ^ "\n");
            if not (List.null(locals)) then
              TextIO.output(outFile,indent^"Locals: "^(commaSepList locals) ^ "\n")
            else ();
            if not (List.null(freeVars)) then
              TextIO.output(outFile,indent^"FreeVars: "^(commaSepList freeVars) ^ "\n")
            else ();
            if not (List.null(cellVars)) then
              TextIO.output(outFile,indent^"CellVars: "^(commaSepList cellVars) ^ "\n")
            else ();
            TextIO.output(outFile,indent^"Globals: "^(commaSepList globals) ^ "\n");
            TextIO.output(outFile,indent^"BEGIN\n");
            List.map (fn (match(pat,exp)) => 
                  makeFunctions(exp,outFile,indent^"    ",consts,locals,freeVars,cellVars,patBindings(pat,scope+1)@[(name,name)]@env,globalBindings,scope+1)) expList;

            List.map (fn (match(pat,exp)) => 
                      let val endpatternlabel = nextLabel()
                          val patB = patBindings(pat,scope+1)
                      in
                        TextIO.output(outFile,indent^"    LOAD_FAST 0\n");(*hard-coded to 0 because parameter is always at 0*)
                        patmatch(pat,outFile,indent^"    ",consts,locals,freeVars,cellVars,globals,patB@[(name,name)]@env@globalBindings,scope+1,endpatternlabel);
                        codegen(exp,outFile,indent^"    ",consts,locals,freeVars,cellVars,globals,patB@[(name,name)]@env@globalBindings,globalBindings,scope+1);
                        TextIO.output(outFile,indent^"    RETURN_VALUE\n");
                        TextIO.output(outFile,endpatternlabel^":\n")
                      end) expList;

            TextIO.output(outFile,indent^"    LOAD_GLOBAL "^exceptionidx^"\n");
            TextIO.output(outFile,indent^"    LOAD_CONST "^mismatch^"\n");
            TextIO.output(outFile,indent^"    CALL_FUNCTION 1\n");
            TextIO.output(outFile,indent^"    RAISE_VARARGS 1\n");
            TextIO.output(outFile,indent^"END\n")
         end

     and nestedfuns(ast,outFile,indent,globals,env,globalBindings,scope) =
	       let fun functions(int(n)) = ()
               | functions(boolval(n)) = ()
               | functions(ch(c)) = ()
               | functions(str(s)) = ()
               | functions(id(name)) = ()
               | functions(listcon(L)) = (List.map (fn x => (functions x)) L; ())
               | functions(tuplecon(L)) = (List.map (fn x => (functions x)) L; ())
               | functions(apply(exp1,exp2)) = (functions exp1;functions exp2)
               | functions(infixexp(operator,exp1,exp2)) = (functions exp1;functions exp2)
               | functions(handlexp(exp,L)) = (functions exp; List.map (fn (match(pat,exp)) => functions exp) L; ())
               | functions(raisexp(e)) = (functions e)

               | functions(expsequence(L)) = (List.map (fn x => (functions x)) L; ())
               | functions(letdec(d,L2)) = 
                 let val newbindings = #1(localBindings(letdec(d,[]),env,globalBindings,scope))
                 in
                   dec d; 
                   List.map (fn x => (nestedfuns(x,outFile,indent,globals,newbindings@env,globalBindings,scope+1))) L2; 
                   ()
                 end
               | functions(func(idnum,L)) = 
                 let val name = "anon@"^Int.toString(idnum)
                 in
                   nestedfun(name,L,outFile,indent,globals,env,globalBindings,scope) 
                 end
               | functions(other) =
                  (TextIO.output(TextIO.stdOut, "\nAttempt to call nestedfuns functions function for expression not currently supported!\n");
                   TextIO.output(TextIO.stdOut, "Expression was: " ^ nameOf(other) ^ "\n");
                   raise Unimplemented)   
 
             and dec(bindval(pat,exp)) = functions exp
               | dec(bindvalrec(idpat(name),func(idnum,L))) = 
                 let val targetName = name
                 in
                   nestedfun(targetName,L,outFile,indent,globals,(name,targetName)::env,globalBindings,scope) 
                 end

               | dec(bindvalrec(_,_)) =                  
                 (TextIO.output(TextIO.stdOut,"val rec construct must be val rec id = (fn <x> => ...)\n");
                  raise Unimplemented)

               | dec(funmatch(name,nil)) = ()

               | dec(funmatch(name, matchList)) = nestedfun(name,matchList,outFile,indent,globals,env,globalBindings,scope)
 
               | dec(funmatches(L)) =
                 let val nameList = List.map (fn (name,matchlist) => name) L
                 in 
                   List.map (fn (name,matchList) => 
                       let val adjustedBindings = List.map (fn x => (x,x)) (listdiff nameList [name])
                       in
                         nestedfun(name,matchList,outFile,indent,globals,adjustedBindings@env,globalBindings,scope)
                       end) L;
                   ()                    
                 end
         in
           functions ast
         end

     (* When a nested named function is defined there must be code generated at the beginning of the enclosing 
        function's body to bind the code of the function to its name. If the nested function contains free variables, 
        then the code and the free variable cells must be used for form a closure and the closure is bound to the 
        function's name. This function is called right after the BEGIN is written for the enclosing function's instructions. 

        For anonymous functions, the name of the anonymous function is anon@idnum where idnum is a unique integer assigned 
        during parsing of the expression. When the function is anonymous, the function is made in the place where it is
        defined (see the call to makeFunction in the codegen for func above.) *)

     and makeFunction(name,expList,outFile,indent,consts,locals,freeVars,cellVars,env,globalBindings,scope) = 
         let val bindingsandvars = List.map (fn match(pat,exp) =>
                                        let val patB = patBindings(pat,scope+1)
                                        in
                                          localBindings(exp,patB,globalBindings,scope+1)
                                        end) expList 
             val nestedfreeVars = removeDups (concat (List.map (fn x => (#2(x))) bindingsandvars))
         in
           if length(nestedfreeVars) = 0 then
             let val constIndex = lookupIndex("code("^name^")",consts)
             in
               TextIO.output(outFile,indent^"LOAD_CONST "^constIndex^"\n");
               TextIO.output(outFile,indent^"MAKE_FUNCTION 0\n")
             end
           else 
             let val constIndex = lookupIndex("code("^name^")",consts)
             in
               List.map (fn x => 
                         let val realName = boundTo(x,[(name,name)]@env)
                             val cellIndex = lookupIndex(realName,cellVars@freeVars)
                         in
                           TextIO.output(outFile,indent^"LOAD_CLOSURE "^cellIndex^"\n")
                         end) nestedfreeVars;
               TextIO.output(outFile,indent^"BUILD_TUPLE "^Int.toString(length(nestedfreeVars))^"\n");
               TextIO.output(outFile,indent^"LOAD_CONST "^constIndex^"\n");
               TextIO.output(outFile,indent^"MAKE_CLOSURE 0\n")
             end
        end

     and makeFunctions(ast,outFile,indent,consts,locals,freeVars,cellVars,env,globalBindings,scope) =
         let fun functions(int(n)) = ()
               | functions(boolval(n)) = ()
               | functions(ch(c)) = ()
               | functions(str(s)) = ()
               | functions(id(name)) = ()
               | functions(listcon(L)) = (List.map (fn x => (functions x)) L; ())
               | functions(tuplecon(L)) = (List.map (fn x => (functions x)) L; ())
               | functions(apply(exp1,exp2)) = (functions exp1;functions exp2)
               | functions(infixexp(operator,exp1,exp2)) = (functions exp1;functions exp2)
               | functions(handlexp(exp,L)) = (functions exp;List.map (fn (match(pat,exp)) => functions exp) L; ())
               | functions(raisexp(e)) = (functions e)


               | functions(expsequence(L)) = (List.map (fn x => (functions x)) L; ())

               | functions(letdec(d,L)) = 
                 let val lbindings = localBindings(letdec(d,[]),env,globalBindings,scope)
                     val newbindings = #1lbindings
                     val newfreevars = removeDups (#2lbindings @ freeVars)
                     val newcellvars = removeDups (#3lbindings @ cellVars) 
                 in
                   dec d; 
                   List.map (fn x => (makeFunctions(x,outFile,indent,consts,locals,newfreevars,newcellvars,newbindings@env,globalBindings,scope+1))) L; 
                   ()
                 end
               | functions(func(idnum,L)) = ()
               | functions(other) = 
                  (TextIO.output(TextIO.stdOut, "\nAttempt to call makeFunctions functions function for expression not currently supported!\n");
                   TextIO.output(TextIO.stdOut, "Expression was: " ^ nameOf(other) ^ "\n");
                   raise Unimplemented)   
        
             and dec(bindval(pat,exp)) = functions exp
               | dec(bindvalrec(idpat(name),func(idnum,L))) = 
                 let val targetName = name
                 in
                   makeFunction(targetName,L,outFile,indent,consts,locals,freeVars,cellVars,(name,targetName)::env,globalBindings,scope);
                    (* The function or closure will be left on the top of the stack by makeFunction. Then it is stored 
                       in the location bound to the function's identifier. *)
                   store(name,outFile,indent,locals,freeVars,cellVars,[],[(name,targetName)]@env@globalBindings)
                 end

               | dec(bindvalrec(_,_)) = 
                 (TextIO.output(TextIO.stdOut,"val rec construct must be val rec id = (fn <x> => ...)\n");
                  raise Unimplemented)

               | dec(funmatch(name,nil)) = ()
               | dec(funmatch(name,matchList)) = 
                   (makeFunction(name,matchList,outFile,indent,consts,locals,freeVars,cellVars,env,globalBindings,scope);
                    (* The function or closure will be left on the top of the stack by makeFunction. Then it is stored 
                       in the location bound to the function's identifier. *)
                    store(name,outFile,indent,locals,freeVars,cellVars,[],[(name,name)]@env@globalBindings))

               | dec(funmatches(L)) =
                 let val nameList = List.map (fn (name,matchlist) => name) L
                 in 
                   List.map (fn (name,matchList) => 
                       let val adjustedBindings = List.map (fn x => (x,x)) (listdiff nameList [name])
                       in
                         makeFunction(name,matchList,outFile,indent,consts,locals,freeVars,cellVars,adjustedBindings@env,globalBindings,scope);
                         (* The function or closure will be left on the top of the stack by makeFunction. Then it is stored 
                            in the location bound to the function's identifier. *)
                         store(name,outFile,indent,locals,freeVars,cellVars,[],[(name,name)]@adjustedBindings@env@globalBindings)
                       end) L;
                   ()                    
                 end
         in
           functions ast
         end
     
     (* Here is where it all begins. The file is parsed to produce an abstract syntax tree. Then the output file a.casm is opened for writing. Constants
        and globals are defined. The ast is examined to find the cell variables and the locals. No free variables should exists in the main expression as 
        that would mean that we could not compile the code completely. The FUNCTION keyword is writen to the file. Then nestedfunctions is called to 
        generate the nested function definitions. The prolog stuff (up to the BEGIN keyword) is generated. Then makeFunctions is called make the nested functions
        or closures and bind those functions or closures to their names. A closure is needed for any function that contains free variables. If the nested function
        contains free variables a closure is made of the code for the function and references to all free variables in the enclosing scope. Finally, codegen
        is called on the main expression followed by a little code to clean up at the end. None is returned from the main function in this implementation, just
        like Python programs. *)
         
     fun compile filename  = 
         let val (ast, _) = parse filename
             val outFile = TextIO.openOut("a.casm")
             val termFile = TextIO.openOut("a.term")
             val _ = writeTerm(termFile,ast)
             val _ = TextIO.closeOut(termFile)
             val consts = removeDups ("None"::"'Match Not Found'"::"0"::(constants ast))
             (* The cprint function is typechecked as a function that returns itself (so it may be applied repeatedly in one expression.
                The print function, while bound to the same target function, is typechecked as returning a unit so it may not be 
                applied repeatedly in one expression. *)
             val globalBindings = [("println","print"),("print","fprint"),("cprint", "fprint"),("input","input"),("Int.fromString","int"),
                                   ("length","len"),("type","type"),("Exception","Exception"),("explode","funlist"),("implode","concat")]
             val (newbindings,freeVars,cellVars) = localBindings(ast,[],globalBindings,0)
             val bindingVars = removeDups (List.map (fn x => #2(x)) newbindings)
             val locals = listdiff bindingVars cellVars
             val globals = removeDups (List.map (fn (x,y) => y) globalBindings)

         in
           if length(freeVars) <> 0 then
              (TextIO.output(TextIO.stdOut,"Error: Unbound variable(s) found in main expression => " ^ (commaSepList freeVars) ^ ".\n");
               raise notFound)
           else ();
           TextIO.output(outFile,"Function: main/0\n");
           nestedfuns(ast,outFile,"    ",globals,[],globalBindings,0);
           TextIO.output(outFile,"Constants: "^(commaSepList consts) ^ "\n");
           if not (List.null(locals)) then
             TextIO.output(outFile,"Locals: "^(commaSepList locals) ^ "\n")
           else
             ();
           if not (List.null(cellVars)) then
             TextIO.output(outFile,"CellVars: "^(commaSepList cellVars) ^ "\n")
           else
             ();
           TextIO.output(outFile,"Globals: "^(commaSepList globals) ^ "\n");
           TextIO.output(outFile,"BEGIN\n");
           makeFunctions(ast,outFile,"    ",consts,locals,[],cellVars,[],globalBindings,0);

           codegen(ast,outFile,"    ",consts,locals,freeVars,cellVars,globals,globalBindings,globalBindings,0);
       
           TextIO.output(outFile,"    POP_TOP\n");       
           TextIO.output(outFile,"    LOAD_CONST 0\n");        
           TextIO.output(outFile,"    RETURN_VALUE\n");        
           TextIO.output(outFile,"END\n");        
           TextIO.closeOut(outFile)
         end 
         handle _ => (TextIO.output(TextIO.stdOut, "An error occurred while compiling!\n\n"));
             
       
     fun run(a,b::c) = (compile b; OS.Process.success)
       | run(a,b) = (TextIO.print("usage: sml @SMLload=mlcomp\n");
                     OS.Process.success)
end


