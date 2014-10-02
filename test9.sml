(* This code checks to see that a non-ref binding cannot
   be used incorrectly as a variable binding. This code will
   run on the CoCo interpreter because free variables are 
   implemented as cellvars and that is exactly how a ref
   binding is implemented, too. However, this program
   does not pass the typechecker. *)

let val x = 0
    fun f y = (x:=!x+1)
in
  f 0;
  println x 
end
  
