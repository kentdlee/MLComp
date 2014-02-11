(* This is an invalid program and should not 
   pass the type checker. However, it will 
   compile and run even with the type error
   on the CoCo Virtual Machine. In general,
   the result is undefined. 
*)

let val x = ref 0
in
  x := x + 1;
  println (x)
end
