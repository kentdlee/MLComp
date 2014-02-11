(* In this program the explode is called funlist in the CoCo virtual machine
   while the implode is called concat in the virtual machine. Unlike SML, 
   explode returns a list of strings and implode concatenates a list of strings
   to build a string.
*)

let fun skipH nil = ()
      | skipH ("H"::t) = skipH t
      | skipH (h::t) = 
        (println h;
         skipH t)
    val s = input("Please enter some text: ")
in
  skipH (explode s);
  println (implode (explode s))
end
