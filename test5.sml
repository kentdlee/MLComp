(* there is a type error in this program that is not caught at run-time *)
let val x = true
    val y = false
in
  println (x orelse y div 0);
  println (y andalso x * 5)
end

