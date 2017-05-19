(* This program should fail in execution but not in type checking *)
let val [x1, x2, x3] = [1,2,3,4,5]
in
  println x2
end
