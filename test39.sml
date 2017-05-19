
let fun zip [] [] = []
      | zip [] L = raise (Exception "List length mismatch")
      | zip L [] = raise (Exception "List length mismatch") 
      | zip (h::t) (g::u) = (h,g)::(zip t u)
 
    val x = [1,2,3]
    val y = [4,5,6]
in
  println(zip x y)
end
  
