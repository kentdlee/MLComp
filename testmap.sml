let fun map f [] = []
      | map f (h::t) = (f h) :: (map f t)

in 
  println(map (fn x => x + 1) [1,2,3,4])
end