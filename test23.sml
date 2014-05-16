let fun add3 x y 0 = x + y
      | add3 0 y z = y + z
      | add3 x 0 z = x + z
      | add3 x y z = x + y + z
in
  println (add3 1 2 3)
end