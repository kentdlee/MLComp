let fun f(0,y) = y
      | f(x,y) = g(x,x*y)
    and g(x,y) = f(x-1,y)
in
  println (f(10,5)) 
end
