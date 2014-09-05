let fun f 0 y = y
      | f x y = g x (x*y)
    and g x y = f (x-1) y
in
  (f 5 1)
end
