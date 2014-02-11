let fun f(true,x) = (println(x); g(x-1))
      | f(false,x) = g(x-1)

    and g 0 = ()
      | g x = f(true,x)

in
  g(10)
end