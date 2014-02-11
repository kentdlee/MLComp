let val rec f = (fn 0 => 1
                  | x => x * (f (x-1)))
in
   println(f 5)
end