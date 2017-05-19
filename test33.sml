let val x = 6 
     fun f() = x
in
  let val x = 5
  in
    println(f())
  end
end