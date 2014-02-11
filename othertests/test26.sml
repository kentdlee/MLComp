let val zero = 0 
    fun fib n = 
    let val i = ref zero
        val current = ref 0
        val next = ref 1
        val tmp = ref 0
    in
      while !i < n do (
        tmp := !next + !current;
        current := !next;
        next := !tmp;
        i := !i + 1
      );

      !current
    end
    val x = Int.fromString(input("Please enter an integer: "))
    val r = fib(x)
in
  print "Fib(";
  print x;
  print ") is ";
  println r 
end

