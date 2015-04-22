let fun fact(0) = 1
         | fact(n) = n * (fact(n-1))
in 
    println(fact(5))
end
