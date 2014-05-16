let val x = Int.fromString(input("Please enter an integer: "))
in
	print "The value of 100 div ";
	print x;
	print " is ";
	println (100 div x)
end handle ex  => println ex
   | redundant => ()