(* 
	This program type checks correctly as it stands, but it should
	not. This is an example of a program that demonstrates that the
	implementation of type checking for MLComp is not sound. 

	This program and why it should not type check correctly is 
	discussed at

	http://www.smlnj.org/doc/Conversion/types.html

	where it explains why this should not pass the type checker
	and how that is handled correctly in SML/NJ. 
*)

let val r = ref(fn x => x)
in
    r := (fn x => x+1);
    !r true
end
