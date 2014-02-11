fun input s = 
    (TextIO.output(TextIO.stdOut,s^"\n");
     valOf(TextIO.inputLine(TextIO.stdIn)));

fun print s = TextIO.output(TextIO.stdOut,"["^(List.foldr op ^ "" (List.map (fn x => (Int.toString x)^" ") s))^"]\n");

let 
    val x = input("Please enter a list of integers: ")
    val lst = String.tokens (fn c => c = #" ") x
    val lstInts = List.map (fn s => valOf(Int.fromString s)) lst
    val fVal = ref 0

    fun g aVal = 
    let fun f () =
        let val count = ref 0
        in
            List.map (fn x => 
                        let val value = x + aVal * !count + !fVal
                        in
                            count := !count + 1;
                            value
                        end) lstInts
        end
    in
      fVal := valOf(Int.fromString(input("Please enter another integer: ")));
      f
    end

    val myFun = g(6)
in
    print(myFun())
end

