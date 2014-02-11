(* This test assumes that variable refs are already implemented.
 
   The new feature in this code is the while loop. You can run 
   test12.py to see the generated code for a while loop. Be sure
   to use python 3.2 when running the disassembler. 
*)

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

(* Here is the correctly generated code for this program. 

Function: main/0
    Function: fib/1
    Constants: None, 'Match Not Found', 0, 1
    Locals: fib@Param, n@2
    FreeVars: zero@0
    CellVars: tmp@5, next@4, current@3, i@2
    Globals: fprint, input, int, len, type, Exception
    BEGIN
        LOAD_FAST 0
        STORE_FAST 1
        LOAD_DEREF 4
        STORE_DEREF 3
        LOAD_CONST 2
        STORE_DEREF 2
        LOAD_CONST 3
        STORE_DEREF 1
        LOAD_CONST 2
        STORE_DEREF 0
        SETUP_LOOP L3
L1:
        LOAD_DEREF 3
        LOAD_FAST 1
        COMPARE_OP 0
        POP_JUMP_IF_FALSE L2
        LOAD_DEREF 1
        LOAD_DEREF 2
        BINARY_ADD
        DUP_TOP
        STORE_DEREF 0
        POP_TOP
        LOAD_DEREF 1
        DUP_TOP
        STORE_DEREF 2
        POP_TOP
        LOAD_DEREF 0
        DUP_TOP
        STORE_DEREF 1
        POP_TOP
        LOAD_DEREF 3
        LOAD_CONST 3
        BINARY_ADD
        DUP_TOP
        STORE_DEREF 3
        JUMP_ABSOLUTE L1
L2:
        POP_BLOCK
L3:
        POP_TOP
        LOAD_DEREF 2
        RETURN_VALUE
L0:
        LOAD_GLOBAL 5
        LOAD_CONST 1
        CALL_FUNCTION 1
        RAISE_VARARGS 1
    END
Constants: None, 'Match Not Found', 0, code(fib), "Please enter an integer: ", "Fib(", ") is ", "\n"
Locals: r@3, x@2, fib
CellVars: zero@0
Globals: fprint, input, int, len, type, Exception
BEGIN
    LOAD_CLOSURE 0
    BUILD_TUPLE 1
    LOAD_CONST 3
    MAKE_CLOSURE 0
    STORE_FAST 2
    LOAD_CONST 2
    STORE_DEREF 0
    LOAD_GLOBAL 2
    LOAD_GLOBAL 1
    LOAD_CONST 4
    CALL_FUNCTION 1
    CALL_FUNCTION 1
    STORE_FAST 1
    LOAD_FAST 2
    LOAD_FAST 1
    CALL_FUNCTION 1
    STORE_FAST 0
    LOAD_GLOBAL 0
    LOAD_CONST 5
    CALL_FUNCTION 1
    LOAD_FAST 1
    CALL_FUNCTION 1
    LOAD_CONST 6
    CALL_FUNCTION 1
    LOAD_FAST 0
    CALL_FUNCTION 1
    LOAD_CONST 7
    CALL_FUNCTION 1
    POP_TOP
    LOAD_CONST 0
    RETURN_VALUE
END

*)


