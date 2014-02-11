Function: main/0
    Function: fib/1
    Constants: None, 0, 1
    Locals: n, i, current, next, tmp
    FreeVars: zero
    BEGIN
              LOAD_DEREF                     0
              STORE_FAST                     1
              LOAD_CONST                     1
              STORE_FAST                     2
              LOAD_CONST                     2
              STORE_FAST                     3
              SETUP_LOOP               label02
    label00:  LOAD_FAST                      1
              LOAD_FAST                      0
              COMPARE_OP                     0
              POP_JUMP_IF_FALSE        label01
              LOAD_FAST                      3
              LOAD_FAST                      2
              BINARY_ADD               
              STORE_FAST                     4
              LOAD_FAST                      3
              STORE_FAST                     2
              LOAD_FAST                      4
              STORE_FAST                     3
              LOAD_FAST                      1
              LOAD_CONST                     2
              BINARY_ADD               
              STORE_FAST                     1
              JUMP_ABSOLUTE            label00
    label01:  POP_BLOCK                
    label02:  LOAD_FAST                      2
              RETURN_VALUE             
    END
Constants: None, 0, code(fib), "Please enter an integer: ", "Fib(", ") is"
Locals: fib, x, r
CellVars: zero
Globals: int, input, print, str
BEGIN
          LOAD_CONST                     1
          STORE_DEREF                    0
          LOAD_CLOSURE                   0
          BUILD_TUPLE                    1
          LOAD_CONST                     2
          MAKE_CLOSURE                   0
          STORE_FAST                     0
          LOAD_GLOBAL                    0
          LOAD_GLOBAL                    1
          LOAD_CONST                     3
          CALL_FUNCTION                  1
          CALL_FUNCTION                  1
          STORE_FAST                     1
          LOAD_FAST                      0
          LOAD_FAST                      1
          CALL_FUNCTION                  1
          STORE_FAST                     2
          LOAD_GLOBAL                    2
          LOAD_CONST                     4
          LOAD_GLOBAL                    3
          LOAD_FAST                      1
          CALL_FUNCTION                  1
          BINARY_ADD               
          LOAD_CONST                     5
          BINARY_ADD               
          LOAD_FAST                      2
          CALL_FUNCTION                  2
          POP_TOP                  
          LOAD_CONST                     0
          RETURN_VALUE             
END
