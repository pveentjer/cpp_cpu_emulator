; variables
    VAR A 10
    VAR B 100
    VAR C 100
    VAR D 0

; program
    LOAD 0 A
again:
    PRINTR 0
    DEC 0
    JNZ 0 again
    LOAD 0 B
    LOAD 1 C
    ADD 2 0 1
    STORE 2 D
    HALT