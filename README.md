# __EIGHT QUEENS PROBLEM__

The eight queens problem is a classic coding problem. The goal is to place
eight chess queens on an 8×8 chessboard and no two queens threaten each
other. The size of board may be adjusted.

In this assignment, we write a RISC-V program to find a solution on an 8x8
board after the location of the queen in the first row (row 0) is specified.
One implementation in C is provided in "eightqueens.c". In this version, the
program stops searching more solutions when the first one is found. The
location of the first queen defaults to column 0, but it can be specified at
the command line. For example, the following command place the first queen in
row 0, column 1.

    ./eightqueens 1

The skeleton code is in "eightqueens.s". Here are some differences in the
assembly version and some constraints.

*   Assume only two branch instructions, BEQ and BNE, are available. Do not use
    BLT, BGE, BLTU, or BGEU. Otherwise, you will have to rewrite part of your
    code later.

    One challenge is to implement `if (i < j)` without using the BLT
    instruction. Take a moment to think it through. There is a hint at the
    bottom of the page. 

*   The location of the queen in row 0 is specified by the first number in array
    `a`. We can change the number manually. Note that array `a` is on the
    stack.

`my_puts` function is provided in the skeleton code.

Pay attention to `line` in the C code. It is a local word array. 


## Testing

The solutions found by the RISC-V program should be the same as the ones found
by the C program. Here are some solutions from the C program. Compile the C
program and compare the results for other initial setups.

```
# ./eightqueens
*-------
----*---
-------*
-----*--
--*-----
------*-
-*------
---*----
04752613

# ./eightqueens 1
-*------
---*----
-----*--
-------*
--*-----
*-------
------*-
----*---
13572064
```

## Tips

To check if `i < j`, we can do `i - j` first and then check if the result is
negative. 

To check if the result is negative, we just need to check if its sign bit is 1.

We can check the sign bit after shifting it to the right end in a register.

