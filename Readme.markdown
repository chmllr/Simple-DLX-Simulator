# Supported syntax of assembler programs #

## Syntax ##

On this page we'll shortly describe the syntax you have to use in your assembler programs, in order to be able to execute them on SDS. We assume, that the reader is familiar with basics of the DLX instruction set.
Generally, every line of your assembler program should have the following form:

`[ addr : instr [ rd ] [ rs1 ] [ rs2 ] [ imm ] [ sa ] ] [ ; comment ]`

Older syntax was (can be turned on with the "old_syn" command):

`[ addr : instr [ rs1 ] [ rs2 ] [ rd ] [ imm ] [ sa ] ] [ ; comment ]`

Here, the elements in the square brackets "[ ]" denote optional elements. For example, a valid line of your assembler file can be an empty line, or it can contain just a comment, or an instruction with/without a comment. However, every instruction consists of its address (in memory), of an identifier and of arguments. The kind and number of arguments depend on the identifier. You only have to specify for each instruction the arguments it really uses, but you **have to respect** the order of the arguments!

For example, these lines are valid:

 * 16 : addi r1 r2 5
 * 24 : j 40
 * 44 : xor r1 r2 r1 ; a line with comment!

and these are not:

   * addi r0 r2 5
   * 24 : j r30 40
   * 16 : addi r0 5 r2

This is all we need to know to write an assembler program.

## Sample Program ##

This is a working example of a programm, which computes the product of numbers 37 and 8, and stores the result to the memory at the address 403:

    ; this program computes 37*8 and stores the result to the memory at the address 403
    0  : addi   r1  r0  37      ;   r1  =   37
    4  : addi   r2  r0  8       ;   r2  =   8
    8  : addi   r3  r0  0       ;   r3  =   0
    12 : beqz   r1      16      ;   if r1 == 0 then jump to line 28
    16 : add    r3  r2  r3      ;   r3  =   r3  +   r2
    20 : subi   r1  r1  1       ;   r1--
    24 : j              -12     ;   jump to line 12
    28 : sw     r3  r0  403     ;   M(403 + 0)  =   GPR(r3)
