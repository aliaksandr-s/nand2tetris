// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

// Put your code here.

//       n = R0 */
//       i = R1 */
//       res = R2 */

//       LOOP: */
//           if i == 0 goto STOP */
//           res = res + n */
//           i = i - 1 */
//           goto LOOP */

//       STOP: */
//           R2 = res */

@R0
D=M
@n
M=D // n = R0

@R1
D=M
@i
M=D // i = R1

@res
M=0 // res = 0

(LOOP)
  @i
  D=M
  @STOP
  D;JEQ
  @n
  D=M
  @res
  M=D+M // res = res + n
  @i 
  M=M-1 
  @LOOP
  0;JMP

(STOP)
  @res
  D=M
  @R2
  M=D // RAM[2] = res

(END)
  @END
  0;JMP
