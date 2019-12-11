// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.

@SCREEN
D=A
@startscreen  // 16384 (screen's base address)
M=D
@addr
M=D     

@24575  // end screen address
D=A
@endscreen
M=D

(KBDLOOP)
  @KBD
  D=M
  @WHITELOOP
  D;JEQ
  @BLACKLOOP
  D;JNE

// draw white from end to start
(WHITELOOP)
  @startscreen
  D=M
  @addr
  D=D-M
  @KBDLOOP
  D;JGT     // if reached first screen address goto END 

  @addr
  A=M
  M=0      // RAM[addr]=0000000000000000

  @addr
  M=M-1     
  @WHITELOOP
  0;JMP     // goto WHITELOOP

// draw black from start to end
(BLACKLOOP)
  @endscreen
  D=M
  @addr
  D=D-M
  @KBDLOOP
  D;JLT     // if reached last screen address goto END 

  @addr
  A=M
  M=-1      // RAM[addr]=1111111111111111

  @addr
  M=M+1     
  @BLACKLOOP
  0;JMP     // goto BLACKLOOP

(END)
  @END      // end with infinite looop
  0;JMP
