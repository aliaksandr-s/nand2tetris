// set SP to 10 in ram [0] //
@10 
D=A
@0
M=D
// =======

@7 // D=7
D=A

@SP // *SP=D
A=M
M=D

@SP // SP++
M=M+1

@8 // D=8
D=A

@SP // *SP=D
A=M
M=D

@SP // SP++
M=M+1

// Get num at SP--
@SP
M=M-1
A=M
D=M

// sum with number at SP--
// @SP
// M=M-1
// A=M
// M=D+M


// compare with number at SP--
@SP
M=M-1
A=M
M=D&M
