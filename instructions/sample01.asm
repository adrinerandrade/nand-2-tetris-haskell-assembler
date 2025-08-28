// Program: mult.asm
// Multiplies R2 = R0 * R1
	
	@R2
	M=0 // R2 = 0
	
	@i
	M=0 // i = 0
	
(LOOP)
	@i
	D=M
	D=D+1
	M=D    // i = i + 1

	@R1
	D=D-M  // i - R1
	
	@END
	D;JGT // If i > R1
	
	@R2
	D=M
	@R0
	D=D+M // R2 = R2 + R0
	@R2
	M=D
	
	@LOOP
	0;JMP
	
(END)
	@END
	0;JMP