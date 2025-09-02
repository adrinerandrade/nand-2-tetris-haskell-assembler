// Program Signum.asm
// Computes: if R0>0
//				R1=1
//			 else
//			    R1=0
				
	@R0
	D=M     // D = RAM[0]
	
	@POSITIVE
	D;JGT   // If R0>0 goto 8
	
	@R1     // RAM[1]=0
	M=0
	@END
	0;JMP   // end of the program
	
  (POSITIVE)
	@R1
	M=1      // R1=1
	
   (END)
    @END
	0;JMP