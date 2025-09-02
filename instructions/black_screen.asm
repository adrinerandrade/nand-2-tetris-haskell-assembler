// Program: black_screen.asm
// If a key in the keyboard is pressed, turn the screen completely black.
// The screen resolution is 512 pixels x 256 pixels.
// The screen is composed by 256 rows of 32 words (16 bits)

(LISTEN_KEYBOARD)
	@KBD
	D=M	// Captures Keyboard Input
	
	@LISTEN_KEYBOARD
	D;JEQ
	
	// Draws the screen black
	
	@256
	D=A
	@j           
	M=D // Set the row counter
	
	@SCREEN
	D=A
	@screen_index
	M=D // Set the begin of the screen memory map index to a variable
	
(DRAW_LINE)
	@j
	D=M
	@END
	D;JEQ // if j = 256 go to end
	
	@j
	D=M-1 // j = j - 1
	M=D
	
	@32
	D=A
	@i
	M=D // set the column counter
	
(DRAW_WORD)
	@i
	D=M
	@DRAW_LINE  
	D;JEQ // if i = 32 draw the next line
	
	@i
	D=M-1 // i = i - 1
	M=D
	
	@screen_index
	D=M
	A=D // recover screen index pointer
	M=-1 // paint the word black
	D=D+1 // set the screen index to the next address
	@screen_index
	M=D
	
	@DRAW_WORD // Draws the next word
	0;JMP

(END)
	@END
	0;JMP
