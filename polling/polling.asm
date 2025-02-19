;------------------------------------------------------------------------------------------------------------------
;This program read port 60h in cycle
;------------------------------------------------------------------------------------------------------------------
;                                        program

.model tiny   ;64 kilobytes in RAM, address == 16 bit == 2 bytes (because: sizeof (register) == 2 bytes)

;--------------------------------------------------------------------------------------------------------------
;										main program
;--------------------------------------------------------------------------------------------------------------

.code         ;begin program
org 100h      ;start == 256:   jmp start == jmp 256 != jmp 0 (because address [0;255] in program segment in DOS for PSP)
start:
	mov di, 0b800h     ;video segmentf  
	mov es, di         ;register es for segment address of video memory  (es != const    es == reg)

	mov di, 5*80*2 + 40*2    ;address of point for print in video_memory

	mov ah, color            ;color of symbol

	call check_next_symbol_from_keyboard

	mov ax, 4c00h      ;end of program with returned code = 0
	int 21h            ;call system
;--------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------------------
;											 check_next_symbol_from_keyboard
;check symbol from keyboard and print it or ret from func   (ASCII of print symbol == scan code from keyboard) 
;
;Entry: ah = color
;		di = address of point in video memory (in video segment)
;		es = address of video segment                                         
;
;Exit:  None
;
;Destr: al = symbol
;--------------------------------------------------------------------------------------------------------------

check_next_symbol_from_keyboard proc     

	next_symbol:
		in al, 60h     ;scan code from keyboard    -->   ASCII code of symbol in port 60h  == scan code from keyboard  -->    ASCII code of symbol in al

		mov es:[di], ax   ;print symbol from keyboard in video memory    (ASCII of print symbol == scan code from keyboard)

		cmp al, 01d
		jne next_symbol   ;if (al == ESC): goto next_symbol

	ret     
	endp    
;--------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------------------
;                                      variables
.data 
color db 01011011b
	    ;bBBBFFFF	b == blink;  B == back ground;  F == for ground       
	    ; rgbIRGB	r/R == red;  g/G == green;  b/B == blue;  I == increase

;--------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------------------
end start              ;end of asm and address of program's beginning

;list of colors: 
;01011011 = blue symbols on violet background
