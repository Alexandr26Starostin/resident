;------------------------------------------------------------------------------------------------------------------
;This program is the first version of resident and it changes 9th interrupt from old to new
;------------------------------------------------------------------------------------------------------------------
;                                        program

.model tiny   ;64 kilobytes in RAM, address == 16 bit == 2 bytes (because: sizeof (register) == 2 bytes)

;--------------------------------------------------------------------------------------------------------------
;										main program
;--------------------------------------------------------------------------------------------------------------

.code         ;begin program
org 100h      ;start == 256:   jmp start == jmp 256 != jmp 0 (because address [0;255] in program segment in DOS for PSP)
start:
	xor ax, ax
	mov es, ax       ;es = 0

	mov bx, 0009h*4  ;bx = address on 9th interrupt
	int 09h          ;call old 9th interrupt

	cli     ;CPU cannot work with interrupts

	mov es:[bx], offset new_9th_interrupt    ;es:[bx] = address on new 9th interrupt

	push cs
	pop ax     ;ax = cs (segment of our code and code with new 9th interrupt)
 	
	mov es:[bx+2], ax    ;es:[bx+2] in table with interrupts = cs (segment address, where is code with new 9th interrupt)

	sti       ;CPU can work with interrupts   

	int 09h   ;call new 9th interrupt

	mov ax, 3100h       ;complete program, return 00h and save code in RAM from segment address in PSP in quantity = dx * 16 bytes

	mov dx, offset end_of_program  
	shr dx, 4
	inc dx               ;dx = (address of program's end)/4 + 1

	int 21h              ;call 21th interrupt  (end work of CPU)
;--------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------------------
;											 new_9th_interrupt
;Has code for new 9th interrupt, that can print key in video memory
;Entry: None                                      
;
;Exit:  None
;
;Destr: None
;--------------------------------------------------------------------------------------------------------------

new_9th_interrupt proc     

	push ax
	push bx
	push es    ;save registers

	mov ax, 0b800h     ;video segment
	mov es, ax         ;register es for segment address of video memory  (es != const    es == reg)

	mov bx, 5*80*2 + 40*2    ;address of point for print in video_memory

	mov ah, color            ;color of symbol

	in al, 60h   ;al = scan code of last key from 60th port

	mov es:[bx], ax    ;print scan code from keyboard with color in video memory

;-------------------------------------------------------------------------------------------------------
;			This code simulates releasing of key, and we can wark with new key  (new interrupt must fix condition of keyboard's working)
	in al, 61h    
	mov ah, al   ;save value of keyboard control lines

	or al, 80h    ;set high bit and turn on keyboard
	out 61h, al   ;write value of keyboard control lines with 'turn on' bit in control port

	mov al, ah    ;delete high bit and turn off keyboard  (<==>   and al, not 80h)
	out 61h, al   ;write value of keyboard control lines in control port
;-------------------------------------------------------------------------------------------------------

	mov al, 20h  
	out 20h, al    ;send signal about end of interrupt to the 8259 interrupt controller

	pop es
	pop bx
	pop ax

	iret     
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
end_of_program:
end start              ;end of asm and address of program's beginning

;list of colors: 
;01011011 = blue symbols on violet background
