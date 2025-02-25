;------------------------------------------------------------------------------------------------------------------
;This program is the test fro working of resident frame
;------------------------------------------------------------------------------------------------------------------
;                                        program

.model tiny   ;64 kilobytes in RAM, address == 16 bit == 2 bytes (because: sizeof (register) == 2 bytes)

;--------------------------------------------------------------------------------------------------------------
;										main program
;--------------------------------------------------------------------------------------------------------------

.code         ;begin program
org 100h      ;start == 256:   jmp start == jmp 256 != jmp 0 (because address [0;255] in program segment in DOS for PSP)
start:

	mov ax, 1111h
	mov bx, 2222h
	mov cx, 3333h
	mov dx, 4444h

	get_next_scan_code_of_key:
	in al, 60h
	jmp get_next_scan_code_of_key

	mov ax, 4c00h       ;complete program, return 00h

	int 21h              ;call 21th interrupt  (end work of CPU)

.data

end start              ;end of asm and address of program's beginning