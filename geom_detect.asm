; Print drive geometry

; The code will be loaded there.
%define load_addr 07c00h
%define sectorsize 512

; Normalized load segment and offset
%define fpseg(flat) ((flat)/10h)
%define fpoff(flat) ((flat)&0fh)

		cpu 8086
		section .text
		org load_addr
entry:		cli
		jmp 0:.entry2
.entry2:	; setup flat address space.
		xor ax,ax
                mov ss,ax
                mov es,ax
                mov ss,ax
                mov sp,load_addr
		cld
		sti
		mov si,hello
		call puts
.stop:		hlt
		jmp .stop

; Video BIOS services may trash AX, SI, DI, BP unless used for results.
puts:		; displays null-terminated string in [DS:SI]++
		push ax
		push bx
		push bp
		push di
		mov ah,0fh
		push si
		int 10h ; get active video page into BH, trashes AX
		pop si
		mov bl,0bh ; foreground color (most likely unused)
.loop:		lodsb
		test al,al
		jz .end
		mov ah,0eh ; 'tty output' action code for INT10h
		push si
		int 10h
		pop si
		jmp short .loop
.end:		pop di
		pop bp
		pop bx
		pop ax
		ret

hello:		db 'Hello, world!', 13, 10, 0

		times (sectorsize - ($ - entry) - 2) nop
		db 55h, 0aah

