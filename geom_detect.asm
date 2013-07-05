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
.entry2:	; Set up flat address space.
		xor ax,ax
		mov ss,ax
		mov es,ax
		mov ss,ax
		mov sp,load_addr
		cld
		sti
		xor dl,dl ; Drive number to operate on.
enum:		xor ah,ah; Reset Disk System.
		push dx
		int 13h
		mov al,ah
		xor ah,ah
		mov si,error
		call puts
		call putns
		jmp stop

		jc .nextdrive
		pop dx
		mov ah,8 ; Get Current Drive Parameters.
		push dx
		int 13h
		jnc report
.nextdrive:	pop dx
		mov dh,80
		cmp dl,dh
		jae done
		mov dl,dh
		jmp enum
report:		mov si,msg
		call puts
		mov al,dl ; # of drives.
		xor ah,ah
		call putns
		mov al,ch ; # of cylinders.
		mov ah,cl
		mov cl,6
		shr ah,cl
		call putns
		mov al,dh ; # heads.
		xor ah,ah
		call putns
		mov al,cl ; # spt.
		and cl,63
		call putns
		pop dx
		inc dl
		jmp enum

done:		mov si,endtext
		call puts
stop:		hlt
		jmp stop

putn:		; Display an unsigned decimal integer in AX.
		push ax
		push bx
		push dx
		mov bx,10
		call .putn1
		pop dx
		pop bx
		pop ax
		ret

.putn1:		; Integer in AX. BX == 10. DX and AX can be trashed.
		cmp ax,bx
		jb .last ; It is only a single decimal digit.
		xor dx,dx ; Dividend is DX:AX.
		div bx ; Otherwise divide by 10.
		push dx ; Remainder.
		call .putn1 ; Recursively display the leading digits first.
		pop ax ; Display the last digit now.
.last:		add al,'0' ; Make it an ASCII character.
		; Fall through.

putc:		; Display an ASCII character in AL.
		push ax
		xor ah,ah ; Code for "display character".
		jmp video

putns:		; Display unsigned number in AX and then call puts.
		call putn
		; Fall through.

puts:		; Displays null-terminated string in [DS:SI]++
		push ax
		mov ah,01h ; Code for "display string".
		; Fall through.

; Video BIOS services may trash AX, SI, DI, BP unless used for results.
video:		; Displays character or string depending on code in AH.
		push bx
		push bp
		push di
		push ax
		mov ah,0fh
		push si
		int 10h ; Get active video page into BH, trashes AX.
		pop si
		pop ax
		mov bl,0bh ; Foreground color (most likely unused).
		cmp ah,0
		jne .string
		call .single
.end:		pop di
		pop bp
		pop bx
		pop ax
		ret

.string:	lodsb
		test al,al
		jz .end
		call .single
		jmp short .string

.single:	mov ah,0eh ; 'tty output' action code for INT10h.
		push si
		int 10h
		pop si
		ret


msg:		db 'Disk parameters for device ', 0
		db ':', 13, 10, 0
		db ' drives, ', 0
		db ' cylinders, ', 0
		db ' heads, ', 0
		db ' sectors/track.', 13, 10, 13, 10, 0

endtext:	db 'End of list of disk devices.', 13, 10, 0

error:		db 'Disk error # ', 0, '.', 13, 10, 0

		times (sectorsize - ($ - entry) - 2) nop
		db 55h, 0aah

