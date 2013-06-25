; BOOT Redirector
; for Boot Loaders that execute the contents of a file
; as an MS-DOS boot-sector (such as NT loader)
;
; $Author: gb $
; $Revision: 1002 $


; The following definition can be changed for debugging purposes:
; It specified the offset <loadseg:loadoff> where the code will execute.
;%define loadseg 01f44h
;%define loadoff 01200h
;%define loadseg 02b91h
;%define loadoff 01200h
%define loadseg 00000h
%define loadoff 07c00h

; This is the size of the boot sector in bytes. Do not change.
%define sectorsize 200h

; Normalized load segment and offset
%define normseg (loadseg+(loadoff/10h))
%define normoff (loadoff&0fh)

; Normalized work segment and offset
%define workseg (normseg-(sectorsize/10h))
%define workoff normoff

; Top of stack: Same as (loadseg:loadoff)-sectorsize
%define TOS (loadseg*10h+loadoff-sectorsize)
%define stackseg (TOS/10h&0f000h)
%define stackoff (TOS&0ffffh)

		section .text
		org 0 ; the sector will actually be loaded to <loadpos>
entry:		cli
		jmp word (normseg):entry2
entry2:		; setup registers and stack
                lds si,[cs:copy_source]
                les di,[copy_dest]
                lss sp,[intial_tos]
		cld
		; copy remaining code <sectorsize> bytes before loadpos
		mov cx,tpl_end-tpl_start
		rep movsb
		jmp workseg:tpl_start

tpl_start:	sti
		mov ax,es ; synchronize DS with ES
		mov ds,ax
 		; initialize parameter pointer
		mov ax,configdata
		mov [configptr],ax
		; reset disk
		call getnextpar ; DRIVE
		mov dl,al
		mov ah,0dh ; reset hard disk controller
		call doint13
		; display msg
		mov si,loadmsg
		call puts
		xor ah,ah
		mov [endconfig], ah ; terminate config string
		mov si,configdata
		call puts
		; read sector
		call getnextpar ; CYLINDER
		mov ch,al ; cyl 7..0
		shl ah,6
		mov cl,ah ; cyl 9..8
		mov cx,ax
		call getnextpar ; HEAD
		mov dh,al
		call getnextpar ; SECTOR
		or cl,al
		mov bx,sectorsize ; equals original <entry>
		mov ax,0201h ; read 1 sector
		call doint13
		; execute code
		mov si,exemsg
		call puts
		; jump to loaded boot sector
		mov ax,loadseg
		mov ds,ax
		mov es,ax
		mov ss,ax
		jmp loadseg:loadoff
		
doint13:	; executes int13h and checks for successful operation
		int 13h
		jc .error
		ret
.error:		mov dh,ah
		mov di,errcode
		shr ax,4+8
		call wrnibble
		mov al,dh
		and al,0fh
		call wrnibble
		mov si,int13err
		call puts
.endless	hlt
		jmp short .endless

puts:		; displays null-terminated string in [DS:SI]++
		push bx
		mov ah,0fh
		int 10h ; get active video page into BH, trashes AX
		xor bh,0bh ; foreground color (most likely unused)
		mov ah,0eh ; 'tty output' action code for INT10h
.loop:		lodsb
		test al,al
		jz .end
		int 10h
		jmp short .loop
.end:		pop bx
		ret

wrnibble:	; writtes value 0-15 in AL into [ES:DI]++
		cmp al,10
		jb .noadjust
		add al,'A'-('9'+1)
.noadjust:	add al,'0'
		stosb
		ret

getnextpar:	; fetches the next value from <configptr> into AX
		push dx
		push si
		push di
		push cx
		mov di,[configptr]
		mov cx,endconfig-configdata
		mov al,10 ; scan for next newline
		repne scasb
		mov al,'=' ; scan for next '='
		repne scasb
		mov al,' ' ; skip spaces
		repe scasb
		dec di
		mov si,di
		xor ax,ax
.loop:		mov dx,ax ; save number
		lodsb
		cmp al,'0'
		jb  .end
		cmp al,'9'
		ja  .end
		sub al,'0' ; make digit
		xor ah,ah
		mov di,ax ; save new_digit
		mov ax,dx ; AX:= 1 * number
		shl ax,2 ; AX:= 4 * number
		add ax,dx ; AX:= 5 * number
		add ax,ax ; AX:= 10 * number
		add ax,di ; AX:= 10 * number + new_digit
		jmp short .loop
.end:		mov [configptr],si
		mov ax,dx
		pop cx
		pop di
		pop si
		pop dx
		ret

copy_source:	dw normoff+tpl_start, normseg
copy_dest:	dw workoff+tpl_start, workseg
intial_tos:	dw stackoff, stackseg

configptr:	; points to where to continue parameter search
		dw 0
		
loadmsg:	db 'LOADING BOOT SECTOR FROM', 13, 10, 0
exemsg:		db 13, 10, 'EXECUTING...', 13, 10, 13, 10, 0
int13err:	db 13, 10, 'INT-13h ERROR # '
errcode:	db '??', 13, 10, 0		
		db 13, 10
		db '*****', 13, 10
		db 'CHANGE DECIMALS BELOW; '
		db "DON'T CHANGE FILE SIZE:", 13, 10
configdata:	db 13, 10
		db 'DRIVE = 129 (0-1...FDDs, 128-255...HDDs)', 13, 10
		db 'CYLINDER = 0000 (0-1023)', 13, 10
		db 'HEAD = 001 (0-254)', 13, 10
		db 'SECTOR = 01 (1-63)', 13, 10
tpl_end:
endconfig:	db '*****', 13, 10
		times (sectorsize - ($ - entry) - 2) db 'x'
		db 55h, 0aah
