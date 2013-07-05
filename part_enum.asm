; Print drive geometry

; The code or the original MBR and chained boot block will be loaded there.
load_addr	equ 07c00h

; The ext2/3/4 superblock will be loaded there.
superblock	equ load_addr

; The current partition table sector being processed will be stored there.
pblock_addr	equ superblock - sector_size

; Most of the code will be copied to and run from there.
run_addr	equ pblock_addr - sector_size

; Global variables will be located there.
work_addr	equ run_addr - sector_size

; The stack (growing down) ends there (exclusive).
stacktop_addr	equ work_addr

; Byte size of the MBR and all other sectors.
sector_size	equ 200h

; 0x55, 0xaa boot signature.
sig_size	equ 2
sig_off		equ sector_size - sig_size

; Partition table.
pte_size	equ 10h
pt_size		equ 4 * pte_size
pt_off		equ sig_off - pt_size

; Partition entry offset for type octet.
pte_type	equ 4

; Partition entry offset for relative sectors.
pte_relsecs	equ 8

; Partition entry offset for sector count.
pte_totalsecs	equ 0ch

; "Disk serial number valid"-indicator size, precedes partition table.
sn_ind_size	equ 2

; Disk serial number size, precedes disk serial number.
sn_size		equ 4

; EXT2/3/4 superblock magic value.
s_magic_off	equ 38h
s_magic_size	equ 2
ext_magic	equ 0ef53h

; Must be >= s_rev_level_min in order for a UUID to exist. Also, just to be
; sure, check that it is not above s_rev_level_max.
s_rev_level_off	equ 04ch
s_rev_level_size \
		equ 4
s_rev_level_min	equ 1
s_rev_level_max	equ 7

; Where to find the filesystem UUID. It is stored in the same byte order as
; the hex digits are displayed.
s_uuid_off	equ 68h
s_uuid_size	equ 16


; Normalized load segment and offset
%define fpseg(flat) ((flat) / 10h)
%define fpoff(flat) ((flat) & 0fh)

		cpu 8086
		bits 16

		section .text vstart=load_addr
s1start:	cli
		jmp 0:.flat
		
.flat:		; Set up flat address space.
		xor ax,ax
		mov ss,ax
		mov es,ax
		mov ss,ax
		mov sp,stacktop_addr
		cld
		sti

		; Copy the partition table.
		mov si,(s1start + pt_off)
		mov di,(pblock_addr + pt_off)
		mov cx,(pt_size + sig_size)
.pcopy:		lodsb
		stosb
		loop .pcopy

		; Copy the remaining code.
		mov si,s1end
		mov di,(s1size + run_addr)
		mov cx,s2size
.ccopy:		lodsb
		stosb
		loop .ccopy

		; Execute the copied code.
		jmp s2start
s1end:		; Start of source copy area.
s1size		equ s1end - s1start

		section code2copy follows=.text \
		        vstart=(s1size + run_addr) align=1
s2start:	xor dl,dl ; Drive number to operate on.
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

s2end:		; Start of unused space after used space.
s2size		equ s2end - s2start

code_space	equ s1size + s2size
unused_code_space \
		equ   sector_size - sig_size \
		    - pt_size - sn_ind_size - sn_size - code_space

		; Fill the unused rest of the code space with NO-OPs.
		times unused_code_space nop

		times sn_size db 'S' ; Invalid disk serial number.
		times sn_ind_size db 0ffh ; Mark serial number as invalid.
		times pt_size db 0 ; Empty partition table.

		db 55h, 0aah ; MBR / EBR signature.

