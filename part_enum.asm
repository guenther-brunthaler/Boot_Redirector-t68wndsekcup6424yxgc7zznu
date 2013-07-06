; Enumerate partitions.
;
; (c) 2013 by Guenther Brunthaler.
; This source file is free software.
; Distribution is permitted under the terms of the GPLv3.

; The code or the original MBR and chained boot block will be loaded there.
load_addr	equ 07c00h

; The ext2/3/4 superblock will be loaded there.
superblock	equ load_addr

; Most of the code will be copied to and run from there.
run_addr	equ superblock - sector_size

; Global variables will be located there.
work_addr	equ run_addr - sector_size

; Binary partition type to check for.
ptype_off	equ work_addr

; Binary UUID to check for.
uuid_off	equ work_addr + 1

; The stack (growing down) ends there (exclusive).
stacktop_addr	equ work_addr

; Byte size of the MBR and all other sectors.
sector_size	equ 200h

; BIOS data area: Number of hard disks attached (single byte value).
; It seems CD-ROM drives are also included into this count.
bda_hdd_count	equ 475h

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
		
		; Parse ascii hex literals into binary for later comparison.
		mov si,ascii_ptype
		mov di,ptype_off
		call parse_hex
		mov si,ascii_uuid
		mov di,uuid_off
		call parse_hex

		; Copy the remaining code.
		;
		; We don't change the offsets relative to the start of the
		; sector, allowing easier manual code review. Also there is
		; enough memory available.
		mov si,s1end
		mov di,(s1size + run_addr)
		mov cx,s2size
		rep movsb

		; Execute the copied code.
		jmp s2start
		
		; Parse ASCII hex constant at DS:SI into binary ES:DI.
		; Stop when a character other than '0'-'9' 'a'-'f' or '-' is
		; encountered. '-' are NO-OPs.
parse_hex:	xor ch,ch
.next_digit:	lodsb
		sub al,'0'
		jae .lower_ok
.no_more:	ret
.lower_ok:	cmp al,10
		jb .ok
		sub al,('A' - ('9' + 1))
		cmp al,16
		jae .no_more
.ok:		mov cl,4
		shl ah,cl
		add ah,al
		inc ch
		shr ch,1
		jc .next_digit
		mov al,ah
		stosb
		jmp .next_digit

s1end:		; Start of source copy area.
s1size		equ s1end - s1start

		section code2copy follows=.text \
		        vstart=(s1size + run_addr) align=1
s2start:	mov dl,80h ; Drive number to operate on (0x80 = 1st hard disk).
enum_drives:	mov ah,41h; Get int13 extensions supported by drive.
		mov bx,[run_addr + sig_off]
		; Disk BIOS services may potentially trash: AX, SI, DI, BP, ES.
		push es
		int 13h
		pop es
		jc nextdrive ; Command failed.
		mov ax,cx
		shr ax, 1 ; Set CF to bit # 0 of feature bitmask.
		jnc nextdrive ; LBA packet addressing not supported.
		
		; Examine drive - set MBR at sector 0 to be examined next.
		mov di,sector
		xor ax,ax
		stosw
		stosw
		stosw
		stosw
		
enum_parttabs:	; Examine next partition table on current drive.
		call read_sector
		jc nextdrive ; Read error - ignore this drive.
		
		; Make a copy of the partition table for continued use.
		mov si,(load_addr + pt_off)
		mov di,(run_addr + pt_off)
		mov cx,pt_size
		rep movsb
		
		; Examine partition table of MBR or EBR in 2 phases.
		mov si,(run_addr + pt_off)
		
nextdrive:	inc dl
		mov dh,7fh
		and dh,dl
		cmp dh,[bda_hdd_count]
		jb enum_drives

		mov si,not_found
		call puts
stop:		hlt
		jmp stop

read_sector:	; Read sector with currently set sector number in DAP.
		push ax
		push si
		mov ah,42h ; Extended Read Sectors From Drive.
		mov si,dap
		push es
		int 13h
		pop es
		pop si
		pop ax
		ret

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

dap_tpl:	; DAP - Disk Address Packet (template to be copied).
.self_size:	db .struct_size - $
.reserved:	db 0
.numsects:	dw 1
.bufoff:	dw load_addr
.bufseg:	dw 0
.sector		dq 0 ; Sector to read (0-based).
.struct_size:

dap		equ dap_tpl - load_addr + run_addr
sector		equ dap_tpl.sector - load_addr + run_addr

; Message text area.

%define NL 13, 10 ; CR/LF is newline.
%define LNUM 0, '.', NL, 0

debug:		db "Yeah - read a sector!", NL, 0

not_found:	db 'NOT '
found:		db 'Found ext2+ FS {'
ascii_uuid:	db '67758eb7-5baa-4c14-ba21-cbf38d0180f3'
		db '} ptype 0x'
ascii_ptype:	db '83', NL, 0

s2end:		; Start of unused space after used space.
s2size		equ s2end - s2start

; Gap between the used and unused part of the MBR.

leading_data	equ s1size + s2size
unused_space 	equ   sector_size - sig_size \
		    - pt_size - sn_ind_size - sn_size - leading_data

		; Fill the unused rest of the code space with NO-OPs.
		times unused_space nop

; Fixed data structures at the end of the MBR.

		times sn_size db 'S' ; Invalid disk serial number.
		times sn_ind_size db 0ffh ; Mark serial number as invalid.
		times pt_size db 0 ; Empty partition table.

		db 55h, 0aah ; MBR / EBR signature.

