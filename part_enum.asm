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
ptype	equ work_addr

; Binary UUID to check for.
uuid	equ work_addr + 1

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
pt_num_entries	equ 4
pt_size		equ pt_num_entries * pte_size
pt_off		equ sig_off - pt_size

; Partition entry offset for type octet.
pte_type	equ 4

; Partition entry offset for relative sectors.
pte_relsecs	equ 8
pte_relsecs_size \
		equ 8

; Partition entry offset for sector count.
pte_totalsecs	equ 0ch

; Partition type for "EXTENDED" partitions. These form a linked chain we need
; to traverse in order to locate all logical drives (if any).
ptype_EXTENDED:	equ 5

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
s_uuid	equ 68h
s_uuid_size	equ 16

s_offset_superblock \
		equ 2

%macro		hello83 0
		pushf
		push si
		mov si,ascii_ptype
		call puts
		popf
		pop si
%endmacro

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
		mov si,(ascii_ptype - s2start + s1end)
		mov di,ptype
		call parse_hex
		mov si,(ascii_uuid - s2start + s1end)
		mov di,uuid
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
		; encountered. '-' are NO-OPs. Trashes AX and CX.
parse_hex:	mov ch,1 ; Toggle.
.next_digit:	lodsb
		cmp al,'-' ; Ignore dashes.
		je .next_digit
		sub al,'0' ; ASCII lowercase hex-digit in AL -> binary.
		jae .lower_ok
.no_more:	ret
.lower_ok:	cmp al,10
		jb .ok
		sub al,('a' - ('9' + 1))
		cmp al,16
		jae .no_more
.ok:		mov cl,4 ; AH contains the byte read so far.
		shl ah,cl ; Shift in the new nibble from the right.
		or ah,al
		neg ch ; For every first of 2 digits, it was positive.
		js .next_digit ; Negative now? Then we need the 2nd digit.
		mov al,ah ; Byte in AH is complete - add it to the result.
		stosb
		jmp .next_digit

s1end:		; Start of source copy area.
s1size		equ s1end - s1start

		section code2copy follows=.text \
		        vstart=(s1size + run_addr) align=1
s2start:	mov dl,80h ; Drive number to operate on (0x80 = 1st hard disk).
		; Assigned variables at this point:
		; DL: Drive number.
enum_drives:	mov ah,41h; Get int13 extensions supported by drive.
		mov bx,[run_addr + sig_off] ; Required signature argument.
		; Disk BIOS services may potentially trash: AX, SI, DI, BP, ES.
		push es
		int 13h
		pop es
		jc nextdrive ; Command failed.
		mov ax,cx
		shr ax, 1 ; Set CF to bit # 0 of feature bitmask.
		jnc nextdrive ; LBA packet addressing not supported.
		
		; Examine drive - read MBR at sector 0 to be examined next.
		mov si,[sector] ; Subtract [sector] from itself - will give 0.
		xor bl,bl ; No additional offset.
		stc ; Subtract mode.
		call read_relsector
		jc nextdrive ; Read error - ignore this drive.
		
enum_parttabs:	; Examine next partition table on current drive.
		; New partition table sector has just been successfully loaded.
		
		; Make a copy of the partition table for continued use.
		mov si,(load_addr + pt_off)
		mov di,(run_addr + pt_off)
		mov cx,pt_size
		rep movsb
		
		; Examine partition table of MBR or EBR in 2 phases.
		mov dh,[ptype] ; First, search for boot partition type.

enum_phases:	; Examine partiton table in current phase.
		;
		; Assigned variables at this point:
		; DL: Drive number.
		; DH: Partition type to search for in this phase.
		mov si,(run_addr + pt_off + pte_type)
		mov cx, pt_num_entries
		
enum_parts:	; Assigned variables at this point:
		; DL: Drive number.
		; DH: Partition type to search for in this phase.
		; SI: Points at the partition type field of the current PTE.
		; CX: Number of remaining partitions in current table.
		cmp dh,[si] ; DH contains partition type to locate.
		jne nextpart

		; Partition type matches.
		cmp dh,ptype_EXTENDED ; Are we in phase # 2?
		mov bl,s_offset_superblock ; We will load an ext2+ super block.
		jne .have_offset ; But only in phase # 1.
		xor bl,bl ; Otherwise, load an EBR in phase # 2.

		; Add BL to RELSECS from PT entry as the new sector number.
.have_offset:	push si
		add si,(pte_relsecs - pte_type) ; Point to RELSECS field.
		call read_relsector ; CF=0 because 'add si' should not overflow.
		pop si
		jc nextdrive ; Read error - ignore this drive.
		cmp dh,ptype_EXTENDED ; Are we in phase # 2?
		je enum_parttabs ; Yes, examine next EBR.

		; Phase # 1 - Examine potential ext2+ superblock.
		; Check superblock magic.
		mov ax,[load_addr + s_magic_off]
		cmp ax,ext_magic
		jne .notsuper ; does not match.
		
		; Check whether there is a UUID.
		mov ax,[load_addr + s_rev_level_off]
		cmp ax, s_rev_level_min
		jb .notsuper ; No UUID.
		
		; Finally, compare the UUID itself.
		push si
		push cx
		mov si,[load_addr + s_uuid]
		mov di,[uuid]
		mov cx,s_uuid_size
		repe cmpsb ; ZF=1 only if match and loop is exhausted.
		pop cx
		pop si
		jne .notsuper ; UUIDs did not match.
		
		; We have found the searched-for partition!
		; Load its boot block.
		add si,(pte_relsecs - pte_type) ; Point to RELSECS field.
		mov bl,s_offset_superblock
		stc ; but subtract it this time.
		call read_relsector
		jc stop ; Extremely unlikely as we already read it before.
		xor bl,bl ; CF=0 and BL=0
		call read_relsector ; add again, but exclude superblock offset.
		jc stop ; We are fucked! Very unlikely to happen, though.
		
		; Execute to loaded boot code.
		mov si,found
		call puts
		jmp s1start ; Has been replaced with loaded code, though.

.notsuper:	push si
		add si,(pte_relsecs - pte_type) ; Point to RELSECS field.
		mov bl,s_offset_superblock
		stc ; but subtract it this time.
		call read_relsector
		pop si
		jc stop ; Extremely unlikely as we already read it before.
		
nextpart:	; Assigned variables at this point:
		; DL: Drive number.
		; DH: Partition type to search for in this phase.
		; SI: Points at the partition type field of the current PTE.
		; CX: Number of remaining partitions in current table.
		add si,pte_size
		loop enum_parts ; Process remaining PTEs in current phase.
		mov ah,ptype_EXTENDED
		cmp dh,ah ; Are we in phase # 2 already?
		je nextdrive ; No more partiton tables on this drive.
		mov dh,ah ; Next phase - search for EXTENDED partition.
		jmp enum_phases
		
nextdrive:	; Assigned variables at this point:
		; DL: Drive number.
		inc dl
		mov dh,7fh
		and dh,dl ; DH: 0-based drive number.
		cmp dh,[bda_hdd_count]
		jb enum_drives

		mov si,not_found
		call puts
stop:		hlt
		jmp stop

read_relsector:	; Add or subtract QWORD at [DS:SI] and also BL to/from
		; [sector]. CF=0 will add, CF=1 will subtract.
		; DL is drive number.
		; Then try to read the sector and set CF=1 on error.
		push ax
		push bx
		push cx
		push dx
		push si
		push di
		push es
		rcr dl ; Save add/subtract flag in CF into sign-bit of DL.
		xor bh,bh ; Make BL a WORD value in BX.
		mov di,sector
		mov cx,4 ; Process QWORDs as 4 * 2 WORDs.
.nextword:	lodsw ; Current limb from source into AX.
		test dl,dl
		js .subtract
		add ax,[di]
		adc ax,bx
		jmp .store
.subtract:	sub ax,[di]
		sbb ax,bx
.store:		stosw ; Set limb to result and increment DI.
		sbb bx,bx ; Convert carry/borrow to new BX.
		neg bx ; BX will be 1 if there was a carry/borrow, else 0.
		loop .nextword
		mov ah,42h ; Extended Read Sectors From Drive.
		mov si,dap
		int 13h ; Read sector.
		push es
		push di
		push si
		push dx
		push cx
		push bx
		push ax
		ret

puts:		; Displays null-terminated string in [DS:SI]++
		push ax
		push bx
		push bp
		push di
		; Video BIOS services may trash AX, SI, DI, BP unless used for
		; results.
		mov ah,0fh
		push si
		int 10h ; Get active video page into BH, trashes AX.
		pop si
		mov bl,0bh ; Foreground color (most likely unused).
		jmp .getchar
.outchar	mov ah,0eh ; 'tty output' action code for INT10h.
		push si
		int 10h
		pop si
.getchar:	lodsb
		test al,al ; Check for null terminator.
		jnz .outchar
		pop di
		pop bp
		pop bx
		pop ax
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

not_found:	db '*NOT* '
found:		db 'Found: ext2+ FS {'
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

