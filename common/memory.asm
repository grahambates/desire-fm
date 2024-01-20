		include	_main.i
		include	common/memory.i

ERROR_OOM_CHIP = $ff0			; yellow
ERROR_OOM_PUBLIC = $00f			; blue

CHIP_BUFFER_SIZE = 1024*250
PUBLIC_BUFFER_SIZE = 1024*300

********************************************************************************
; Memory
********************************************************************************

OutOfChip:
		move.w	#ERROR_OOM_CHIP,color00(a6)
		bra	OutOfChip
OutOfPublic:
		move.w	#ERROR_OOM_PUBLIC,color00(a6)
		bra	OutOfPublic


********************************************************************************
; Allocate public RAM
;-------------------------------------------------------------------------------
; d0 = bytes
; returns a0 = address
;-------------------------------------------------------------------------------
MemAllocPublic:
		tst.w	Desc
		bne	MemAllocPublicDesc
MemAllocPublicAsc:
		move.l	PublicOffsAsc(pc),a0 ; current offset is new address
		move.l	d0,d1		; d1 = new offset
		add.l	a0,d1
		cmp.l	PublicOffsDesc(pc),d1 ; check bounds
		bgt	OutOfPublic
		move.l	d1,PublicOffsAsc
		rts
MemAllocPublicDesc:
		move.l	PublicOffsDesc(pc),a0 ; a0 - current offset
		sub.l	d0,a0		; update offset
		cmp.l	PublicOffsAsc(pc),a0 ; check bounds
		blt	OutOfPublic
		move.l	a0,PublicOffsDesc ; write new offset
		rts


********************************************************************************
; Allocate chip RAM
;-------------------------------------------------------------------------------
; d0 = bytes
; returns a0 = address
;-------------------------------------------------------------------------------
MemAllocChip:
		tst.w	Desc
		bne	MemAllocChipDesc
MemAllocChipAsc:
		move.l	ChipOffsAsc(pc),a0 ; current offset is new address
		move.l	d0,d1		; d1 = new offset
		add.l	a0,d1
		cmp.l	ChipOffsDesc(pc),d1 ; check bounds
		bgt	OutOfChip
		move.l	d1,ChipOffsAsc
		rts
MemAllocChipDesc:
		move.l	ChipOffsDesc(pc),a0 ; a0 - current offset
		sub.l	d0,a0		; update offset
		cmp.l	ChipOffsAsc(pc),a0 ; check bounds
		blt	OutOfChip
		move.l	a0,ChipOffsDesc	; write new offset
		rts


********************************************************************************
; Allocate chip RAM within a single high word offset (i.e. 64k block).
; Used so we can update just the lower word in copperlist.
;-------------------------------------------------------------------------------
; d0 = bytes
; returns a0 = address
;-------------------------------------------------------------------------------
MemAllocChipAligned:
		tst.w	Desc
		bne	MemAllocChipAlignedDesc
MemAllocChipAlignedAsc:
		move.l	ChipOffsAsc(pc),a0
		move.l	a0,d1
		move.l	d1,d2		; d2 = start address
		add.l	d0,d1		; d1 = end address
; compare upper word
		swap	d1
		swap	d2
		cmp.w	d1,d2
		beq	.ok
; Not ok, need to adjust start
		swap	d1		; clear lower word of end address
		clr.w	d1
		move.l	d1,a0		; update returned address
		add.l	d0,d1		; add bytes to new start
		swap	d1
.ok:
; Ok, just swap back and set
		swap	d1
		cmp.l	ChipOffsDesc(pc),d1
		bgt	OutOfChip
		move.l	d1,ChipOffsAsc
		rts

MemAllocChipAlignedDesc:
		move.l	ChipOffsDesc(pc),a0
		move.l	a0,d1
		move.l	d1,d2		; d2 = start address
		sub.l	d0,d1		; d1 = end address
; compare upper word
		swap	d1
		swap	d2
		cmp.w	d1,d2
		beq	.ok
; Not ok, need to adjust start
		swap	d1		; clear lower word of end address
		clr.w	d1
		move.l	d1,a0		; update returned address
		sub.l	d0,d1		; add bytes to new start
		swap	d1
.ok:
; Ok, just swap back and set
		swap	d1
		cmp.l	ChipOffsAsc(pc),d1
		blt	OutOfChip
		move.l	d1,ChipOffsDesc
		rts


********************************************************************************
; Free allocated RAM in previous direction
;-------------------------------------------------------------------------------
MemFreeLast:
		tst.w	Desc
		beq	MemFreeDesc	; Frees the direction *not* in use
MemFreeAsc:
		move.l	#PublicBuffer,PublicOffsAsc
		move.l	#ChipBuffer,ChipOffsAsc
		rts
MemFreeDesc:
		move.l	#PublicBufferE,PublicOffsDesc
		move.l	#ChipBufferE,ChipOffsDesc
		rts


********************************************************************************
; Change memory direction
;-------------------------------------------------------------------------------
MemFlip:
		not.w	Desc
		bne	MemFreeDesc
		bra	MemFreeAsc


*******************************************************************************
; Vars
*******************************************************************************

Desc:		dc.w	0
PublicOffsAsc:	dc.l	PublicBuffer
ChipOffsAsc:	dc.l	ChipBuffer
PublicOffsDesc:	dc.l	PublicBufferE
ChipOffsDesc:	dc.l	ChipBufferE


*******************************************************************************
		bss
*******************************************************************************

PublicBuffer:
		ds.b	PUBLIC_BUFFER_SIZE
PublicBufferE:

*******************************************************************************
		bss_c
*******************************************************************************

ChipBuffer:
		ds.b	CHIP_BUFFER_SIZE
ChipBufferE:
