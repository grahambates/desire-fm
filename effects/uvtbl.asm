		include	_main.i
		include	effects/uvtbl.i

; Based on examples by
; Ghostown:
; https://github.com/cahirwpz/demoscene/blob/master/effects/uvmap/uvmap.c
; paraj (Michael Rasmussen):
; https://eab.abime.net/showthread.php?t=86108

; Scrambled textures:
;-------------------------------------------------------------------------------
; input      = __ __ __ __ a3 a2 a1 a0
;
; Basic scramble:
; lower      = __ __ a3 a2 __ __ a1 a0
; up         = a3 a2 __ __ a1 a0 __ __
;
; Additional variants for optimisations:
; duplicate  = a3 a2 a3 a2 a1 a0 a1 a0       Where b = a
; sequential = a3 a2 b3 b2 a1 a0 b1 b0       Where b = a+1
;
; Chunky layout:
;-------------------------------------------------------------------------------
; a3 a2 b3 b2 a1 a0 b1 b0   e3 e2 f3 f2 e1 e0 f1 f0
; c3 c2 d3 d2 c1 c0 d1 d0   g3 g2 h3 h2 g1 g0 h1 h0
;
; Swap 2x4:
;-------------------------------------------------------------------------------
;  a3 a2 b3 b2 [c3 c2 d3 d2]  e3 e2 f3 f2 [g3 g2 h3 h2]
; [a1 a0 b1 b0] c1 c0 d1 d0  [e1 e0 f1 f0] g1 g0 h1 h0
;
; Expand 2x1
;-------------------------------------------------------------------------------
; a3 a3 b3 b3 c3 c3 d3 d3   e3 e3 f3 f3 g3 g3 h3 h3
; a2 a2 b2 b2 c2 c2 d2 d2   e2 e2 f2 f2 g2 g2 h2 h2
; a1 a1 b1 b1 c1 c1 d1 d1   e1 e1 f1 f1 g1 g1 h1 h1
; a0 a0 b0 b0 c0 c0 d0 d0   e0 e0 f0 f0 g0 g0 h0 h0
;
; Blitter operations for swap and expand are chained together with blitter interrupts.
; Expand is started by a copper interrupt at DIW YSTOP in order to make full use of
; free cycles when bitplane DMA is off.

SCREEN_W = 320
SCREEN_H = 220				; max 244 in 4bpl
SCREEN_BW = ((SCREEN_W+15)/16)*2
SCREEN_SIZE = (SCREEN_H/2)*SCREEN_BW

CHUNKY_W = SCREEN_W/2
CHUNKY_H = SCREEN_H/2
CHUNKY_SIZE = SCREEN_SIZE*2

CIRC_Y = 52
CIRC_H = 100

DIW_XSTRT = $81+(320-SCREEN_W)/2
DIW_YSTRT = $2c+(256-SCREEN_H)/2
DIW_YSTOP = DIW_YSTRT+SCREEN_H
DIW_XSTOP = DIW_XSTRT+SCREEN_W

BLITPASS_SIZE = SCREEN_SIZE/2

TEX_W = 128
TEX_H = 128
TEX_SIZE = TEX_W*TEX_H

SCRAMBLED_TEX_SIZE = TEX_SIZE*4		; word per byte and repeated for scroll

SPRITE_H = 26


********************************************************************************
Script:
		dc.l	0,CmdLerpWord,15,7,FadePos

FRAME_CRED1 = 8*3000/BPM
FRAME_CRED1_OUT = 16*3000/BPM

FRAME_CRED2 = 24*3000/BPM
FRAME_CRED2_OUT = 32*3000/BPM

FRAME_CRED3 = 40*3000/BPM
FRAME_CRED3_OUT = 48*3000/BPM

		; Gigabates
		dc.l	FRAME_CRED1,CmdMoveIL,Cred1,TextSprite
		dc.l	FRAME_CRED1,CmdMoveIW,DIW_XSTRT-64,TextX
		dc.l	FRAME_CRED1,CmdMoveIW,DIW_YSTOP-36,TextY
		dc.l	FRAME_CRED1,CmdLerpWord,DIW_XSTRT+10,5,TextX
		dc.l	FRAME_CRED1_OUT,CmdLerpWord,DIW_YSTOP,5,TextY

		; MA2e
		dc.l	FRAME_CRED2,CmdMoveIL,Cred2,TextSprite
		dc.l	FRAME_CRED2,CmdMoveIW,DIW_XSTOP,TextX
		dc.l	FRAME_CRED2,CmdMoveIW,DIW_YSTRT+10,TextY
		dc.l	FRAME_CRED2,CmdLerpWord,DIW_XSTOP-74,5,TextX
		dc.l	FRAME_CRED2_OUT,CmdLerpWord,DIW_YSTRT-26,5,TextY

		; Steffest
		dc.l	FRAME_CRED3,CmdMoveIL,Cred3,TextSprite
		dc.l	FRAME_CRED3,CmdMoveIW,DIW_XSTRT-64,TextX
		dc.l	FRAME_CRED3,CmdMoveIW,DIW_YSTRT+10,TextY
		dc.l	FRAME_CRED3,CmdLerpWord,DIW_XSTRT+10,5,TextX
		dc.l	FRAME_CRED3_OUT,CmdLerpWord,DIW_YSTRT-26,5,TextY

		dc.l	64*3000/BPM-1<<4,CmdLerpWord,0,4,CropH

		dc.l	$1000


********************************************************************************
UvTbl_BlitInt:
********************************************************************************
		movem.l	d0/d1/a0/a6,-(sp)
		lea	custom,a6
		; Continue previous blit?
		move.w	BlitWords(pc),d0
		ble	.checkNext
		moveq	#0,d1		; d1 = blit size
		cmp.w	#1024,d0	; Max size?
		bge	.max
		move.w	#1023,d1	; Get remainder
		and.w	d0,d1
		lsl.w	#6,d1
.max:		addq	#1,d1		; Width: 1 word
		move.w	d1,bltsize(a6)
		sub.w	#1024,BlitWords	; Update remaining count
		bra	.end
		; Check next blit operation
.checkNext:	move.l	BlitNext(pc),d0
		beq	.end
		move.l	d0,a0
		jsr	(a0)		; Call blit routine
.end:		movem.l	(sp)+,d0/d1/a0/a6
		rts


********************************************************************************
UvTbl_Pre:
********************************************************************************
		jsr	MemFlip

		ALLOC_CHIP CHUNKY_W*CHUNKY_H*4+CHUNKY_W*CHUNKY_H/8+2
		move.l	a0,TableCode

		ALLOC_CHIP SCREEN_SIZE*4,DrawScreen
		ALLOC_CHIP SCREEN_SIZE*4,ViewScreen

		ALLOC_CHIP CHUNKY_SIZE,DrawChunky
		ALLOC_CHIP CHUNKY_SIZE,ViewChunky

		; This is pretty huge - 256k!
		ALLOC_PUBLIC SCRAMBLED_TEX_SIZE*4,ScrambledTex

		ALLOC_PUBLIC 14*32*2,PalData

		ALLOC_PUBLIC CHUNKY_W*(CHUNKY_H+1),XOffsets
		ALLOC_PUBLIC CHUNKY_W*(CHUNKY_H+1),YOffsets

		PRINT_MEM_TOTALS

		bsr	GenerateTable
		bsr	GenerateTableCode
		bsr	ScrambleTexture

		lea	BlankData,a0
		lea	TexturePal,a1
		move.l	PalData(pc),a2
		lea	PalPtrs,a3
		moveq	#16-1,d0
		moveq	#32-1,d1
		jmp	PreLerpPal


********************************************************************************
UvTbl_Effect:
********************************************************************************
		jsr	SetBgTask
		jsr	MemFreeLast

		WAIT_BLIT
		move.l	#-1,bltafwm(a6)
		clr.w	bltdmod(a6)
		bsr	SetPalette
		bsr	PokeCopper

		jsr	WaitEOF
		move.w	#DMAF_SETCLR!DMAF_SPRITE,dmacon(a6)
		bsr	SetNullSprites
		move.l	#Cop,cop1lc(a6)

		; Set blitter interrupt
		move.l	#UvTbl_BlitInt,BlitterInterrupt
		move.w	#INTF_SETCLR!INTF_BLIT,intena(a6)

		move.l	#UvTbl_Vbi,VBlankInterrupt

		lea	Script,a0
		jsr	Commander_Init

********************************************************************************
Mainloop:
		; Start blitter swap operations. These will chain together with BlitNext
		bsr	StartBlit


		move.l	DrawChunky(pc),a0
		lea	CHUNKY_SIZE(a0),a0 ; Descending write

		move.l	ScrambledTex(pc),a1
		move.w	TexOffset(pc),d0
		lea	(a1,d0.w),a1	; offset texture

		; Alternate scrambled versions
		move.l	#SCRAMBLED_TEX_SIZE,d0
		lea	(a1,d0.l),a2
		lea	(a2,d0.l),a3
		lea	(a3,d0.l),a4

		move.l	TableCode(pc),a5
		jsr	(a5)

.bltq:		tst.l	BlitNext
		bne	.bltq
		WAIT_BLIT

		bsr	NextOffset
		bsr	SwapBuffers
		bsr	PokeCopper

		jsr	VSyncWithBgTask

		jsr	PartOver
		blt	Mainloop
		rts


********************************************************************************
UvTbl_Vbi:
		lea	custom,a6
		bsr	SetPalette
		bsr	SetSprites
		bra	SetDiwH


********************************************************************************
SetSprites:
		move.l	TextSprite(pc),d0
		beq	SetNullSprites

		move.l	d0,a0
		move.w	TextX(pc),d0
		move.w	TextY(pc),d1
		bra	SetTextSprite


********************************************************************************
SetNullSprites:
		lea	BlankData,a0
		lea	CopSprPt+2,a1
		move.l	a0,d0
		swap	d0
		moveq	#8-1,d7
.l:
		move.w	d0,(a1)
		move.w	a0,4(a1)
		lea	8(a1),a1
		dbf	d7,.l
		rts


********************************************************************************
; a0 - sprite
; d0 - x
; d1 - y
;-------------------------------------------------------------------------------
SetTextSprite:
		; Vertical pos is same for all sprites
		move.w	d1,d2
		add.w	#SPRITE_H,d2	; d2 = vstop lower
		move.w	d2,d3
		lsr.w	#7,d3		; d3 = vstop upper
		and.w	#2,d3
		move.w	d1,d4		; d4 = vstart upper
		lsr.w	#6,d4
		and.w	#4,d4
		or.b	d3,d4		; d4 = vstart upper | vstop upper

		lea	CopSprPt+2,a1
		move.l	a0,a3
		moveq	#8-1,d7
.l:
		; Get next sprite
		move.w	(a3)+,d3
		lea	(a0,d3),a2

		; Set sprite ptrs in copper
		move.l	a2,d3
		swap	d3
		move.w	d3,(a1)
		move.w	a2,4(a1)

		; Set control words
		move.b	d1,(a2)+	; vstart
		move.w	d0,d3
		lsr.w	d3
		move.b	d3,(a2)+	; hstart upper
		move.b	d2,(a2)+	; vstop lower
		move.w	d0,d3		; hstart lower
		and.b	#1,d3
		or.b	d4,d3
		move.b	d3,(a2)+

		add.w	#16,d0		; inc x
		lea	8(a1),a1	; next ptr
		dbf	d7,.l
		rts


********************************************************************************
SetDiwH:
		move.w	CropH(pc),d1
		lsr.w	d1
		move.b	#DIW_YSTRT+SCREEN_H/2,d0
		sub.b	d1,d0
		move.b	d0,CopDiwStrt
		move.b	#DIW_YSTRT+SCREEN_H/2,d0
		add.b	d1,d0
		move.b	d0,CopDiwStop
		rts


********************************************************************************
; Scramble pixels in texture and repeat
;-------------------------------------------------------------------------------
ScrambleTexture:
		move.l	#SCRAMBLED_TEX_SIZE,d0
		move.l	ScrambledTex(pc),a0 ; low:	__ __ a3 a2 __ __ a1 a0
		lea	(a0,d0.l),a1	; high:		a3 a2 __ __ a1 a0 __ __
		lea	(a1,d0.l),a2	; dupe:		a3 a2 a3 a2 a1 a0 a1 a0       Where b = a
		lea	(a2,d0.l),a3	; seq: 		a3 a2 b3 b2 a1 a0 b1 b0       Where b = a+1

		; Texture is repeated for shift
		move.l	#SCRAMBLED_TEX_SIZE/2,d2 ; repeat offset

		moveq	#0,d3		; init prev value
		lea	TextureSrc,a4
		move.w	#TEX_SIZE-1,d7
.scramble:
		move.b	(a4)+,d0	; ....3210
		move.b	d0,d1
		and.b	#%11,d0		; ......10
		and.b	#%1100,d1	; ....32..
		lsl.b	#2,d1		; ..32....
		or.b	d1,d0		; ..32..10
		move.b	d0,(a1,d2.l)
		move.b	d0,(a1)+
		move.b	d0,d1

		move.b	d3,d4		; get prev
		move.b	d0,d3		; update prev

		lsl.b	#2,d0		; 32..10..

		or.b	d0,d4		; combine prev / current
		move.b	d4,(a3,d2.l)
		move.b	d4,(a3)+

		move.b	d0,(a0,d2.l)
		move.b	d0,(a0)+
		or.b	d1,d0		; 32321010
		move.b	d0,(a2,d2.l)
		move.b	d0,(a2)+

		; write next byte - value not important
		move.b	d0,(a0)+
		move.b	d0,(a1)+
		move.b	d0,(a2)+
		move.b	d0,(a3)+

		dbf	d7,.scramble
		rts


********************************************************************************
SwapBuffers:
		movem.l	DblBuffers(pc),d0-d3
		exg	d0,d1		; view/draw screen
		exg	d2,d3		; view/draw chunky
		movem.l	d0-d3,DblBuffers
		rts

PokeCopper:
		move.l	ViewScreen(pc),d1
		; Adjust for crop
		move.w	#SCREEN_H,d2
		sub.w	CropH(pc),d2
		lsr.w	#2,d2
		mulu	#SCREEN_BW,d2
		add.l	d2,d1

		; Set bitplane ptrs
		lea	CopBplpts,a0
		moveq	#4-1,d0		; bpls 0-3
.l:		SET_COP_PTR d1,a0
		add.l	#SCREEN_SIZE,d1
		dbf	d0,.l

		move.l	#Circle,d1
		; TODO: Adjust for crop
		SET_COP_PTR d1,a0	; bpl 4
		rts


********************************************************************************
SetPalette:
		lea	PalPtrs,a0
		move.w	FadePos(pc),d0
		lsl.w	#2,d0
		; Write first 16 directly to color regs
		move.l	(a0,d0.w),a0
		lea	color(a6),a1
		moveq	#16/2-1,d7
.l:		move.l	(a0)+,(a1)+
		dbf	d7,.l
		; Upper 16 need to go in copper because we switch between sprite and fg cols
		lea	CopPal+2,a1
		moveq	#16-1,d7
.l1:		move.w	(a0)+,(a1)
		lea	4(a1),a1
		dbf	d7,.l1
		rts


********************************************************************************
StartBlit:
		WAIT_BLIT
********************************************************************************
BlitSwap1:
		move.l	#BlitSwap2,BlitNext
		move.w	#BLITPASS_SIZE-1024,BlitWords
		move.w	#2,bltamod(a6)
		move.w	#2,bltbmod(a6)
		move.w	#%1111000011110000,bltcdat(a6)
		; D = (A & C) | ((B >> 4) & ~C)
		move.l	#($de4<<16)!(4<<12),bltcon0(a6)
		move.l	ViewChunky(pc),a0
		move.l	a0,bltapth(a6)	; A = chunky buffer
		addq	#2,a0
		move.l	a0,bltbpth(a6)	; B = A+2 i.e. next word
		move.l	DrawScreen(pc),a0 ; D = bpl0
		lea	SCREEN_SIZE(a0),a0
		move.l	a0,bltdpth(a6)
		move.w	#1,bltsize(a6)
		rts

********************************************************************************
BlitSwap2:
		move.l	#BlitExtend1,BlitNext
		move.w	#BLITPASS_SIZE-1024,BlitWords
		; D = ((A<<4) & C) | (B & ~C)
		move.l	#($4de4<<16)!BLITREVERSE,bltcon0(a6)
		move.l	ViewChunky(pc),a0
		lea	CHUNKY_SIZE-4(a0),a0 ; A = chunky buffer (end for desc)
		move.l	a0,bltapth(a6)
		addq	#2,a0
		move.l	a0,bltbpth(a6)	; B = A+2 i.e. prev word in desc
		move.l	DrawScreen(pc),a0
		lea	SCREEN_SIZE-2(a0),a0 ; D = bpl2 (end for desc)
		move.l	a0,bltdpth(a6)
		move.w	#1,bltsize(a6)
		rts


********************************************************************************
; Copy to bpls 1/3 and apply pixel doubling
;-------------------------------------------------------------------------------
BlitExtend1:
		move.l	#BlitExtend2,BlitNext
		clr.l	bltcmod(a6)
		clr.l	bltamod(a6)
		move.w	#%1010101010101010,bltcdat(a6)
		; D = (A & C) | ((B >> 1) & ~C)
		move.l	#($de4<<16)!(1<<12),bltcon0(a6)
		move.l	DrawScreen(pc),a0
		move.l	a0,bltapth(a6)	; A = bpl0/bpl2
		move.l	a0,bltbpth(a6)	; B = A
		lea	SCREEN_SIZE*2(a0),a0
		move.l	a0,bltdpth(a6)	; D = bpl1/bpl3
		move.w	#(SCREEN_H<<6)!(SCREEN_BW/2),bltsize(a6)
		rts


********************************************************************************
; Apply pixel doubling in place on bpls 0/2
;-------------------------------------------------------------------------------
BlitExtend2:
		clr.l	BlitNext
		; D = ((A << 1) & C) | (B & ~C)
		move.l	#($1de4<<16)!BLITREVERSE,bltcon0(a6)
		move.l	DrawScreen(pc),a0
		lea	SCREEN_SIZE*2-2(a0),a0
		move.l	a0,bltapth(a6)	; A = bpl0/bpl2 (end for desc)
		move.l	a0,bltbpth(a6)	; B = A
		move.l	a0,bltdpth(a6)	; D = A
		move.w	#(SCREEN_H<<6)!(SCREEN_BW/2),bltsize(a6)
		rts


********************************************************************************
GenerateTable:
		lea	Deltas,a0
		move.l	XOffsets(pc),a1
		move.l	YOffsets(pc),a2

		; Center ptr in tables
		lea	CHUNKY_W/2+(CHUNKY_H/2)*CHUNKY_W(a1),a1 ; xOffsetsAsc
		lea	CHUNKY_W/2+(CHUNKY_H/2)*CHUNKY_W(a2),a2 ; yOffsets base
		move.l	a1,a3		; xOffsetsDesc

		move.w	#$7f,d2

;-------------------------------------------------------------------------------
		moveq	#0,d7		; y
.yloop:
		move.b	#TEX_W/2,d0	; positive value (incremented by delta)
		move.b	d0,d1		; negative value (deccremented by delta)

		move.l	a2,a4		; yOffsetsAsc
		move.l	a2,a5		; yOffsetsDesc

		;---------------------------------------------------------------
		moveq	#0,d6		; x
.xloop:
		add.b	(a0),d0		; add delta
		sub.b	(a0)+,d1	; sub delta
		and.b	d2,d0
		and.b	d2,d1

; Write xOffsets:
		; table is square, but screen is not - don't write for rows out of range
		cmp.w	#CHUNKY_H/2,d7
		bgt	.noX
		move.b	d0,(a1,d6.w)	; pos asc
		move.b	d0,(a3,d6.w)	; pos desc
		neg.w	d6
		move.b	d1,-1(a1,d6.w)	; neg asc
		move.b	d1,-1(a3,d6.w)	; neg desc
		neg.w	d6
.noX:

; Write yOffsets:
		cmp.w	#CHUNKY_H/2,d6
		bgt	.noY
		move.b	d0,(a4,d7.w)
		move.b	d1,(a5,d7.w)
		neg.w	d7
		move.b	d0,-1(a4,d7.w)
		move.b	d1,-1(a5,d7.w)
		neg.w	d7
.noY:

		; increment row on yOffsets
		lea	CHUNKY_W(a4),a4
		lea	-CHUNKY_W(a5),a5

		addq	#1,d6
		cmp.w	#CHUNKY_W/2,d6
		blt	.xloop
		;---------------------------------------------------------------

		; increment row on xOffsets
		lea	CHUNKY_W(a1),a1
		lea	-CHUNKY_W(a3),a3

		addq	#1,d7
		cmp.w	#CHUNKY_W/2,d7
		blt	.yloop
;-------------------------------------------------------------------------------
		rts


********************************************************************************
; Generate table speed code from offset delta tables
;-------------------------------------------------------------------------------
GenerateTableCode:
		move.l	YOffsets(pc),a0
		move.l	XOffsets(pc),a1
		move.l	TableCode(pc),a2

		move.w	#(CHUNKY_W*CHUNKY_H)/8-1,d7
.l0:
		; Apply deltas to get 8 pixel offsets and write to temporary buffer:
		lea	Tmp,a3
		moveq	#8-1,d2
.deltaPair:
		; Write to tmp buffer:
		; y is already shifted << 8 by virtue of being in its own byte,
		; so leave as-is for 128*2 (doubled for word offset)
		move.b	(a0)+,(a3)+
		; add x delta twice to double
		move.b	(a1)+,d1
		add.b	d1,d1
		move.b	d1,(a3)+
		dbf	d2,.deltaPair

		; Get data reg number from counter and shift << 9 to merge with op codes
		move.w	d7,d2
		add.w	d2,d2		; (i * 2 + 1) & 7
		addq	#1,d2
		and.w	#7,d2		; d2 = d reg index << 9
		ror.w	#7,d2

; Macro to process 4 px from buffer
.group		macro
		; Grab four pixel offsets from buffer in scrambled order
		move.w	Tmp+(0+\1)*2(pc),d3
		move.w	Tmp+(1+\1)*2(pc),d4
		move.w	Tmp+(4+\1)*2(pc),d5
		move.w	Tmp+(5+\1)*2(pc),d6

; Upper nibble:
		and.w	#$0f00,d2
		; Optimiation: Duplicate offsets?
		cmp.w	d5,d6
		bne	.noDupeUpper\@
		add.w	#$302b,d2	; 3029 move.w a3
		move.w	d2,(a2)+
		move.w	d6,(a2)+
		bra	.endUpper\@
.noDupeUpper\@:
		; Optimiation: Sequential offsets?
		move.w	d6,d0
		sub.w	d5,d0
		subq	#2,d0
		bne	.noSeqUpper\@
		add.w	#$302c,d2	; 3029 move.w a4
		move.w	d2,(a2)+
		move.w	d6,(a2)+
		bra	.endUpper\@
.noSeqUpper\@:
		; Default: OR two values
		add.w	#$3029,d2	; 3029 move.w a1
		move.w	d2,(a2)+
		move.w	d6,(a2)+
		add.w	#$806a-$3029,d2	; 806a or.w a2
		move.w	d2,(a2)+
		move.w	d5,(a2)+
.endUpper\@:

; Lower nibble
		and.w	#$0f00,d2
		; Optimiation: Duplicate offsets?
		cmp.w	d3,d4
		bne	.noDupeLower\@
		add.w	#$102b,d2	; 102b move.b a3
		move.w	d2,(a2)+
		move.w	d4,(a2)+
		bra	.endLower\@
.noDupeLower\@:
		; Optimiation: Sequential offsets?
		move.w	d4,d0
		sub.w	d3,d0
		subq	#2,d0
		bne	.noSeqLower\@
		add.w	#$102c,d2	; 1029 move.b a4
		move.w	d2,(a2)+
		move.w	d4,(a2)+
		bra	.endLower\@
.noSeqLower\@:
		; Default: OR two values
		add.w	#$1029,d2	; 1029 move.b a1
		move.w	d2,(a2)+
		move.w	d4,(a2)+
		sub.w	#$1029-$802a,d2	; 802a or.b a2
		move.w	d2,(a2)+
		move.w	d3,(a2)+
.endLower\@:
		endm

		.group 0
		sub.w	#1<<9,d2	; Decrement data reg number
		.group 2

		; Insert movem after last register
		and.w	#$0f00,d2
		bne	.next
		move.l	#$48a0ff00,(a2)+ ; movem.w	d0-d7,-(a0)
.next:
		dbf	d7,.l0
		move.w	#$4e75,(a2)+	; rts
		rts

Tmp:		ds.w	8


********************************************************************************
; Sets TexOffset for frame
;-------------------------------------------------------------------------------
NextOffset:
		lea	Vars(pc),a5
		move.w	BpmFrame,d7
		lsl.w	#3,d7

		move.w	LocalFrame+2,d6
		move.w	LastUpdate,d5
		move.w	d6,LastUpdate
		sub.w	d5,d6		; frame delta
		subq	#1,d6

		lea	Sin,a0
; X1 (slow)
		move.w	d7,d0
		lsr.w	#3,d0		; speed
		and.w	#SIN_MASK,d0
		add.w	d0,d0
		move.w	(a0,d0.w),d0
		ext.l	d0
		lsl.l	#3,d0		; amp
; X2
		move.w	d7,d1
		lsr.w	#2,d0		; speed
		and.w	#SIN_MASK,d1
		add.w	d1,d1
		move.w	(a0,d1.w),d1
		ext.l	d1
		lsl.l	#1,d1		; amp
		add.l	d1,d0
; X2
		move.w	d7,d1
		and.w	#SIN_MASK,d1
		add.w	d1,d1
		move.w	(a0,d1.w),d1
		ext.l	d1
		lsl.l	#3,d1		; amp
		add.l	d1,d0

		; Value is added to current pos (i.e. velocity)
		lsl.l	d6,d0
		add.l	d0,XPos-Vars(a5)

		lea	Cos,a0
; Y1 (slow)
		move.w	d7,d0
		lsr	#2,d0		; speed
		and.w	#SIN_MASK,d0
		add.w	d0,d0
		move.w	(a0,d0.w),d0
		ext.l	d0
		lsl.l	#3,d0		; amp
; Y2
		move.w	d7,d1
		and.w	#SIN_MASK,d1
		add.w	d1,d1
		move.w	(a0,d1.w),d1
		ext.l	d1
		lsl.l	#3,d1		; amp
		add.l	d1,d0
		lsl.l	d6,d0
		add.l	d0,YPos-Vars(a5)

; Position to texture offset
		move.w	XPos(pc),d0
		move.w	YPos(pc),d1
		lsl.w	#7,d1
		add.w	d1,d0
		and.w	#TEX_SIZE-1,d0
		add.w	d0,d0
		move.w	d0,TexOffset
		rts


********************************************************************************
Vars:
********************************************************************************

LastUpdate:	dc.w	0
TexOffset:	dc.w	0
FadePos:	dc.w	0

XPos:		dc.l	0
YPos:		dc.l	0

DblBuffers:
DrawScreen:	dc.l	0
ViewScreen:	dc.l	0
DrawChunky:	dc.l	0
ViewChunky:	dc.l	0

ScrambledTex:	dc.l	0
TableCode:	dc.l	0

PalData:	dc.l	0

; Blitter interrupt chaining:
; Ptr to routine for next blit operation, or 0.
BlitNext:	dc.l	0

; Number of words remaining for current blit operation.
; For swap operations we exceed the maximum blit size of 1024*1, so need to
; continue by poking bltsize again until complete.
BlitWords:	dc.w	0

CropH:		dc.w	SCREEN_H

TextSprite:	dc.l	0
TextX:		dc.w	0
TextY:		dc.w	0

XOffsets:	dc.l	0
YOffsets:	dc.l	0


********************************************************************************
		data
********************************************************************************

PalPtrs:	ds.l	16

TextureSrc:	incbin	"data/texture.chk"

TexturePal:
		incbin	"data/texture.pal"
		incbin	"data/texture-shift.pal"

		; Deltas for UV table
		include	"data/table.i"


********************************************************************************
		data_c
********************************************************************************

Circle:		incbin	"data/circle.bpl"

Cred1:		incbin	"data/cred-1.SPR"
Cred2:		incbin	"data/cred-2.SPR"
Cred3:		incbin	"data/cred-3.SPR"
		; incbin	data/font-8.BPL
		; incbin	data/font-16x16.BPL

********************************************************************************
Cop:
		dc.w	bplcon0,(4<<12)!$200
		dc.w	bplcon1,0
		dc.w	diwstrt
CopDiwStrt:	dc.w	(DIW_YSTRT<<8)!DIW_XSTRT
		dc.w	diwstop
CopDiwStop:	dc.w	(((DIW_YSTRT+SCREEN_H)<<8)&$ff00)!((DIW_XSTRT+SCREEN_W)&$ff)
		dc.w	ddfstrt,DIW_XSTRT/2-8
		dc.w	ddfstop,DIW_XSTRT/2-8+8*((SCREEN_W+15)/16-1)
CopSprPt:
		rept	8*2
		dc.w	sprpt+REPTN*2,0
		endr
		; Sprite cols
		dc.w	color17,$fff
		dc.w	color19,$000
		dc.w	color21,$fff
		dc.w	color23,$000
		dc.w	color25,$fff
		dc.w	color27,$000
		dc.w	color29,$fff
		dc.w	color31,$000
; Bitplane pointers:
CopBplpts:
		dc.w	bpl0pt,0
		dc.w	bpl0ptl,0
		dc.w	bpl2pt,0
		dc.w	bpl2ptl,0
		dc.w	bpl1pt,0
		dc.w	bpl1ptl,0
		dc.w	bpl3pt,0
		dc.w	bpl3ptl,0
		dc.w	bpl4pt,0
		dc.w	bpl4ptl,0

CopY		set	DIW_YSTRT-1

		macro	DUPE_LINE
		COP_WAITH CopY,$df
		dc.w	bpl1mod,-SCREEN_BW
		dc.w	bpl2mod,-SCREEN_BW
		COP_WAIT CopY+1,$df
; move to next scanline
		dc.w	bpl1mod,0
		dc.w	bpl2mod,0
CopY		set	CopY+2
		endm

		; Write lines until start of circle
		rept	CIRC_Y/2
		DUPE_LINE
		endr

		dc.w	bplcon0,(5<<12)!$200 ; enable additional bpl
		; Switch from sprite cols to shift cols
CopPal:
		rept	16
		dc.w	color16+REPTN*2,0
		endr

		; Circle lines
		rept	CIRC_H/2
		DUPE_LINE
		endr
		dc.w	bplcon0,(4<<12)!$200 ; disable additional bpl

		; Switch back to sprite cols
		dc.w	color17,$fff
		dc.w	color19,$000
		dc.w	color21,$fff
		dc.w	color23,$000
		dc.w	color25,$fff
		dc.w	color27,$000
		dc.w	color29,$fff
		dc.w	color31,$000

		; Write remaining lines
		rept	(SCREEN_H-CIRC_Y-CIRC_H)/2
		DUPE_LINE
		endr


		COP_END
