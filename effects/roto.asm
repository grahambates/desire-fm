		incdir	..
		include	_main.i
		include	effects/roto.i

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

SCREEN_W = 256
SCREEN_H = 200				; Can go up to 210, but need room for next part precalc
SCREEN_BW = ((SCREEN_W+15)/16)*2
SCREEN_BPL = (SCREEN_H/2)*SCREEN_BW	; half height because of line doubling
SCREEN_SIZE = SCREEN_BPL*BPLS

BPLS = 4

CHUNKY_W = SCREEN_W/2
CHUNKY_H = SCREEN_H/2
CHUNKY_SIZE = SCREEN_BPL*2

DIW_XSTRT = $81+(320-SCREEN_W)/2
DIW_YSTRT = $2c+(256-SCREEN_H)/2

BLITPASS_SIZE = SCREEN_BPL/2

TEX_W = 128
TEX_H = 128
TEX_SIZE = TEX_W*TEX_H

SCRAMBLED_TEX_SIZE = TEX_SIZE*4		; word per byte and repeated for scroll

DIST = 800
MAX_SCALE = $50000

Script:
		dc.l	0,CmdLerpWord,15,7,FadePos
		dc.l	$10,CmdLerpWord,$8,5,ScrollAmp
		dc.l	$10,CmdLerpWord,$80,7,ScaleAmp
		dc.l	64*3000/BPM-1<<6,CmdLerpWord,31,6,FadePos

		dc.l	$1000


********************************************************************************
Roto_BlitInt:
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
Roto_Pre:
********************************************************************************
		jsr	MemFlip

		ALLOC_CHIP SCRAMBLED_TEX_SIZE*2
		move.l	a0,ScrambledTex
		bsr	ScrambleTexture

		ALLOC_PUBLIC 28*16*2
		move.l	a0,PalData

		PRINT_MEM_TOTALS

		lea	BlankData,a0
		lea	TexturePal,a1
		move.l	PalData(pc),a2
		lea	PalPtrs,a3
		moveq	#16-1,d0
		moveq	#16-1,d1
		jsr	PreLerpPal

		lea	TexturePal,a0
		lea	FillData,a1
		moveq	#16-1,d0
		moveq	#16-1,d1
		jsr	PreLerpPal
		rts


********************************************************************************
Roto_Effect:
********************************************************************************
		jsr	SetBgTask

		jsr	MemFreeLast

		ALLOC_CHIP SCREEN_SIZE
		move.l	a0,DrawScreen
		ALLOC_CHIP SCREEN_SIZE
		move.l	a0,ViewScreen

		ALLOC_CHIP CHUNKY_SIZE
		move.l	a0,DrawChunky
		ALLOC_CHIP CHUNKY_SIZE
		move.l	a0,ViewChunky

		PRINT_MEM_TOTALS

		bsr	InitBlitter

		move.l	#Cop,cop1lc(a6)
		move.l	#Roto_BlitInt,BlitterInterrupt

		move.w	#INTF_SETCLR!INTF_BLIT,intena(a6)

		lea	Script,a0
		jsr	Commander_Init

		lea	Vars,a5

********************************************************************************
Mainloop:

		; Start blitter swap operations. These will chain together with BlitNext
		bsr	StartBlit

		bsr	SetPalette
		bsr	UpdateVars
		bsr	UpdateOffsets

		bsr	WriteOffsets

		move.l	DrawChunky(pc),a0
		lea	CHUNKY_SIZE(a0),a0 ; Descending write
		move.l	ScrambledTex,a1
		move.w	TexOffset(pc),d0
		and.w	#CHUNKY_W-1,d0
		add.w	d0,d0
		lea	(a1,d0.w),a1

		jsr	RotCode
		lea	Vars,a5
		lea	custom,a6

.bltq:		tst.l	BlitNext
		bne	.bltq

		bsr	SwapBuffers
		WAIT_BLIT

		jsr	VSyncWithBgTask

		jsr	PartOver
		blt	Mainloop
		rts



********************************************************************************
; Scramble pixels in texture and repeat
;-------------------------------------------------------------------------------
ScrambleTexture:
		move.l	#SCRAMBLED_TEX_SIZE,d0
		move.l	ScrambledTex(pc),a0 ; low:	__ __ a3 a2 __ __ a1 a0
		lea	(a0,d0.l),a1	; high:		a3 a2 __ __ a1 a0 __ __

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
		lsl.b	#2,d0		; 32..10..
		move.b	d0,(a0,d2.l)
		move.b	d0,(a0)+

; write next byte - value not important
		move.b	d0,(a0)+
		move.b	d0,(a1)+

		dbf	d7,.scramble
		rts


********************************************************************************
SetPalette:
		lea	PalPtrs,a0
		move.w	FadePos(pc),d0
		lsl.w	#2,d0
		move.l	(a0,d0.w),a0
		lea	color(a6),a1
		moveq	#32/2-1,d7
.l:		move.l	(a0)+,(a1)+
		dbf	d7,.l
		rts


********************************************************************************
UpdateVars:
		move.w	BpmFrame,d7
		add.w	9*3000/BPM,d7

		lsl.w	#2,d7
		lea	Sin,a0

		; Angle
		move.w	d7,d0		; d0 = a
		and.w	#SIN_MASK,d0
		add.w	d0,d0
		move.w	(a0,d0.w),d0
		asr.w	#7,d0
		asr.w	#2,d7
		add.w	d7,d0
		muls	AngleAmp(pc),d0
		asr.l	#8,d0
		move.w	d0,Angle-Vars(a5)

		; Scale
		move.w	d7,d0
		and.w	#SIN_MASK*2,d0
		move.w	(a0,d0.w),d0
		asr.w	#2,d0

		move.w	d7,d1
		mulu	#3,d1
		and.w	#SIN_MASK,d1
		add.w	d1,d1
		move.w	(a0,d1.w),d1
		asr.w	#2,d1
		add.w	#$1000,d0	; 0 - $1000

		add.w	d1,d0
		muls	ScaleAmp(pc),d0
		asr.l	#8,d0

		add.w	#DIST+$1000,d0	; 0 - $2000

		move.l	#(DIST+$1000)*64,d1
		divs	d0,d1
		bgt	.ok
		move.w	#1,d1
.ok:
		move.w	d1,Scale-Vars(a5)

		; Tex Offset
		move.w	ScrollAmp,d0
		add.w	d0,TexOffset

		rts


********************************************************************************
; Write XOffsets,YOffsets and CenterOfs for current angle/scale
;-------------------------------------------------------------------------------
UpdateOffsets:
		; duCol = sin(a) / scale;
		; dvCol = cos(a) / scale;
		; duRow = dvCol;
		; dvRow = -duCol;

		move.w	Angle(pc),d0
		and.w	#SIN_MASK,d0
		add.w	d0,d0
		lea	Cos,a0
		move.w	(a0,d0.w),d1	; d1 = cos(a) FP 1:14
		lea	Sin,a0
		move.w	(a0,d0.w),d0	; d0 = sin(a) PF 1:14

		move.w	Scale(pc),d2
		; /64 = 1 at FP 7:9
		ext.l	d0
		add.l	d0,d0
		divs	d2,d0		; d0 = duCol 7:9
		ext.l	d1
		add.l	d1,d1
		divs	d2,d1		; d1 = dvCol 7:9

		; Center offset:
		; u = TEX_W/2 - (CHUNKY_W/2 * dvCol + CHUNKY_H/2 * duCol);
		; v = TEX_H/2 - (CHUNKY_W/2 * dvRow + CHUNKY_H/2 * duRow);

		move.w	#CHUNKY_W/2,d2
		muls	d1,d2		; CHUNKY_W/2 * dvCol
		move.w	#CHUNKY_H/2,d3
		muls	d0,d3		; CHUNKY_H/2 * duCol
		add.w	d3,d2
		neg.w	d2		; -(CHUNKY_W/2 * dvCol + CHUNKY_H/2 * duCol)
		add.w	#(TEX_W/2)<<9,d2 ; u = TEX_W/2 - (CHUNKY_W/2 * dvCol + CHUNKY_H/2 * duCol);

		; dvRow = -duCol
		move.w	#CHUNKY_W/2,d3
		muls	d0,d3		; CHUNKY_W/2 * dvRow
		neg.w	d3
		; duRow = dvCol
		move.w	#CHUNKY_H/2,d4
		muls	d1,d4		; CHUNKY_H/2 * duRow
		add.w	d4,d3
		neg.w	d3
		add.w	#(TEX_H/2)<<9,d3 ; v = TEX_H/2 - (CHUNKY_W/2 * dvRow + CHUNKY_H/2 * duRow);

		lsr.w	d3
		lsr.w	#8,d2
		move.b	d2,d3
		and.w	#$7ffe,d3

		move.w	d3,CenterOfs

		lea	XOffsets,a0
		lea	YOffsets,a1

		; start values
		move.w	#0,d2		; u
		move.w	#0,d3		; v

		neg.w	d0		; dvRow = -duCol
		lsr.w	d0
		lsr.w	d1
		move.w	#$7fff,d5

UNROLL = 4

		move.w	#CHUNKY_W/UNROLL-1,d7
.l:
		rept	UNROLL
		; increment
		add.w	d0,d2		; duCol
		and.w	d5,d2		; duCol&$7fff
		add.w	d1,d3		; dvCol

		; Set upper byte for y using FP value
		; y*TEX_W*2 (can use 8:8 value, *2 is for word offset)
		; lower byte will be overwritten with x value
		move.w	d2,(a0)+	; dvRow -> XOffsets upper byte
		move.w	d3,(a1)+	; dvCol -> YOffsets upper byte

		; Read value back out to get shifted >> 8
		move.b	-2(a1),d4
		add.b	d4,d4		; *2 for word offset
		move.b	d4,-1(a0)	; dvCol -> XOffsets lower byte

		move.b	-2(a0),d4
		add.b	d4,d4		; *2 for word offset
		neg.b	d4
		move.b	d4,-1(a1)	; -dvRow -> YOffsets lower byte

		endr
		dbf	d7,.l

		rts


********************************************************************************
; Insert XOffsets values into SMC, including scrambled order
;-------------------------------------------------------------------------------
WriteOffsets:
		lea	RotCodeInner+2,a0
		lea	XOffsets,a1
		move.w	#4-1,d7
.l0:
		move.w	#4-1,d6
.l1:
		move.w	(a1)+,12(a0)
		move.w	(a1)+,8(a0)
		move.w	(a1)+,28(a0)
		move.w	(a1)+,24(a0)
		move.w	(a1)+,4(a0)
		move.w	(a1)+,(a0)
		move.w	(a1)+,20(a0)
		move.w	(a1)+,16(a0)
		lea	32(a0),a0
		dbf	d6,.l1
		lea	4(a0),a0
		dbf	d7,.l0
		rts


********************************************************************************
SwapBuffers:
		movem.l	DblBuffers(pc),d0-d3
		exg	d0,d1		; view/draw screen
		exg	d2,d3		; view/draw chunky
		movem.l	d0-d3,DblBuffers

		; Set bitplane ptrs
		lea	CopBplpts,a0
		moveq	#4-1,d0		; bpls 0-3
.l:		SET_COP_PTR d1,a0
		add.l	#SCREEN_BPL,d1
		dbf	d0,.l
		rts


********************************************************************************
InitBlitter:
		WAIT_BLIT
		move.l	#-1,bltafwm(a6)
		clr.w	bltdmod(a6)
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
		lea	SCREEN_BPL(a0),a0
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
		lea	SCREEN_BPL-2(a0),a0 ; D = bpl2 (end for desc)
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
		lea	SCREEN_BPL*2(a0),a0
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
		lea	SCREEN_BPL*2-2(a0),a0
		move.l	a0,bltapth(a6)	; A = bpl0/bpl2 (end for desc)
		move.l	a0,bltbpth(a6)	; B = A
		move.l	a0,bltdpth(a6)	; D = A
		move.w	#(SCREEN_H<<6)!(SCREEN_BW/2),bltsize(a6)
		rts


********************************************************************************
Vars:
********************************************************************************

TexOffset:	dc.w	0
Angle:		dc.w	0
Scale:		dc.w	64
CenterOfs:	dc.w	0
FadePos:	dc.w	0

AngleAmp:	dc.w	$300
ScaleAmp:	dc.w	0
ScrollAmp:	dc.w	0

SpTmp:		dc.l	0

DblBuffers:
DrawScreen:	dc.l	0
ViewScreen:	dc.l	0
DrawChunky:	dc.l	0
ViewChunky:	dc.l	0

; Blitter interrupt chaining:
; Ptr to routine for next blit operation, or 0.
BlitNext:	dc.l	0

; Number of words remaining for current blit operation.
; For swap operations we exceed the maximum blit size of 1024*1, so need to
; continue by poking bltsize again until complete.
BlitWords:	dc.w	0

ScrambledTex:	dc.l	0

PalData:	dc.l	0


********************************************************************************
		code_c
********************************************************************************

; TODO:
; moving this (and tex) to chip makes this part work on A4000
; is this just because it slows it down though?

********************************************************************************
; Routine to write to chunky buffer
; Inner loop is SMC
;-------------------------------------------------------------------------------
; a0 - Chunky buffer
; a1 - TexLo inc offset
;-------------------------------------------------------------------------------
RotCode:
		move.l	a7,SpTmp	; free another reg
		move.l	a1,a3		; store original tex offset
		move.w	CenterOfs,a4
		move.l	#SCRAMBLED_TEX_SIZE,a7
		lea	YOffsets,a5
		move.w	#CHUNKY_H-1,d7
RotCodeL:
		move.w	d7,a6		; move iterator to free up data reg
		move.w	(a5)+,d0	; next y offset
		add.w	a4,d0		; add CenterOfs
		and.w	#$7ffe,d0	; mod to tex size
		lea	(a3,d0.w),a1	; offset TexLo
		lea	(a1,a7.l),a2	; offset TexHi
RotCodeInner:
__SMC__ = 1
		move.w	__SMC__(a1),d7
		or.w	__SMC__(a2),d7
		move.b	__SMC__(a1),d7
		or.b	__SMC__(a2),d7
		move.w	__SMC__(a1),d6
		or.w	__SMC__(a2),d6
		move.b	__SMC__(a1),d6
		or.b	__SMC__(a2),d6
		move.w	__SMC__(a1),d5
		or.w	__SMC__(a2),d5
		move.b	__SMC__(a1),d5
		or.b	__SMC__(a2),d5
		move.w	__SMC__(a1),d4
		or.w	__SMC__(a2),d4
		move.b	__SMC__(a1),d4
		or.b	__SMC__(a2),d4
		move.w	__SMC__(a1),d3
		or.w	__SMC__(a2),d3
		move.b	__SMC__(a1),d3
		or.b	__SMC__(a2),d3
		move.w	__SMC__(a1),d2
		or.w	__SMC__(a2),d2
		move.b	__SMC__(a1),d2
		or.b	__SMC__(a2),d2
		move.w	__SMC__(a1),d1
		or.w	__SMC__(a2),d1
		move.b	__SMC__(a1),d1
		or.b	__SMC__(a2),d1
		move.w	__SMC__(a1),d0
		or.w	__SMC__(a2),d0
		move.b	__SMC__(a1),d0
		or.b	__SMC__(a2),d0
		movem.w	d0-d7,-(a0)
		move.w	__SMC__(a1),d7
		or.w	__SMC__(a2),d7
		move.b	__SMC__(a1),d7
		or.b	__SMC__(a2),d7
		move.w	__SMC__(a1),d6
		or.w	__SMC__(a2),d6
		move.b	__SMC__(a1),d6
		or.b	__SMC__(a2),d6
		move.w	__SMC__(a1),d5
		or.w	__SMC__(a2),d5
		move.b	__SMC__(a1),d5
		or.b	__SMC__(a2),d5
		move.w	__SMC__(a1),d4
		or.w	__SMC__(a2),d4
		move.b	__SMC__(a1),d4
		or.b	__SMC__(a2),d4
		move.w	__SMC__(a1),d3
		or.w	__SMC__(a2),d3
		move.b	__SMC__(a1),d3
		or.b	__SMC__(a2),d3
		move.w	__SMC__(a1),d2
		or.w	__SMC__(a2),d2
		move.b	__SMC__(a1),d2
		or.b	__SMC__(a2),d2
		move.w	__SMC__(a1),d1
		or.w	__SMC__(a2),d1
		move.b	__SMC__(a1),d1
		or.b	__SMC__(a2),d1
		move.w	__SMC__(a1),d0
		or.w	__SMC__(a2),d0
		move.b	__SMC__(a1),d0
		or.b	__SMC__(a2),d0
		movem.w	d0-d7,-(a0)
		move.w	__SMC__(a1),d7
		or.w	__SMC__(a2),d7
		move.b	__SMC__(a1),d7
		or.b	__SMC__(a2),d7
		move.w	__SMC__(a1),d6
		or.w	__SMC__(a2),d6
		move.b	__SMC__(a1),d6
		or.b	__SMC__(a2),d6
		move.w	__SMC__(a1),d5
		or.w	__SMC__(a2),d5
		move.b	__SMC__(a1),d5
		or.b	__SMC__(a2),d5
		move.w	__SMC__(a1),d4
		or.w	__SMC__(a2),d4
		move.b	__SMC__(a1),d4
		or.b	__SMC__(a2),d4
		move.w	__SMC__(a1),d3
		or.w	__SMC__(a2),d3
		move.b	__SMC__(a1),d3
		or.b	__SMC__(a2),d3
		move.w	__SMC__(a1),d2
		or.w	__SMC__(a2),d2
		move.b	__SMC__(a1),d2
		or.b	__SMC__(a2),d2
		move.w	__SMC__(a1),d1
		or.w	__SMC__(a2),d1
		move.b	__SMC__(a1),d1
		or.b	__SMC__(a2),d1
		move.w	__SMC__(a1),d0
		or.w	__SMC__(a2),d0
		move.b	__SMC__(a1),d0
		or.b	__SMC__(a2),d0
		movem.w	d0-d7,-(a0)
		move.w	__SMC__(a1),d7
		or.w	__SMC__(a2),d7
		move.b	__SMC__(a1),d7
		or.b	__SMC__(a2),d7
		move.w	__SMC__(a1),d6
		or.w	__SMC__(a2),d6
		move.b	__SMC__(a1),d6
		or.b	__SMC__(a2),d6
		move.w	__SMC__(a1),d5
		or.w	__SMC__(a2),d5
		move.b	__SMC__(a1),d5
		or.b	__SMC__(a2),d5
		move.w	__SMC__(a1),d4
		or.w	__SMC__(a2),d4
		move.b	__SMC__(a1),d4
		or.b	__SMC__(a2),d4
		move.w	__SMC__(a1),d3
		or.w	__SMC__(a2),d3
		move.b	__SMC__(a1),d3
		or.b	__SMC__(a2),d3
		move.w	__SMC__(a1),d2
		or.w	__SMC__(a2),d2
		move.b	__SMC__(a1),d2
		or.b	__SMC__(a2),d2
		move.w	__SMC__(a1),d1
		or.w	__SMC__(a2),d1
		move.b	__SMC__(a1),d1
		or.b	__SMC__(a2),d1
		move.w	__SMC__(a1),d0
		or.w	__SMC__(a2),d0
		move.b	__SMC__(a1),d0
		or.b	__SMC__(a2),d0
		movem.w	d0-d7,-(a0)

		move.w	a6,d7		; restore iterator
		dbf	d7,RotCodeL
		move.l	SpTmp,a7	; restore SP
		rts

PalPtrs:	ds.l	32


********************************************************************************
		data
********************************************************************************

TexturePal:
		incbin	"data/tapes.pal"
TextureSrc:
		incbin	"data/tapes.chk"

********************************************************************************
		data_c
********************************************************************************

********************************************************************************
Cop:
		dc.w	bplcon0,(BPLS<<12)!$200
		dc.w	bplcon1,0
		dc.w	diwstrt,(DIW_YSTRT<<8)!DIW_XSTRT
		dc.w	diwstop,(((DIW_YSTRT+SCREEN_H)<<8)&$ff00)!((DIW_XSTRT+SCREEN_W)&$ff)
		dc.w	ddfstrt,DIW_XSTRT/2-8
		dc.w	ddfstop,DIW_XSTRT/2-8+8*((SCREEN_W+15)/16-1)
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
; TODO: generate in code or write as loop
.y		set	DIW_YSTRT-1
		rept	SCREEN_H/2
		COP_WAITH .y,$df
		dc.w	bpl1mod,-SCREEN_BW
		dc.w	bpl2mod,-SCREEN_BW
		COP_WAIT .y+1,$df
; move to next scanline
		dc.w	bpl1mod,0
		dc.w	bpl2mod,0
.y		set	.y+2
		endr
		COP_END


********************************************************************************
		bss
********************************************************************************

YOffsets:	ds.w	CHUNKY_W
XOffsets:	ds.w	CHUNKY_W
