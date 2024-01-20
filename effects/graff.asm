		include	_main.i
		include	effects/graff.i

********************************************************************************
* Constants:
********************************************************************************

; Display window:
DIW_W = 320
DIW_H = 256
BPLS = 6
LAYER_BPLS = 2
SCROLL = 1				; enable playfield scroll
INTERLEAVED = 1
DPF = 1					; enable dual playfield

; Screen buffer:
SCREEN_W = DIW_W+16
SCREEN_H = DIW_H

STROKE_W = 48
STROKE_BW = STROKE_W/8
STROKE_H = 32
STROKE_SIZE = STROKE_BW*STROKE_H*LAYER_BPLS

QUEUE_SIZE = 1024*4


;-------------------------------------------------------------------------------
; Derived

COLORS = 1<<LAYER_BPLS

SCREEN_BW = SCREEN_W/16*2		; byte-width of 1 bitplane line
		ifne	INTERLEAVED
SCREEN_MOD = SCREEN_BW*(LAYER_BPLS-1)	; modulo (interleaved)
SCREEN_BPL = SCREEN_BW			; bitplane offset (interleaved)
		else
SCREEN_MOD = 0				; modulo (non-interleaved)
SCREEN_BPL = SCREEN_BW*SCREEN_H		; bitplane offset (non-interleaved)
		endc
LAYER_BUFFER_SIZE = SCREEN_BW*SCREEN_H*LAYER_BPLS*4
SCREEN_SIZE = SCREEN_BW*SCREEN_H*LAYER_BPLS

DIW_BW = DIW_W/16*2
DIW_MOD = SCREEN_BW-DIW_BW+SCREEN_MOD-SCROLL*2
DIW_SIZE = DIW_BW*DIW_H*LAYER_BPLS
DIW_XSTRT = ($242-DIW_W)/2
DIW_YSTRT = ($158-DIW_H)/2
DIW_XSTOP = DIW_XSTRT+DIW_W
DIW_YSTOP = DIW_YSTRT+DIW_H

Script:
		dc.l	0,CmdLerpPal,15,5,BlankData,Pal,PalOut
		dc.l	0,CmdLerpWord,244,6,SpriteTop
		dc.l	64*3000/BPM-1<<7,CmdLerpPal,15,7,Pal,BlankData,PalOut
		dc.l	$8000


********************************************************************************
Graff_Effect:
********************************************************************************
		; jsr	SetBgTask
		jsr	MemFlip
		jsr	MemFreeLast

		ALLOC_CHIP LAYER_BUFFER_SIZE,Layer1+Layer_Buffer
		bsr	Clear
		ALLOC_CHIP LAYER_BUFFER_SIZE,Layer2+Layer_Buffer
		move.l	a0,Layer3+Layer_Buffer
		bsr	Clear

		ALLOC_PUBLIC QUEUE_SIZE,Layer1+Layer_Queue
		ALLOC_PUBLIC QUEUE_SIZE,Layer2+Layer_Queue
		ALLOC_PUBLIC QUEUE_SIZE,Layer3+Layer_Queue

		ALLOC_CHIP SCREEN_SIZE,ClearScreen
		ALLOC_CHIP SCREEN_SIZE,DrawScreen
		bsr	ClearWaves
		ALLOC_CHIP SCREEN_SIZE,ViewScreen
		bsr	ClearWaves

		ALLOC_PUBLIC SIN_LEN*4,YTbl

		move.l	Layer1+Layer_Queue(pc),a0
		lea	Greets,a1
		bsr	BuildQueue

		move.l	Layer2+Layer_Queue(pc),a0
		lea	Greets2,a1
		bsr	BuildQueue

		move.l	Layer3+Layer_Queue(pc),a0
		lea	Greets3,a1
		bsr	BuildQueue

		bsr	InitTbl

		lea	Script,a0
		bsr	Commander_Init

		bsr	Scroll
		jsr	WaitEOF
		move.l	#Cop,cop1lc(a6)

		move.w	#DMAF_SETCLR!DMAF_SPRITE,dmacon(a6)

;-------------------------------------------------------------------------------
MainLoop:
		lea	Sprite,a0
		move.w	SpriteTop(pc),d0 ; start y
		moveq	#0,d1		; step
		move.w	BpmFrame,d2
		and.w	#$1f,d2
		bne	.noFlip		; because BpmFrame skip numbers, this gives some randomness
		neg.w	AltInc
.noFlip:
		cmp.w	#4,d2
		bge	.noAlt
		lea	SpriteAlt,a0
		move.w	AltInc(pc),d1	; step
		ble	.neg
		subq	#3,d0		; start y
.neg:
.noAlt:

		bsr	SetSprites
		bsr	SetPalette
		addq.w	#1,QueueStep

		lea	Layer1,a0
		bsr	ProcessLayer

		lea	Layer2,a0
		bsr	ProcessLayer

		lea	Layer3,a0
		bsr	ProcessLayer

		move.l	ClearScreen(pc),a0
		bsr	ClearWaves
		bsr	SetScrollPos
		bsr	DrawWaves

		; ; Make sure we don't update copper too soon
		; move.w	#$100,d0
		; jsr	WaitRaster

		; Swap buffers
		movem.l	ScreenBuffers(pc),d0-d2
		exg	d0,d2
		exg	d1,d2
		movem.l	d0-d2,ScreenBuffers

		bsr	Scroll


		WAIT_BLIT
		; move.w	#$f00,color00(a6)

		jsr	VSync
		jsr	PartOver
		blt	MainLoop
		rts


********************************************************************************
; Routines:
********************************************************************************

********************************************************************************
InitTbl:
		lea	Sin,a0
		move.l	YTbl(pc),a1
		move.w	#SIN_LEN-1,d7
.l:
		move.w	(a0)+,d1
		asr.w	#8,d1
		; asr.w	d1
		mulu	#SCREEN_BW*LAYER_BPLS,d1
		move.w	d1,SIN_LEN*2(a1)
		move.w	d1,(a1)+
		dbf	d7,.l

		rts


********************************************************************************
ClearWaves:
		WAIT_BLIT
		move.l	#$01000000,bltcon0(a6)
		clr.w	bltdmod(a6)
		move.l	a0,bltdpt(a6)
		move.w	#SCREEN_H*2*64+SCREEN_BW/2,bltsize(a6)
		rts


********************************************************************************
SetScrollPos:
		move.w	LocalFrame+2,d0
		move.w	d0,d1
		mulu	#200,d0
		lsr.l	#8,d0
		lea	Layer1,a2
		move.w	d0,Layer_Scroll(a2)

		lsr.w	#1,d1
		lea	Layer2,a2
		move.w	d1,Layer_Scroll(a2)
		rts

********************************************************************************
; d0 = layer 1 scroll
; d1 = layer 2 scroll
DrawWaves:
		moveq	#$f,d6		; d6 = layer 1 scroll adjust
		and.w	d0,d6
		moveq	#$f,d5		; d6 = layer 2 scroll adjust
		and.w	d1,d5
		move.l	DrawScreen(pc),a0
		lea	SCREEN_BW*150*2(a0),a0 ; layer 1 buffer
		lea	SCREEN_BW(a0),a1 ; layer 2 buffer

		; Sin 1
		move.l	YTbl(pc),a5
		move.w	LocalFrame+2,d0
		lsl.w	#2,d0
		and.w	#SIN_LEN-1,d0
		add.w	d0,d0
		lea	(a5,d0),a2
		; Sin 2
		and.w	#SIN_LEN-1,d0
		add.w	d0,d0
		lea	(a5,d0),a3
		; Sin 3
		move.w	BpmFrame,d0
		lsl.w	#2,d0
		and.w	#SIN_LEN-1,d0
		add.w	d0,d0
		lea	(a5,d0),a4
		; Sin 4
		and.w	#SIN_LEN-1,d0
		add.w	d0,d0
		lea	(a5,d0),a5

		move.w	#DIW_W-1,d7
.dot:
		move.w	d6,d0
		add.w	d7,d0
		move.w	d0,d1
		lsr.w	#3,d1
		not.w	d0

		move.w	(a2)+,d2
		add.w	d1,d2
		bset	d0,(a0,d2)
		bset	d0,SCREEN_BW*2(a0,d2)

		move.w	d5,d0
		add.w	d7,d0
		move.w	d0,d1
		lsr.w	#3,d1
		not.w	d0

		move.w	(a3)+,d2
		add.w	d1,d2
		bset	d0,(a1,d2)

		move.w	(a4)+,d2
		add.w	d1,d2
		bset	d0,(a1,d2)

		dbf	d7,.dot
		rts


********************************************************************************
Clear:
		WAIT_BLIT
		clr.w	bltdmod(a6)
		move.l	#$01000000,bltcon0(a6)
		move.l	a0,bltdpt(a6)
		move.w	#SCREEN_H*LAYER_BPLS*64+SCREEN_BW/2,bltsize(a6)
		rts

********************************************************************************
; a0 - queue
; a1 - words
;-------------------------------------------------------------------------------
BuildQueue:
.word:
		movem.w	(a1)+,d6-d7	; word offset
.letter:
		move.w	(a1)+,d0	; x or EOF
		blt	.wordDone
		move.b	(a1)+,d1
		and.w	#$ff,d1
		move.b	(a1)+,d2
		and.w	#$ff,d2
		add.w	d6,d0		; add word offset to letter
		add.w	d7,d1
		lea	CharOffsets,a2
		sub.w	#65,d2
		add.w	d2,d2
		move.w	(a2,d2),d2
		lea	CharData,a2
		lea	(a2,d2.w),a2
		moveq	#0,d2
.glyph:
		move.b	(a2)+,d2	; glyph
		blt	.letterDone
		move.b	(a2)+,d3	; x
		ext.w	d3
		add.w	d0,d3
		move.w	d3,(a0)+
		move.b	(a2)+,d3	; y
		ext.w	d3
		add.w	d1,d3
		move.w	d3,(a0)+
		move.w	d2,(a0)+	; write glyph
		move.b	#0,d3		; dir TODO
		ext.w	d3
		move.w	d3,(a0)+
		bra	.glyph
.letterDone:
		bra	.letter
.wordDone:
		tst.w	(a1)
		bge	.word
		move.w	#-1,(a0)+	; end of queue
		rts


********************************************************************************
; a0 - Layer
;-------------------------------------------------------------------------------
ProcessLayer:
		move.l	Layer_Queue(a0),a5

		; end of word?
		move.w	(a5)+,d0
		blt	.done

		movem.w	(a5)+,d1-d3
		move.w	QueueStep(pc),d4
		and.w	#7,d4
		; Update pos?
		bne	.noStep
		move.l	a5,Layer_Queue(a0)
.noStep:
		move.l	Layer_Buffer(a0),a1
		bsr	BlitStroke

.done:		rts


********************************************************************************
; Blit stroke
;-------------------------------------------------------------------------------
; d0 - x
; d1 - y
; d2 - glyph
; d3 - dir
; d4 - step pos
; a1 - screen buffer
;-------------------------------------------------------------------------------
BlitStroke:
		lea	Font,a0
		mulu	#STROKE_SIZE,d2
		lea	(a0,d2.w),a0

		moveq	#15,d2
		and.w	d0,d2
		lsl.w	#2,d2		; d2 = offset into bltcon table

		mulu	#SCREEN_BW*LAYER_BPLS,d1
		asr.w	#3,d0
		add.w	d0,d1
		lea	(a1,d1.w),a1

		move.l	.bltcon(pc,d2),d6

		; Set blit size from step
		addq	#1,d4

		; linear steps
		; mulu	#STROKE_H/8,d4	; d4 = blit height

		; log2 steps
		move.w	d4,d5
		lsr	d5
		move.w	#2,d4
		lsl.w	d5,d4

; Convert height to bltcon0 value
		lsl.w	#7,d4		; mulu	#64*LAYER_BPLS,d4
		add.w	#STROKE_BW/2,d4

		jsr	WaitBlitter
		move.l	d6,bltcon0(a6)
		move.l	#-1,bltafwm(a6)
		move.l	#SCREEN_BW-STROKE_BW,bltamod(a6)
		move.w	#SCREEN_BW-STROKE_BW,bltcmod(a6)
		movem.l	a0-a1,bltapt(a6)
		move.l	a1,bltcpt(a6)
		move.w	d4,bltsize(a6)
		rts

; Table for combined minterm and shifts for bltcon0/bltcon1
.bltcon:	dc.l	$0bfa0000,$1bfa1000,$2bfa2000,$3bfa3000
		dc.l	$4bfa4000,$5bfa5000,$6bfa6000,$7bfa7000
		dc.l	$8bfa8000,$9bfa9000,$abfaa000,$bbfab000
		dc.l	$cbfac000,$dbfad000,$ebfae000,$fbfaf000


********************************************************************************
; Set bpl pointers in copper:
;-------------------------------------------------------------------------------
; a0 - Screen
;-------------------------------------------------------------------------------
Scroll:
; Layer 1
		lea	Layer1(pc),a2
		move.w	Layer_Scroll(a2),d0

		; h scroll
		move.w	d0,d1
		not.w	d1
		and.w	#$f,d1
		move.w	d1,d2

		lea	CopBplPtPF1+2,a3
		bsr	ScrollLayer

; Layer 2
		lea	Layer2(pc),a2
		move.w	Layer_Scroll(a2),d0

		; h scroll
		move.w	d0,d1
		not.w	d1
		and.w	#$f,d1
		lsl.w	#4,d1
		or.w	d2,d1

		lea	CopBplPtPF2+2,a3
		bsr	ScrollLayer

		move.w	d1,CopScroll

; Extra
		move.l	ViewScreen(pc),d0
		lea	CopBplPtExtra+2,a3
		move.w	d0,4(a3)	; lo
		swap	d0
		move.w	d0,(a3)		; hi
		swap	d0
		add.l	#SCREEN_BW,d0
		move.w	d0,12(a3)	; lo
		swap	d0
		move.w	d0,8(a3)	; hi

		rts


********************************************************************************
ScrollLayer:
		; byte offset
		move.l	Layer_Buffer(a2),a0
		lsr.w	#4,d0
		add.w	d0,d0
		lea	(a0,d0.w),a0

		; clear righthand word
		lea	SCREEN_BW-2(a0),a1
		WAIT_BLIT
		move.l	#$01000000,bltcon0(a6)
		move.w	#SCREEN_BW-2,bltdmod(a6)
		move.l	a1,bltdpt(a6)
		move.w	#SCREEN_H*LAYER_BPLS*64+1,bltsize(a6)

		; Set copper ptrs
		moveq	#LAYER_BPLS-1,d7
.bpl:
		move.l	a0,d0
		swap	d0
		move.w	d0,(a3)		; hi
		move.w	a0,4(a3)	; lo
		lea	8(a3),a3
		lea	SCREEN_BPL(a0),a0
		dbf	d7,.bpl
		rts

********************************************************************************
; a0 - sprite data
; d0.w - start y
; d1.w - y step
;-------------------------------------------------------------------------------
SetSprites:
		lea	CopSprPt+2,a1
		move.l	a0,a3		; duplicate to read offsets without altering base
		; move.w	d0,d6
		; lsr.w	#8,d6		; d6 = vstart upper
		moveq	#3-1,d7
.l:
		move.w	d0,d3
		lsr.w	#6,d3
		and.w	#4,d3
		rept	2
		move.w	(a3)+,d2	; next offset
		lea	(a0,d2.w),a2	; start of sprite
		move.b	d0,(a2)		; set vstart lower
		move.b	3(a2),d4	; get ctrl byte 3
		and.b	#$f2,d4		; clr vstart upper
		or.w	d3,d4		; set new vstart upper
		move.b	d4,3(a2)	; set new ctrl byte 3
		; Set copper ptrs
		move.l	a2,d2
		swap	d2
		move.w	d2,(a1)
		move.w	a2,4(a1)
		lea	8(a1),a1	; next sprite in copper
		endr

		add.w	d1,d0

		dbf	d7,.l

		rts


********************************************************************************
SetPalette:
		move.l	PalOut(pc),a0
		; pf1
		move.l	(a0),color00(a6)
		move.l	(a0)+,color04(a6) ; first half gets overwritten but keep it simple
		move.l	(a0),color02(a6)
		move.l	(a0)+,color06(a6)
		; pf2
		move.l	(a0),color09(a6)
		move.l	(a0)+,color13(a6)
		move.w	(a0),color11(a6)
		move.w	(a0)+,color15(a6)
		; sprites
		lea	color17(a6),a1
		moveq	#3-1,d7
.l:
		move.w	(a0),16(a1)
		move.w	(a0),8(a1)
		move.w	(a0)+,(a1)+
		dbf	d7,.l

		move.w	(a0)+,color04(a6)
		move.w	(a0)+,color12(a6)

		rts


********************************************************************************
Vars:
********************************************************************************

SpriteTop:	dc.w	255+DIW_YSTRT
AltInc:		dc.w	-1
PalOut:		dc.l	BlankData
QueueStep:	dc.w	0
YTbl:		dc.l	0


********************************************************************************
; Data
********************************************************************************

		rsreset
Layer_Queue	rs.l	1
Layer_Buffer	rs.l	1
Layer_Scroll	rs.w	1
Layer_SIZEOF	rs.b	0

Layer1:		ds.b	Layer_SIZEOF
Layer2:		ds.b	Layer_SIZEOF
Layer3:		ds.b	Layer_SIZEOF

ScreenBuffers:
ClearScreen:	dc.l	0
DrawScreen:	dc.l	0
ViewScreen:	dc.l	0

Pal:
		dc.w	$000		; bg
		dc.w	$627		; l1 1
		dc.w	$b6c		; l1 2
		dc.w	$dae		; l1 1+2
		dc.w	$133		; l2 1
		dc.w	$277		; l2 2
		dc.w	$6cc		; l2 1+2
		dc.w	$345
		dc.w	$dee
		dc.w	$79a
		dc.w	$027
		dc.w	$207

********************************************************************************
		data
********************************************************************************

		include	include/graf-font.i


********************************************************************************
		data_c
********************************************************************************

Font:
		incbin	data/graff-font.BPL
Sprite:
		incbin	data/boombox-small.SPR
SpriteAlt:
		incbin	data/boombox-small-alt.SPR

Cop:
		dc.w	diwstrt,DIW_YSTRT<<8!DIW_XSTRT
		dc.w	diwstop,(DIW_YSTOP-256)<<8!(DIW_XSTOP-256)
		dc.w	ddfstrt,(DIW_XSTRT-17)>>1&$fc-SCROLL*8
		dc.w	ddfstop,(DIW_XSTRT-17+(DIW_W>>4-1)<<4)>>1&$fc
		dc.w	bpl1mod,DIW_MOD
		dc.w	bpl2mod,DIW_MOD
		dc.w	bplcon0,BPLS<<12!DPF<<10!$200
		dc.w	bplcon1
CopScroll:	dc.w	0
CopBplPtPF1:
		dc.w	bpl0pt,0
		dc.w	bpl0ptl,0
		dc.w	bpl2pt,0
		dc.w	bpl2ptl,0
CopBplPtPF2:
		dc.w	bpl1pt,0
		dc.w	bpl1ptl,0
		dc.w	bpl3pt,0
		dc.w	bpl3ptl,0
CopBplPtExtra:
		dc.w	bpl4pt,0
		dc.w	bpl4ptl,0
		dc.w	bpl5pt,0
		dc.w	bpl5ptl,0
CopSprPt:
		rept	8*2
		dc.w	sprpt+REPTN*2,0
		endr
		dc.l	-2
CopE:
