		include	_main.i
		include	effects/city.i

********************************************************************************
* Constants:
********************************************************************************

; Display window:
DIW_W = 320
DIW_H = 256
BPLS = 4

; Foreground
FG_W = DIW_W
FG_H = DIW_H
FG_BW = FG_W/8				; byte-width of 1 bitplane line
FG_SIZE = FG_BW*FG_H			; byte size of screen buffer

BUILDINGS_H = LANDSCAPE_Y+LANDSCAPE_H+SKYLINE_H-4

DITHER_H = 200
DITHER_SIZE = DITHER_H*FG_BW

; Background
BG_W = 512
BG_BW = BG_W/8
SKYLINE_H = 23
LANDSCAPE_H = 35
LANDSCAPE_Y = 120
GRAD_H = LANDSCAPE_Y

TEXT_H = 5
TEXT_W = 128
TEXT_BW = TEXT_W/8

; Perspective
PERSP_SHIFT = 6
DIST = 500
FLOOR = 70

; Colour Lerp duration 1<<COL_LERP_POW
COL_LERP_POW = 8

ARR_END = $8000

;-------------------------------------------------------------------------------
; Derived

DIW_BW = DIW_W/16*2
DIW_XSTRT = ($242-DIW_W)/2
DIW_YSTRT = ($158-DIW_H)/2
DIW_XSTOP = DIW_XSTRT+DIW_W
DIW_YSTOP = DIW_YSTRT+DIW_H


Script:
		dc.l	10,CmdMoveIW,$5fd,TextCol
		dc.l	12*3000/BPM,CmdMoveIW,0,TextCol
		dc.l	56*3000/BPM,CmdMoveIL,Text2,TextImg
		dc.l	56*3000/BPM,CmdMoveIW,$5fd,TextCol
		dc.l	$8000


********************************************************************************
City_Effect:
********************************************************************************
		jsr	SetBgTask
		jsr	MemFlip
		jsr	MemFreeLast
; Alloc mem:
		; Foreground screen buffers
		lea	DblBuffers,a1
		ALLOC_CHIP FG_SIZE*3
		WAIT_BLIT
		move.l	#$1000000,bltcon0(a6)
		clr.w	bltdmod(a6)
		move.l	a0,bltdpt(a6)
		move.w	#FG_H*3*64+FG_BW/2,bltsize(a6)

		move.l	a0,(a1)+
		lea	FG_SIZE(a0),a0
		move.l	a0,(a1)+
		lea	FG_SIZE(a0),a0
		move.l	a0,(a1)+

		; Dither pattern
		ALLOC_CHIP DITHER_SIZE

		; Set dither pattern bpl ptrs
		lea	DitherBplPt+2,a1
		move.l	a0,d0
		subq	#2,d0
		move.w	d0,4(a1)	; lo
		swap	d0
		move.w	d0,(a1)		; hi

		bsr	InitDither

		; Solid fill to avoid borders
		ALLOC_CHIP DITHER_SIZE
		WAIT_BLIT
		move.l	#$1ff0000,bltcon0(a6)
		clr.w	bltdmod(a6)
		move.l	a0,bltdpt(a6)
		move.w	#DITHER_H*64+FG_BW/2,bltsize(a6)

		; Set fill bpl ptrs
		lea	FillBplPt+2,a1
		move.l	a0,d0
		move.w	d0,4(a1)	; lo
		swap	d0
		move.w	d0,(a1)		; hi

		jsr	WaitEOF

		; Clear colors
		lea	color00(a6),a0
		moveq	#32/2-1,d7
.col:		clr.l	(a0)+
		dbf	d7,.col

		bsr	UpdateCols
		bsr	PokeBpls
		move.l	#Cop,cop1lc(a6)

		lea	Script,a0
		jsr	Commander_Init

;-------------------------------------------------------------------------------
MainLoop:
		bsr	VFill
		bsr	WriteText
		bsr	UpdateCols
		bsr	SetCols
		bsr	SetBg
		bsr	UpdateCam
		bsr	Clear
		bsr	Transform
		bsr	DrawLines

		bsr	SwapBuffers
		bsr	PokeBpls
		; move.w	#$f00,color(a6)
		jsr	VSyncWithBgTask

		jsr	PartOver
		blt	MainLoop
		rts


********************************************************************************
* Routines
********************************************************************************

********************************************************************************
InitDither:
		move.w	#DITHER_H/2,d7
.l:
		move.l	#$aaaaaaaa,d0
		move.w	#FG_BW/4-1,d6
.l0:		move.l	d0,(a0)+
		dbf	d6,.l0
		not.l	d0
		move.w	#FG_BW/4-1,d6
.l1:		move.l	d0,(a0)+
		dbf	d6,.l1
		dbf	d7,.l
		rts


********************************************************************************
Clear:
		WAIT_BLIT
		clr.w	bltdmod(a6)
		move.l	#$01000000,bltcon0(a6)
		move.l	DrawScreen(pc),bltdpt(a6)
		move.w	#FG_H*64+FG_BW/2,bltsize(a6)
		rts


********************************************************************************
SwapBuffers:
		movem.l	DblBuffers(pc),a0-a2
		exg	a0,a2
		exg	a1,a2
		movem.l	a0-a2,DblBuffers
		rts

********************************************************************************
PokeBpls:
		lea	FgBplPt+2,a0
		subq	#2,a2
		move.l	a2,d0
		swap	d0
		move.w	d0,(a0)		; hi
		move.w	a2,4(a0)	; lo
		rts


********************************************************************************
UpdateCam:
		lea	Sin,a1
		lea	Vars(pc),a5
; Rotation
		move.w	LocalFrame+2,d1
		move.w	d1,d0
		add.w	d0,d0

		move.w	d0,Rot-Vars(a5)
		and.w	#$3ff,d0
		add.w	d0,d0
		move.w	(a1,d0.w),d0
		asr.w	#6,d0
		add.w	d0,Rot-Vars(a5)

		; Distance
		move.w	d1,d0
		add.w	d0,d0
		and.w	#$3ff,d0
		add.w	d0,d0
		move.w	(a1,d0.w),d0
		asr.w	#6,d0
		add.w	#1000,d0
		move.w	d0,Dist-Vars(a5)

		move.w	d1,d0
		mulu	#5,d0
		and.w	#$3ff,d0
		add.w	d0,d0
		move.w	(a1,d0.w),d0
		asr.w	#7,d0
		add.w	d0,Dist-Vars(a5)

		; Translation
		move.w	d1,d0
		and.w	#$3ff,d0
		add.w	d0,d0
		move.w	(a1,d0.w),d0
		asr.w	#8,d0
		move.w	d0,Translate-Vars(a5)

		move.w	d1,d0
		and.w	#$3ff,d0
		add.w	d0,d0
		move.w	(a1,d0.w),d0
		asr.w	#8,d0
		move.w	d0,Translate+4-Vars(a5)
		rts


********************************************************************************
DrawLines:
		bsr	InitDrawLine
		lea	Transformed,a1
		move.w	#BUILDING_COUNT-1,d7
.l:
		movem.w	(a1)+,d0-d6/a2
		movem.l	d7-a1,-(sp)

		lea	Poly,a0
; Copy and repeat first
		movem.w	d0-d6/a2,(a0)
		movem.w	d0-d1,16(a0)
		move.w	#ARR_END,20(a0)

; Basic clip checks:
; min x
		tst.w	d0
		blt	.clip
		tst.w	d2
		blt	.clip
		tst.w	d4
		blt	.clip
		tst.w	d6
		blt	.clip
; max x
		move.w	#FG_W,a3
		cmp.w	a3,d0
		bge	.clip
		cmp.w	a3,d2
		bge	.clip
		cmp.w	a3,d4
		bge	.clip
		cmp.w	a3,d6
		bge	.clip
; min y
		tst.w	d1
		blt	.clip
		tst.w	d3
		blt	.clip
		tst.w	d5
		blt	.clip
		move.w	a2,d0
		bge	.noClip
.clip:
		bsr	Clip
.noClip:

		lea	ScreenMuls,a1
		lea	Poly,a2
.l1:
		movem.w	(a2),d0-d3
		cmp.w	#ARR_END,d2
		beq	.done
		cmp.w	d2,d0
		bge	.s1
		move.l	DrawScreen(pc),a0
		bsr	DrawLine
.s1:
		addq	#4,a2
		bra	.l1
.done:

		movem.l	(sp)+,d7-a1
		dbf	d7,.l
		rts


********************************************************************************
; Vertical blitter fill
;-------------------------------------------------------------------------------
VFill:
		move.l	FillScreen(pc),a0 ; prev line
		lea	(FG_BW-DIW_BW)/2(a0),a0
		WAIT_BLIT
		move.l	#-1,bltafwm(a6)
		move.w	#FG_BW-DIW_BW,d0
		move.w	d0,bltamod(a6)
		move.w	d0,bltbmod(a6)
		move.w	d0,bltdmod(a6)
		move.l	a0,bltapt(a6)
		lea	FG_BW(a0),a0	; curr line
		move.l	a0,bltbpt(a6)
		move.l	a0,bltdpt(a6)
		move.l	#(BLTEN_ABD!(BLT_A!BLT_B))<<16,bltcon0(a6)
		move.w	#((BUILDINGS_H-1)<<6)!(DIW_BW/2),bltsize(a6)
		rts


********************************************************************************
WriteText:
		move.l	TextImg(pc),a0
		move.l	FillScreen(pc),a1
		lea	FG_BW*230+12(a1),a1
		WAIT_BLIT
		move.l	#-1,bltafwm(a6)
		clr.w	bltamod(a6)
		move.w	#FG_BW-TEXT_BW,bltdmod(a6)
		move.l	a0,bltapt(a6)
		move.l	a1,bltdpt(a6)
		move.l	#$09f00000,bltcon0(a6)
		move.w	#(TEXT_H<<6)!(TEXT_BW/2),bltsize(a6)

		move.w	TextCol(pc),d0
		move.w	BpmFrame,d1
		and.w	#1<<5,d1	; flash speed
		beq	.n
		clr.w	d0
.n:
		move.w	d0,ColText
		rts

TextImg:	dc.l	Text1
TextCol:	dc.w	0


********************************************************************************
UpdateCols:
		lea	ColDeltas,a0
		lea	ColValues,a1
		move.w	ColLerpSteps,d0
		bne	.doLerp
		; init
		move.l	ColorsPos(pc),a2
		cmp.l	#ColorsE-COLS_SZ,a2
		bge	.done
		lea	COLS_SZ(a2),a3

		moveq	#COLS_COUNT-1,d7
.l:
		movem.w	(a2)+,d0-d2	; current r/g/b
		movem.w	(a3)+,d3-d5	; next r/g/b

		sub.w	d0,d3
		sub.w	d1,d4
		sub.w	d2,d5

		; conv to fixed point
		swap	d0
		swap	d1
		swap	d2
		swap	d3
		swap	d4
		swap	d5
		clr.w	d0
		clr.w	d1
		clr.w	d2
		clr.w	d3
		clr.w	d4
		clr.w	d5

		; write start values
		move.l	d0,(a1)+
		move.l	d1,(a1)+
		move.l	d2,(a1)+

		asr.l	#COL_LERP_POW,d3
		asr.l	#COL_LERP_POW,d4
		asr.l	#COL_LERP_POW,d5

		; write deltas
		move.l	d3,(a0)+
		move.l	d4,(a0)+
		move.l	d5,(a0)+
		dbf	d7,.l

		move.l	a2,ColorsPos
		move.w	#1<<COL_LERP_POW,ColLerpSteps
.done:		rts
.doLerp:
		subq	#1,d0
		move.w	d0,ColLerpSteps
		moveq	#COLS_COUNT-1,d7
.l1:
		movem.l	(a0)+,d0-d2
		add.l	d0,(a1)+
		add.l	d1,(a1)+
		add.l	d2,(a1)+
		dbf	d7,.l1

		rts


********************************************************************************
SetCols:
; Do gradient lerp and write to copperlist

GRAD_ROW	macro
		add.l	#$4000*\1,d2	; dither
		move.l	d2,d4		; keep d2 before swap for round up

		swap	d0
		swap	d1
		swap	d2

		add.w	#$4*\1,d1	; dither
		add.w	#$40*\1,d0	; dither

		; Convert $rgb, floor
		move.w	d0,d6
		move.b	d5,d6		; $f0
		and.b	d1,d6
		or.b	d2,d6
		move.w	d6,(a0)

		; Convert $rgb, round up
		move.w	d0,d6
		add.w	a2,d6
		move.b	d5,d6		; $f0
		move.b	d1,d3
		addq.b	#$8,d3
		and.b	d3,d6
		add.l	a1,d4		; #$8000
		swap	d4
		or.b	d4,d6
		move.w	d6,4(a0)

		lea	12(a0),a0

		swap	d0
		swap	d1
		swap	d2

		add.l	a3,d0
		add.l	a4,d1
		add.l	a5,d2
		endm

DO_GRAD		macro
		; delta
		sub.l	d0,d3
		sub.l	d1,d4
		sub.l	d2,d5
		divs	#\1,d3
		divs	#\1,d4
		divs	#\1,d5
		ext.l	d3
		ext.l	d4
		ext.l	d5
		; Pre-shift the values and deltas to RGB positions to simplify conversion
		lsl.l	#8,d0
		lsl.l	#4,d1
		lsl.l	#8,d3
		lsl.l	#4,d4

		; move deltas to address regs to free up some data regs
		move.l	d3,a3
		move.l	d4,a4
		move.l	d5,a5

		; immediate values to regs
		move.l	#$8000,a1
		move.w	#$80,a2
		move.b	#$f0,d5

		moveq	#\1-1,d7
.l\@:
		; two rows per loop for dither
		GRAD_ROW 1
		subq	#1,d7
		GRAD_ROW -1
		dbf	d7,.l\@
		endm

		; Grad from -> grad mid
		lea	CopGrad+6,a0
		movem.l	GradFrom(pc),d0-d5
		DO_GRAD	GRAD_H,DIW_YSTRT

		; Grad mid -> grad to
		lea	CopGrad2+6,a0
		movem.l	GradMid(pc),d0-d5
		DO_GRAD	LANDSCAPE_H-3,DIW_YSTRT+LANDSCAPE_Y

		; Landscape color
		movem.l	LandscapeC(pc),d0-d2
		lsl.l	#8,d0
		lsl.l	#4,d1
		move.l	d2,d4		; keep d2 before swap for round up
		swap	d0
		swap	d1
		swap	d2

		; Convert $rgb, floor
		move.w	d0,d6
		move.b	d5,d6		; $f0
		and.b	d1,d6
		or.b	d2,d6

		lea	LandscapeCol+2,a0
		move.w	d6,(a0)
		move.w	d6,LandscapeCol1-LandscapeCol(a0)

		move.w	d0,d6
		add.w	a2,d6
		move.b	#$f0,d6		; $f0
		move.b	d1,d3
		addq.b	#$8,d3
		and.b	d3,d6
		add.l	a1,d4
		swap	d4
		or.b	d4,d6

		move.w	d6,LandscapeColA-LandscapeCol(a0)
		move.w	d6,LandscapeCol1A-LandscapeCol(a0)

		lea	custom,a6
		rts


********************************************************************************
SetBg:
		move.w	Rot(pc),d0
		asr.w	#1,d0
		and.w	#511,d0
		move.w	d0,d1
		lsr.w	#4,d1
		add.w	d1,d1
		lea	LandscapeBplcon1+2,a0
		neg.w	d1
		and.w	#$f,d0
		lsl.w	#4,d0
		move.w	d0,(a0)
		lea	LandscapeBplPt+2,a0
		lea	Landscape,a1
		lea	BG_BW(a1,d1.w),a1
		move.l	a1,d0
		swap	d0
		move.w	d0,(a0)		; hi
		move.w	a1,4(a0)	; lo

		moveq	#0,d0
		move.w	Rot(pc),d0
		divs	#3,d0
		and.w	#511,d0
		move.w	d0,d1
		lsr.w	#4,d1
		add.w	d1,d1
		lea	SkylineBplCon1+2,a0
		neg.w	d1
		and.w	#$f,d0
		lsl.w	#4,d0
		move.w	d0,(a0)
		lea	SkylineBplPt+2,a0
		lea	Skyline,a1
		lea	BG_BW(a1,d1.w),a1
		move.l	a1,d0
		swap	d0
		move.w	d0,(a0)		; hi
		move.w	a1,4(a0)	; lo
		rts


********************************************************************************
InitDrawLine:
; Prepare common blit regs for line draw
;-------------------------------------------------------------------------------
		WAIT_BLIT
		move.w	#FG_BW,bltcmod(a6)
		move.l	#-$8000,bltbdat(a6)
		move.l	#-1,bltafwm(a6)
		rts


********************************************************************************
; Draw a line *not* for filling using the blitter
;-------------------------------------------------------------------------------
; d0.w - x1
; d1.w - y1
; d2.w - x2
; d3.w - y2
; a0 - Draw buffer
; a1 - screen muls
; a6 - Custom
;-------------------------------------------------------------------------------
DrawLine:	sub.w	d0,d2		; D2 = Dx = X1 - X2
		bmi.b	.Oct2345	; Nagative? Octant could be 2,3,4,5
		sub.w	d1,d3		; D3 = Dy = Y1 - Y2
		bmi.b	.Oct01		; Negative? Octant is 0 or 1
		cmp.w	d3,d2		; Compare Dy with Dx
		bmi.b	.Oct6		; Dy > Dx? Octant 6!
		moveq	#$0011,d4	; Select LINE + octant 7!
		bra.b	.DoneOctant

.Oct6:		exg	d2,d3		; Ensure D2=Dmax and D3=Dmin
		moveq	#$0001,d4	; Select LINE + octant 6
		bra.b	.DoneOctant

.Oct2345:	neg.w	d2		; Make Dx positive
		sub.w	d1,d3		; D3 = Dy = Y1 - Y2
		bmi.b	.Oct23		; Negative? Octant is 2 or 3
		cmp.w	d3,d2		; Compare Dy with Dx
		bmi.b	.Oct5		; Dy > Dx? Octant 5!
		moveq	#$0015,d4	; Select LINE + octant 4
		bra.b	.DoneOctant

.Oct5:		exg	d2,d3		; Ensure D2=Dmax and D3=Dmin
		moveq	#$0009,d4	; Select LINE + octant 5
		bra.b	.DoneOctant

.Oct23:		neg.w	d3		; Make Dy positive
		cmp.w	d3,d2		; Compare Dy with Dx
		bmi.b	.Oct2		; Dy > Dx? Octant 2!
		moveq	#$001d,d4	; Select LINE + octant 3
		bra.b	.DoneOctant

.Oct2:		exg	d2,d3		; Ensure D2=Dmax and D3=Dmin
		moveq	#$000d,d4	; Select LINE + octant 2
		bra.b	.DoneOctant

.Oct01:		neg.w	d3		; Make Dy positive
		cmp.w	d3,d2		; Compare Dy with Dx
		bmi.b	.Oct1		; Dy > Dx? Octant 1!
		moveq	#$0019,d4	; Select LINE + octant 0
		bra.b	.DoneOctant

.Oct1:		exg	d2,d3		; Ensure D2=Dmax and D3=Dmin
		moveq	#$0005,d4	; Select LINE + octant 1

.DoneOctant:	add.w	d2,d2		; D2 = 2 * Dmax
		asl.w	#2,d3		; D3 = 4 * Dmin

		; mulu	#FG_BW,d1	; Convert Y1 pos into offset
		; replace mul with LUT
		add.w	d1,d1
		move.w	(a1,d1.w),d1
		lea	(a0,d1.w),a0	; Add offset to bitplane pointer

		ext.l	d0		; Clear top bits of D0
		ror.l	#4,d0		; Roll shift bits to top word
		add.w	d0,d0		; Bottom word: convert to byte offset
		adda.w	d0,a0		; Add byte offset to bitplane pointer
		swap	d0		; Move shift value to bottom word
		or.w	#$0bca,d0	; USEA, C and D. Minterm $5A, D=A/C+/AC

		move.w	d2,d1		; D1 = 2 * Dmax
		lsl.w	#5,d1		; Shift Dmax to Hx pos for BLTSIZE
		add.w	#$0042,d1	; Add 1 to Hx and set Wx to 2

		WAIT_BLIT

		move.l	a0,bltcpt(a6)	; Source C = bitplane to draw on
		move.l	a0,bltdpt(a6)	; Destination = bitplane to draw on
		move.w	d0,bltcon0(a6)	; Source A shift and logic function
		move.w	d3,bltbmod(a6)	; Set 4 * Dmin

		sub.w	d2,d3		; D3 = (2 * Dmax)-(4 * Dmin)
		ext.l	d3		; Make full long sized
		move.l	d3,bltapt(a6)	; Store in A pointer
		bpl.b	.NotNeg		; Skip if positive
		or.w	#$0040,d4	; Set SIGN bit if negative
.NotNeg:	move.w	d4,bltcon1(a6)	; Octant selection, SIGN and LINE
		sub.w	d2,d3		; D2 = (2*Dmax), D3 = (2*Dmax)-(4*Dmin)
		move.w	d3,bltamod(a6)	; D3 = 4 * (DMax - Dmin)
		move.w	d1,bltsize(a6)	; Set length and start the Blitter
		rts


********************************************************************************
Transform:
		move.w	Rot(pc),d0	; d0 = a
		and.w	#$3ff,d0
		add.w	d0,d0
		lea	Cos,a0
		move.w	(a0,d0.w),d1	; d1 = cos(a)
		lea	Sin,a0
		move.w	(a0,d0.w),d0	; d0 = sin(a)

		lea	BuildingData,a0
		lea	Transformed,a1
		movem.w	Translate(pc),a2-a4

		move.w	#BUILDING_COUNT*4-1,d7
.l:
		movem.w	(a0)+,d2-d4
		; translate
		add.w	a2,d2		; x
		add.w	a3,d3		; y
		add.w	a4,d4		; z
		; rotate Y
		move.w	d2,d5
		muls	d1,d2		; x*cos(a)
		muls	d0,d5		; x*sin(a)
		move.w	d4,d6
		muls	d1,d4		; z*cos(a)
		muls	d0,d6		; z*sin(a)
		add.l	d6,d2		; x*cos(a)+z*sin(a)
		; lsl.l	#2+2,d2		; fp conv to subpixel res + 2
		; swap	d2
		sub.l	d5,d4		; z*cos(a)-x*sin(a)
		lsl.l	#3,d4		; fp to int
		swap	d4

		; perspective
		add.w	Dist(pc),d4	; z += dist

		asr.w	#7,d3
		swap	d3
		clr.w	d3
		divs	d4,d2
		divs	d4,d3

		asr.w	#4,d2

		add.w	#(FG_W/2),d2
		add.w	#(FG_H/2+FLOOR),d3

		move.w	d2,(a1)+
		move.w	d3,(a1)+

		dbf	d7,.l
		rts


********************************************************************************
; Sutherland-Hodgman clipping algorithm
; Based on Planet-Rocklobster
;-------------------------------------------------------------------------------
; a0 - source polygon (overwritten)
;-------------------------------------------------------------------------------
Clip:
		move.l	a0,-(sp)	; keep reference to source
		move.w	#FG_W-1,d5	; d5 = MAX_X

;-------------------------------------------------------------------------------
; Right:
		lea	ClipRight,a3	; a3 = dest
		move.l	a3,a2		; keep reference for first point
.lRight:
		movem.w	(a0),d0-d3	; get 2 points from source - current and next
		addq	#4,a0		; increment 1 point
		cmpi.w	#ARR_END,d2	; end of array?
		beq.b	.endRight

		moveq	#0,d4		; d4 = inside state
; Check p1:
		cmp.w	d5,d0		; p1 > MAX_X?
		bgt.b	.p1Right
		bset	#0,d4		; p1 is inside - set bit
		move.w	d0,(a3)+	; add to output
		move.w	d1,(a3)+
.p1Right:

; Check p2:
		cmp.w	d5,d2		; p2 > MAX_X?
		bgt.b	.p2Right
		bchg	#0,d4		; toggle inside state - this will tell us if state for both points is the same
.p2Right:

		tst.w	d4		; clipping needed?
; If states for p1 and p2 are the same the bit will cancel out to zero
; Nothing to do in either case - either both visible (p1 was added) or both offscreen (no points added)
		beq.b	.lRight

; Do clipping:
; Find intersection point
		sub.w	d0,d2		; x2-x1
		sub.w	d1,d3		; y2-y1
		move.w	d5,d4
		sub.w	d0,d4		; MAX_X-x1
		muls.w	d4,d3		; (y2-y1)*(MAX_X-x1)
		divs	d2,d3		; (y2-y1)*(MAX_X-x1)/(x2-x1)
		add.w	d3,d1		; y1+(y2-y1)*(MAX_X-x1)/(x2-x1)
; Add clippoint:
		move.w	d5,(a3)+	; x = MAX_X
		move.w	d1,(a3)+	; y = intersection point
		bra.b	.lRight
.endRight:
		move.l	(a2)+,(a3)+	; add first point to close loop
		move.w	#ARR_END,(a3)+

;-------------------------------------------------------------------------------
; Left:
		lea	ClipRight,a0
		lea	ClipLeft,a3
		move.l	a3,a2
.lLeft:
		movem.w	(a0),d0-d3
		addq	#4,a0
		cmpi.w	#ARR_END,d2
		beq.b	.endLeft
		moveq	#0,d4
		tst.w	d0
		blt.b	.p1Left
		bset	#0,d4
		move.w	d0,(a3)+
		move.w	d1,(a3)+
.p1Left:
		tst.w	d2
		blt.b	.p2Left
		bchg	#0,d4
.p2Left:
		tst.w	d4
		beq.b	.lLeft
		sub.w	d0,d2		; x2-x1
		sub.w	d1,d3		; y2-y1
		neg.w	d0		; MIN_X-x1
		muls.w	d0,d3		; (y2-y1)*(MIN_X-x1)
		divs	d2,d3		; (y2-y1)*(MIN_X-x1)/(x2-x1)
		add.w	d3,d1		; y1+(y2-y1)*(MIN_X-x1)/(x2-x1)
		clr.w	(a3)+		; x = 0
		move.w	d1,(a3)+	; y = intersection point
		bra.b	.lLeft
.endLeft:
		move.w	(a2)+,(a3)+
		move.w	(a2)+,(a3)+
		move.w	#ARR_END,(a3)+

;-------------------------------------------------------------------------------
; Top:
		; lea	ClipBottom,a0
		lea	ClipLeft,a0
		move.l	(sp)+,a3
		move.l	a3,a2
.lTop:
		movem.w	(a0),d0-d3
		addq	#4,a0
		cmpi.w	#ARR_END,d2
		beq.b	.endTop
		moveq	#0,d4
		tst.w	d1
		blt.b	.p1Top
		bset	#0,d4
		move.w	d0,(a3)+
		move.w	d1,(a3)+
.p1Top:
		tst.w	d3
		blt.b	.p2Top
		bchg	#0,d4
.p2Top:
		tst.w	d4
		beq.b	.noClipTop
		sub.w	d1,d3		; y2-y1
		sub.w	d0,d2		; x2-x1
		neg.w	d1		; MIN_Y-y1
		muls.w	d1,d2		; (x2-x1)*(MIN_Y-y1)
		divs	d3,d2		; (x2-x1)*(MAX_Y-y1)/(y2-y1)
		add.w	d2,d0		; x1+(x2-x1)*(MIN_Y-y1)/(y2-y1)
		move.w	d0,(a3)+	; x = intersection point
		clr.w	(a3)+		; y = MIN_Y
		bra.b	.lTop
.noClipTop:
		; Add top line - can't skip lines off top of canvas with vertical fill
		tst.w	d1
		bge	.lTop
		move.w	d0,(a3)+
		clr.w	(a3)+
		move.w	d2,(a3)+
		clr.w	(a3)+
		bra.b	.lTop
.endTop:
		move.l	(a2)+,(a3)+	; add first point as last to make loop perfect
		move.w	#ARR_END,(a3)+
		rts


********************************************************************************
Vars:
********************************************************************************

DblBuffers:
DrawScreen:	dc.l	0
FillScreen:	dc.l	0
ViewScreen:	dc.l	0

Rot:		ds.w	1
Translate:	ds.w	3
Dist:		ds.w	1

MAX_FACE_VERTS = 8			; TODO

Poly:		ds.l	MAX_FACE_VERTS

; Buffers for Clip routine
ClipRight:	ds.l	MAX_FACE_VERTS
ClipLeft:	ds.l	MAX_FACE_VERTS
ClipBottom:	ds.l	MAX_FACE_VERTS

ColorsPos:	dc.l	Colors
ColLerpSteps:	dc.w	0

ColValues:
GradFrom:	ds.l	3
GradMid:	ds.l	3
GradTo:		ds.l	3
LandscapeC:	ds.l	3

ColDeltas:	ds.l	COLS_COUNT*3


; split colour into RGB components
COL		macro
		dc.w	(\1>>8)&$f,(\1>>4)&$f,\1&$f
		endm

Colors:
		COL	$000		; grad from
		COL	$000		; grad mid
		COL	$000		; grad to
		COL	$000		; landscape
; https://gradient-blaster.grahambates.com/?points=000@0,007@211,11c@233&steps=256&blendMode=oklab&ditherMode=blueNoise&target=amigaOcs&ditherAmount=40
		COL	$000
		COL	$007
		COL	$11c
		COL	$004
; https://gradient-blaster.grahambates.com/?points=005@0,78f@147,fca@255&steps=256&blendMode=oklab&ditherMode=blueNoise&target=amigaOcs&ditherAmount=40
		COL	$005
		COL	$78f
		COL	$fca
		COL	$66a		;
; https://gradient-blaster.grahambates.com/?points=65a@0,f64@147,fe8@255&steps=256&blendMode=oklab&ditherMode=blueNoise&target=amigaOcs&ditherAmount=40
		COL	$65a
		COL	$f64
		COL	$fe8
		COL	$f64
; https://gradient-blaster.grahambates.com/?points=cef@0,cba@147,fe8@255&steps=256&blendMode=oklab&ditherMode=blueNoise&target=amigaOcs&ditherAmount=40
		COL	$cef
		COL	$cba
		COL	$fe8
		COL	$a98
; ; https://gradient-blaster.grahambates.com/?points=05c@0,cef@130,fff@196,abd@234&steps=256&blendMode=oklab&ditherMode=blueNoise&target=amigaOcs&ditherAmount=40
; 		COL	$05c
; 		COL	$cef
; 		COL	$fff
; 		COL	$abd
;
; 		COL	$000
; 		COL	$007
; 		COL	$11c
; 		COL	$004
;
; 		COL	$000		; grad from
; 		COL	$000		; grad mid
; 		COL	$000		; grad to
; 		COL	$000		; landscape
ColorsE:

COLS_COUNT = 4
COLS_SZ = 2*3*COLS_COUNT


********************************************************************************
		data
********************************************************************************

BUILDING	macro
		dc.w	\1-\4,-\3,\2-\4
		dc.w	\1+\4,-\3,\2-\4
		dc.w	\1+\4,-\3,\2+\4
		dc.w	\1-\4,-\3,\2+\4
		endm

BuildingData:
		BUILDING 80,60,130,20
		BUILDING 90,60,85,10
		BUILDING 30,-30,160,17
		BUILDING -40,30,110,14
		BUILDING 120,80,90,14
		BUILDING -130,-80,70,12

		BUILDING 200,90,75,23
		BUILDING 160,-90,85,21
		BUILDING -140,120,120,18
		BUILDING -110,10,80,20

		BUILDING -190,130,110,18
		BUILDING -130,-120,90,22
BuildingDataE:

BUILDING_SIZE = 4*3*2

BUILDING_COUNT = (BuildingDataE-BuildingData)/BUILDING_SIZE


*******************************************************************************
		data_c
*******************************************************************************

Skyline:
		incbin	data/skyline.BPL
Landscape:
		incbin	data/landscape.BPL
Text1:
		incbin	data/city-text-1.BPL
Text2:
		incbin	data/city-text-2.BPL

*******************************************************************************
Cop:
		dc.w	fmode,0
		dc.w	diwstrt,DIW_YSTRT<<8!DIW_XSTRT
		dc.w	diwstop,(DIW_YSTOP-256)<<8!(DIW_XSTOP-256)
		dc.w	ddfstrt,(DIW_XSTRT-17)>>1&$fc-8
		dc.w	ddfstop,(DIW_XSTRT-17+(DIW_W>>4-1)<<4)>>1&$fc
		dc.w	bpl1mod,FG_BW-DIW_BW-2
		dc.w	bpl2mod,-DIW_BW-2
		dc.w	bplcon0,(BPLS<<12)!$200!1
LandscapeBplcon1:
		dc.w	bplcon1,0
FgBplPt:
		dc.w	bpl0pt,0
		dc.w	bpl0ptl,0
LandscapeBplPt:
		dc.w	bpl1pt,0
		dc.w	bpl1ptl,0
DitherBplPt:
		dc.w	bpl2pt,0
		dc.w	bpl2ptl,0
FillBplPt:
		dc.w	bpl3pt,0
		dc.w	bpl3ptl,0

		dc.w	color00,0
; Sky:
		dc.w	color00+16,0	; sky
		dc.w	color01+16,0	; building
CopGrad:
		rept	LANDSCAPE_Y
		COP_WAITV DIW_YSTRT+REPTN
		dc.w	color00+16,0	; sky
		dc.w	color04+16,0	; sky + dither
		endr

; Landscape:
LandscapeCol:	dc.w	color02+16,0	; hills
		dc.w	color03+16,0	; building + hills
		dc.w	color05+16,0	; building + dither
LandscapeColA:	dc.w	color06+16,0	; hills + dither
		dc.w	color07+16,0	; building + hills + dither
		dc.l	(bpl2mod<<16)!BG_BW-DIW_BW-2
CopGrad2:
		rept	LANDSCAPE_H-3
		COP_WAITV DIW_YSTRT+LANDSCAPE_Y+REPTN
		dc.w	color00+16,0	; sky
		dc.w	color04+16,0	; sky + dither
		endr

; Skyline:
		COP_WAITV DIW_YSTRT+LANDSCAPE_Y+LANDSCAPE_H-2
LandscapeCol1:	dc.w	color00+16,0	; hills col as bg for skyline
LandscapeCol1A:	dc.w	color04+16,0	; hills col as bg for skyline + dither
		dc.w	color02+16,0	; skyline
		dc.w	color06+16,0	; skyline + dither
SkylineBplCon1:	dc.w	bplcon1,0
SkylineBplPt:	dc.w	bpl1pt,0
		dc.w	bpl1ptl,0

; Below horizon
		COP_WAITV DIW_YSTRT+BUILDINGS_H
		dc.w	bplcon0,1<<12!$200!1 ; drop dow to 1 bpl for text
		dc.w	color01
ColText:	dc.w	0

		dc.l	-2


*******************************************************************************
		bss
*******************************************************************************

Transformed:	ds.w	BUILDING_COUNT*4*2
