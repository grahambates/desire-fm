		include	_main.i
		include	effects/intro.i


********************************************************************************
* Constants:
********************************************************************************

; Display window:
DIW_W = 320
DIW_H = 240
PF_BPLS = 3
BPLS = PF_BPLS*2
SCROLL = 0				; enable playfield scroll
DPF = 1					; enable dual playfield

; Screen buffer:
SCREEN_W = DIW_W+16
SCREEN_H = DIW_H+60			; determines the amount of trail extending beyond diw, and length of scroll

LOGO_W = 176
LOGO_BW = LOGO_W/8
LOGO_H = 174
LOGO_BPL = (LOGO_H+1)*LOGO_BW		; +1 to prevent overrun on c2p
LOGO_SIZE = LOGO_BPL*3

; Top box
TOP_H_ACTUAL = 101
TOP_H = MASK_H				; needs empty space to match height of mask
TOP_SIZE = SCREEN_BW*TOP_H

TOP_BLIT_H = 10				; Height of slice blitted on a single frame

; Mask for bars and used to animate in top
MASK_H = 123
MASK_SIZE = SCREEN_BW*MASK_H
MASK_INCLINE = 50

BAR_H = MASK_H
BAR_W = 48
BAR_BW = BAR_W/8
BAR_SIZE = BAR_BW*BAR_H*2

BARS_Y = 120

DIAL_Y = 62
DIAL_H = 102

NUMBER_H = 13
NUMBER_BW = 4
NUMBER_SIZE = NUMBER_H*NUMBER_BW
NUMBER_SPACING = SCREEN_W+256		; Distance between numbers when scrolling


RIGHT_SEG_H = 5
RIGHT_SEG_BW = 4
RIGHT_SEG_SIZE = RIGHT_SEG_H*RIGHT_SEG_BW

MARKER_SPR_H = 12

NUMBER_SPR_SIZE = NUMBER_H*2*2+8	; 2 bpls * h, 2 control words and two end words

MARKER_CLAMPED_X = 249			; max x for clamping before marker breaks free
MARKER_CLAMPED_Y = 93
MARKER_INIT_Y = 142			; y offset at start of range

TRAIL_Y = 45				; controls angle of trail lines

FRAME_START_SCROLL = 550
; FRAME_START_SCROLL = 050
FRAME_END_SCROLL = FRAME_START_SCROLL+SCREEN_H-MARKER_CLAMPED_Y-TRAIL_Y
FRAME_LOGO = FRAME_END_SCROLL+50

; Palette:
COL_DARK_BLUE = $012
COL_MID_BLUE = $123
COL_BLUE = $145
COL_LIGHT_BLUE = $8aa
COL_ORANGE = $e52

;-------------------------------------------------------------------------------
; Derived

SCREEN_BW = SCREEN_W/16*2		; byte-width of 1 bitplane line
SCREEN_BPL = SCREEN_BW*SCREEN_H		; bitplane offset (non-interleaved)
PF_SIZE = SCREEN_BW*SCREEN_H*PF_BPLS

DIW_BW = DIW_W/16*2
DIW_MOD = SCREEN_BW-DIW_BW
DIW_XSTRT = ($242-DIW_W)/2
DIW_YSTRT = ($158-DIW_H)/2
DIW_XSTOP = DIW_XSTRT+DIW_W
DIW_YSTOP = DIW_YSTRT+DIW_H


********************************************************************************
* Macros:
********************************************************************************

LINE_H		macro
		move.w	#\1,d0		; x1
		lea	SCREEN_BW*\2+SCREEN_BPL*\5(a2),a0 ; y / bpl
		move.w	#\3,d1		; x2
		moveq	#\4,d2		; initial step size
		bsr	LineH
		endm

LINE_H_BLIT	macro
		move.w	#\1,d0		; x1
		lea	SCREEN_BW*\2+SCREEN_BPL*\4(a2),a0 ; y / bpl
		move.w	#\3,d1		; x2
		bsr	LineHBlit
		endm

LINE_V		macro
		move.w	#\1,d0		; x1
		lea	SCREEN_BW*\2+SCREEN_BPL*\5(a2),a0 ; y / bpl
		move.w	#\3,d1		; h
		move.w	#\4,d2		; initial step size
		bsr	LineV
		endm


BLIT_CLEAR	macro
		move.w	#(\1)*64+(\2)/2,d0
		bsr	BlitClear
		endm

BLIT_FILL	macro
		lea	((\1)*(\2))-2(a2),a0
		move.w	#(\1)*64+(\2)/2,d0
		bsr	BlitFill
		endm

BLIT_NUM	macro
		move.w	#\1,d0
		move.w	#\2,d1
		move.w	#\3,d2
		bsr	BlitNumber
		endm



********************************************************************************
* Entry points:
********************************************************************************

Script:
		dc.l	0,CmdLerpPal,8,4,BlankData,Pal,PalFront
		dc.l	56,CmdLerpPal,8,5,BlankData,Pal,PalBack

		; Backlight flicker on
		dc.l	96,CmdLerpWord,$4000,3,BacklightBrightness
		dc.l	96+8,CmdLerpWord,0,3,BacklightBrightness
		dc.l	96+8+8,CmdLerpWord,$4000,2,BacklightBrightness
		dc.l	96+8+8+4,CmdLerpWord,0,2,BacklightBrightness
		dc.l	96+8+8+4+4+4,CmdLerpWord,$4000,2,BacklightBrightness
		dc.l	96+8+8+4+4+4+2,CmdLerpWord,0,1,BacklightBrightness
		dc.l	96+8+8+4+4+4+2+2,CmdLerpWord,$4000,1,BacklightBrightness

		dc.l	FRAME_START_SCROLL,CmdLerpPal,8,7,PalTrails,PalTrailsLight,PalBack
		dc.l	FRAME_END_SCROLL,CmdLerpPal,8,3,PalTrailsFlash,PalTrailsLight,PalBack
		dc.l	FRAME_END_SCROLL+(1<<3),CmdLerpPal,8,6,PalTrailsLight,PalTrails,PalBack
		dc.l	FRAME_LOGO+$30,CmdLerpPal,8,6,PalTrails,PalTrailsOff,PalBack

		dc.l	$8000


********************************************************************************
Intro_Effect:
		jsr	SetBgTask

; Allocate RAM:
		ALLOC_CHIP LOGO_SIZE,Logo

		; Playfields:
		ALLOC_CHIP PF_SIZE,BackPF
		BLIT_CLEAR SCREEN_H*PF_BPLS,SCREEN_BW
		ALLOC_CHIP PF_SIZE,FrontPF
		BLIT_CLEAR SCREEN_H*PF_BPLS,SCREEN_BW

		; Mask buffer
		ALLOC_CHIP MASK_SIZE,Mask
		BLIT_CLEAR MASK_H,SCREEN_BW

		; Top shape buffer
		ALLOC_CHIP TOP_SIZE,Top
		BLIT_CLEAR TOP_H,SCREEN_BW

		; Bar src buffer
		ALLOC_CHIP BAR_SIZE,Bar
		BLIT_CLEAR BAR_H,BAR_BW

		; Number sprites
		ALLOC_CHIP NUMBER_SPR_SIZE*4,NumSprs
		BLIT_CLEAR NUMBER_SPR_SIZE*2,1

		lea	LogoChunky,a0
		move.l	Logo(pc),a1
		lea	LOGO_BPL(a1),a2
		lea	LOGO_BPL(a2),a3
		move.w	#LOGO_H*LOGO_BW/2,d7
		bsr	C2P_3bpl

		move.l	#Cop,cop1lc(a6)
		bsr	SetPal

; Precalc
		ALLOC_PUBLIC 28*8*2,PalData

		lea	BlankData,a0
		lea	PalLogo,a1
		move.l	PalData(pc),a2
		lea	PalPtrs,a3
		moveq	#16-1,d0
		moveq	#8-1,d1
		jsr	PreLerpPal

		lea	PalLogo,a0
		lea	FillData,a1
		moveq	#16-1,d0
		moveq	#8-1,d1
		jsr	PreLerpPal

; Initial draw
		WAIT_BLIT
		bsr	DrawTop
		bsr	DrawMask
		bsr	DrawBar
		bsr	DrawRightSeg
		bsr	DrawDial
		bsr	DrawNumbers

		move.l	BackPF(pc),a0
		move.l	FrontPF(pc),a1
		bsr	PokeBpls

		bsr	SetMarkerSprite

		bsr	SetNullSprites

		move.w	#DMAF_SETCLR!DMAF_SPRITE,dmacon(a6)

		jsr	ResetFrameCounter
		lea	Script,a0
		jsr	Commander_Init

;-------------------------------------------------------------------------------
StartLoop:
		bsr	UpdateMarker
		bsr	SetPal
		bsr	DrawRadioAnimation
		bsr	SetBacklightCols

		jsr	VSync
		cmp.w	#FRAME_START_SCROLL,LocalFrame+2
		blt	StartLoop

;-------------------------------------------------------------------------------
; Prepare for scroll loop
		bsr	FlattenDial

		move.l	#Marker4,MarkerSpr ; long marker sprite while flying
		bsr	SetMarkerSprite
		bsr	PokeNumSprites


;-------------------------------------------------------------------------------
ScrollLoop:
		bsr	SetMarkerCol
		bsr	SetPal

; Set scroll pos
		move.w	LocalFrame+2,d0
		sub.w	#FRAME_START_SCROLL,d0
		move.w	d0,ScrollPos

; Move marker slowly forward with scroll
		lsr.l	#2,d0
		move.w	d0,d1
		move.w	d1,d2
		divu	#5,d2
		neg.w	d2
		add.w	#MARKER_CLAMPED_X,d1
		add.w	#MARKER_CLAMPED_Y,d2
		move.w	d1,MarkerX
		move.w	d2,MarkerY
		bsr	SetMarkerSprite

; Scroll radio out of view:
		move.w	ScrollPos(pc),d0
		cmp.w	#DIW_BW/2,d0
		bgt	.pf1ScrollDone
		; Set PF1 ptr for scroll
		; 16px steps makes this super easy
		move.l	FrontPF(pc),a1
		add.w	d0,d0
		lea	(a1,d0.w),a1

		; Clear rightmost word to allow scroll without wrap
		lea	SCREEN_BW-4(a1),a2
		WAIT_BLIT
		move.l	#$01000000,bltcon0(a6)
		move.w	#SCREEN_BW-2,bltdmod(a6)
		move.l	a2,bltdpt(a6)
		move.w	#SCREEN_H*PF_BPLS*64+1,bltsize(a6)

		move.l	BackPF(pc),a0
		bsr	PokeBpls
.pf1ScrollDone:

		bsr	UpdateNumPos
		bsr	SetNumberSprites

; Animate trails behind marker
		bsr	DrawTrails


		jsr	VSync
		cmp.w	#FRAME_END_SCROLL,LocalFrame+2
		blt	ScrollLoop

;-------------------------------------------------------------------------------
; Prepare for end loop

		; Draw end line
		move.l	BackPF(pc),a2
		LINE_V	272,0,56,3,2
		LINE_V	272+16,79,DIW_H-79,3,2

		; Squash marker on impact
		move.l	#Marker1,MarkerSpr
		bsr	SetMarkerSprite
		jsr	VSync
		move.l	#Marker2,MarkerSpr
		bsr	SetMarkerSprite
		jsr	VSync
		move.l	#Marker3,MarkerSpr
		bsr	SetMarkerSprite
		jsr	VSync
		move.l	#Marker5,MarkerSpr
		bsr	SetMarkerSprite



;-------------------------------------------------------------------------------
EndLoop:
		bsr	SetMarkerCol
		bsr	SetPal
		jsr	VSync
		cmp.w	#FRAME_LOGO,LocalFrame+2
		blt	EndLoop

		bsr	BlitLogo

;-------------------------------------------------------------------------------
LogoLoop:
		; Defaults
		move.w	#$f,d0		; logo fade step
		move.w	#COL_ORANGE,MarkerCol

		; Drum roll?
		move.w	LocalFrame+2,d1
		cmp.w	#56*3000/BPM,d1	; start?
		blt	.rollNotStarted
		cmp.w	#60*3000/BPM,d1	; end?
		bge	.rollDone
		move.w	#$f+3,d0	; rolling
		move.w	MarkerCols+10,MarkerCol
		bra	.rollDone
.rollNotStarted:

		; Fade in:
		move.w	LocalFrame+2,d0
		sub.w	#FRAME_LOGO,d0
		lsr.w	#1,d0
		cmp.w	#$f,d0
		ble	.noClamp
		move.w	#$f,d0
.noClamp:

		; Flash
		move.w	BpmFrame,d1
		lsr.w	#4,d1
		and.w	#3,d1
		neg.w	d1
		add.w	#3,d1
		add.w	d1,d0

		bsr	SetMarkerCol
.rollDone:
		lea	PalPtrs,a0
		lsl.w	#2,d0
		move.l	(a0,d0.w),PalFront
		bsr	SetPal

		jsr	VSyncWithBgTask
		jsr	PartOver
		blt	LogoLoop

		rts


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
; Draw trails behind marker once it has escaped
;-------------------------------------------------------------------------------
DrawTrails:
		bsr	InitDrawLine
		move.l	BackPF(pc),a0
		move.w	ScrollPos(pc),d6
		move.w	d6,d5
		lsr.w	#2,d5

		move.w	MarkerX(pc),d0
		sub.w	#11,d0
		move.w	MarkerY(pc),d1
		addq	#2,d1
		moveq	#0,d2
		move.w	#MARKER_CLAMPED_Y+TRAIL_Y,d3

		moveq	#PF_BPLS-1,d7
.l:
		sub.w	d5,d3
		bsr	DrawLine

		addq	#1,d1
		addq	#1,d3
		bsr	DrawLine
		subq	#1,d1
		subq	#1,d3

		add.w	d5,d3
		add.w	d6,d3
		bsr	DrawLine
		sub.w	d6,d3

		subq	#1,d1
		subq	#1,d3
		bsr	DrawLine
		addq	#1,d1
		addq	#1,d3

		asr.w	d5
		asr.w	d6
		lea	SCREEN_BPL(a0),a0
		dbf	d7,.l

		rts


********************************************************************************
DrawRadioAnimation:
; top
		move.w	LocalFrame+2,d1
		move.w	d1,d6
		move.w	#TOP_H_ACTUAL,d0
		lsl.w	#1,d1
		sub.w	d1,d0
		cmp.w	#-1,d0
		blt	.topDone
		bsr	BlitTop
.topDone:
; Bottom bars
		lsr.w	d6
		lsl.w	#3,d6

		move.w	d6,d0
		add.w	#100,d0
		cmp.w	#260,d0
		bgt	.barsRDone
		bsr	BlitBar
.barsRDone:

		move.w	d6,d0
		neg.w	d0
		add.w	#100,d0
		cmp.w	#-20,d0
		blt	.barsLDone
		bsr	BlitBar
.barsLDone:

; Right hand bar
		move.w	#288,d1
		move.w	#DIW_H,d2
		move.w	LocalFrame+2,d6
		move.w	#RIGHT_SEG_H,d3
		mulu	d6,d3
		sub.w	d3,d2
		blt	.rhDone
		sub.w	d6,d1
		bsr	BlitRightSeg
.rhDone:
		rts


********************************************************************************
SetBacklightCols:
		move.w	BacklightBrightness(pc),d0
		add.w	d0,d0
		clr.w	d3
		move.w	#COL_BLUE,d4
		jsr	LerpCol
		move.w	d7,PalBlue

		move.w	BacklightBrightness(pc),d0
		add.w	d0,d0
		clr.w	d3
		move.w	#COL_LIGHT_BLUE,d4
		jsr	LerpCol
		move.w	d7,PalLightBlue
		rts


********************************************************************************
; Copy dial from Back -> Front
; Once backlight effect is done we can combine these and use PF2 for other stuff
;-------------------------------------------------------------------------------
FlattenDial:
		move.l	BackPF(pc),a0	; src
		move.l	FrontPF(pc),a1	; dest
		lea	DIAL_Y*SCREEN_BW(a0),a0
		lea	DIAL_Y*SCREEN_BW(a1),a1
		move.w	#DIAL_H*64!SCREEN_BW/2,d0
		WAIT_BLIT
		move.l	#$dfc0000,bltcon0(a6)
		clr.l	bltamod(a6)
		clr.w	bltamod(a6)
		move.l	a1,bltbpt(a6)
		movem.l	a0-a1,bltapt(a6)
		move.w	d0,bltsize(a6)
		lea	SCREEN_BPL(a0),a0
		lea	SCREEN_BPL(a1),a1
		WAIT_BLIT
		move.l	a1,bltbpt(a6)
		movem.l	a0-a1,bltapt(a6)
		move.w	d0,bltsize(a6)
		lea	SCREEN_BPL(a0),a0
		lea	SCREEN_BPL(a1),a1
		WAIT_BLIT
		move.l	a1,bltbpt(a6)
		movem.l	a0-a1,bltapt(a6)
		move.w	d0,bltsize(a6)

		; Clear original dial
		move.l	BackPF(pc),a0
		lea	DIAL_Y*SCREEN_BW(a0),a0
		BLIT_CLEAR DIAL_H,SCREEN_BW
		lea	SCREEN_BPL(a0),a0
		BLIT_CLEAR DIAL_H,SCREEN_BW
		lea	SCREEN_BPL(a0),a0
		BLIT_CLEAR DIAL_H,SCREEN_BW
		rts


********************************************************************************
; Update the position of marker before it escapes
;-------------------------------------------------------------------------------
UpdateMarker:
		move.l	LocalFrame,d0
		move.w	d0,d1

		; Linear x progress
		mulu	#150,d0		; control speed
		lsr.l	#8,d0

		; Add sin for back-and-forth movement
		lea	Sin,a0
		sub.w	#25,d1		; offset sin
		lsl.w	#3,d1
		and.w	#$7fe,d1
		move.w	(a0,d1.w),d1
		asr.w	#8,d1

		add.w	d1,d0		; total xpos = linear + sin

		moveq	#0,d1
		cmp.w	#MARKER_CLAMPED_X,d0
		ble	.noClamp
		; Choose sprite img based on amount of clamping
		move.w	d0,d1
		sub.w	#MARKER_CLAMPED_X,d1 ; x-max (amount removed in clamp)
		lsr.w	#4,d1
		cmp.w	#2,d1		; limit to 3 sprite variants
		ble	.noClampSpr
		moveq	#2,d1
.noClampSpr:
		lsl.w	#2,d1		; *4 for sprite ptr offset
		move.w	#MARKER_CLAMPED_X,d0
.noClamp:
		; Set sprite
		lea	MarkerSprites,a0
		move.l	(a0,d1.w),MarkerSpr

		move.w	d0,MarkerX

		; Calc y: -x/5
		divu	#5,d0
		neg.w	d0
		add.w	#MARKER_INIT_Y,d0
		move.w	d0,MarkerY

		bra	SetMarkerSprite


********************************************************************************
; Set bpl pointers in copper:
;-------------------------------------------------------------------------------
; a0 - pf1
; a1 - pf2
;-------------------------------------------------------------------------------
PokeBpls:
		lea	CopBplPt+2,a2
		moveq	#3-1,d7
.bpl:
		move.l	a0,d0
		move.l	a1,d1
		swap	d0
		swap	d1
		move.w	d0,(a2)		; hi
		move.w	a0,4(a2)	; lo
		move.w	d1,8(a2)	; hi
		move.w	a1,12(a2)	; lo
		lea	16(a2),a2
		lea	SCREEN_BPL(a0),a0
		lea	SCREEN_BPL(a1),a1
		dbf	d7,.bpl
		rts


********************************************************************************
; Poke current marker sprite in copper and set position
;-------------------------------------------------------------------------------
SetMarkerSprite:
		move.l	MarkerSpr(pc),a0
		move.w	2(a0),d0
		lea	(a0,d0.w),a1	; spr1
		lea	4(a0),a0	; spr2

		lea	CopSprPt+2,a2
		move.l	a0,d0
		move.l	a1,d1
		swap	d0
		swap	d1
		move.w	d0,(a2)
		move.w	a0,4(a2)
		move.w	d1,8(a2)
		move.w	a1,12(a2)

		movem.w	MarkerX(pc),d0/d1
		add.w	#DIW_XSTRT-32,d0
		add.w	#DIW_YSTRT-MARKER_SPR_H/2,d1
		move.w	d0,d2
		add.w	#16,d2

		move.w	d0,d3
		move.w	d2,d4
		and.b	#1,d3
		and.b	#1,d4
		lsr.w	d0
		lsr.w	d2
		move.b	d1,(a0)+	; vstart 1
		move.b	d1,(a1)+	; vstart 2
		move.b	d0,(a0)+	; hstart upper 1
		move.b	d2,(a1)+	; hstart upper 2
		add.b	#MARKER_SPR_H,d1
		move.b	d1,(a0)+	; vstop 1
		move.b	d1,(a1)+	; vstop 2
		move.b	d3,(a0)+	; hstart lower 1
		move.b	d4,(a1)+	; hstart lower 2

		rts


********************************************************************************
PokeNumSprites:
		move.l	NumSprs(pc),a0
		lea	CopSprPt+16+2,a1 ; start a sprite 3, first to used for marker
		moveq	#4-1,d7
.l:
		move.l	a0,d0
		swap	d0
		move.w	d0,(a1)
		move.w	a0,4(a1)
		lea	8(a1),a1	; next sprite slot
		lea	NUMBER_SPR_SIZE(a0),a0 ; next sprite
		dbf	d7,.l
		rts

********************************************************************************
UpdateNumPos:
		move.l	LocalFrame,d2
		sub.l	#100,d2		; adjust to finetune alignment with end
		lsl.l	#4,d2
		divu	#NUMBER_SPACING,d2 ; modulo
		swap	d2
		tst.w	d2
		bne	.noInc
		addq.l	#6,NumPt
.noInc:
		move.w	#NUMBER_SPACING+DIW_XSTRT-27,d0
		sub.w	d2,d0
		cmp.w	#SCREEN_W+DIW_XSTRT,d0 ; Clamp to prevent wrap
		ble	.noClamp
		move.w	#SCREEN_W+DIW_XSTRT,d0
.noClamp:
		move.w	d2,d1
		ext.l	d1
		divu	#5,d1
		add.w	#DIW_YSTRT+5,d1
		move.w	d0,NumX
		move.w	d1,NumY
		rts


********************************************************************************
; Set content and position of number sprites
;-------------------------------------------------------------------------------
SetNumberSprites:
		move.w	NumX(pc),d0
		move.w	NumY(pc),d1
		move.l	NumPt,a2

		; common blit regs for copy
		WAIT_BLIT
		move.l	#$09f00000,bltcon0(a6)
		move.l	#-1,bltafwm(a6)
		move.l	#(2<<16)!2,bltamod(a6)

		move.l	NumSprs(pc),a0
		lea	Numbers,a1	; source
		move.w	#NUMBER_H,d2
		moveq	#3-1,d7
.l:
		bsr	SetSprPos

		move.w	(a2)+,d6
		lea	(a1,d6.w),a3

		WAIT_BLIT
		move.l	a3,bltapt(a6)
		move.l	a0,bltdpt(a6)
		move.w	#NUMBER_H*64+1,bltsize(a6)

		lea	NUMBER_SPR_SIZE-4(a0),a0 ; next sprite
		add.w	#10,d0		; increment pos
		sub.w	#2,d1
		dbf	d7,.l

		bsr	SetSprPos	; final pos

		tst.w	(a2)
		bne	.done
		; .7 for final item
		lea	7*NUMBER_SIZE(a1),a3
		WAIT_BLIT
		move.l	a3,bltapt(a6)
		move.l	a0,bltdpt(a6)
		move.w	#NUMBER_H*64+1,bltsize(a6)

		; decimal point
		WAIT_BLIT
		or.w	#$1000,(NUMBER_H-1)*4(a0)

.done:		rts


NumX:		dc.w	0
NumY:		dc.w	0
NumPt:		dc.l	Num

NUM		macro
		dc.w	\1*NUMBER_SIZE
		dc.w	\2*NUMBER_SIZE
		dc.w	\3*NUMBER_SIZE
		endm
Num:
		NUM	1,1,5
		NUM	1,2,0
		NUM	1,2,5
		NUM	1,3,0
		NUM	1,3,3
		dc.w	0

********************************************************************************
; Writes control words to sprite
;-------------------------------------------------------------------------------
; a0 - sprite
; d0 - x
; d1 - y
; d2 - y
;-------------------------------------------------------------------------------
SetSprPos:
		move.b	d1,(a0)+	; vstart
		move.w	d0,d3
		lsr.w	d3
		move.b	d3,(a0)+	; hstart upper
		move.w	d1,d3
		add.b	d2,d3
		move.b	d3,(a0)+	; vstop
		move.w	d0,d3
		and.b	#1,d3
		move.b	d3,(a0)+	; hstart lower
		rts


********************************************************************************
; Blit number from font to screen
;-------------------------------------------------------------------------------
; a0 - dest
; d0.w - number
; d1.w - x
; d2.w - y
;-------------------------------------------------------------------------------
BlitNumber:
		lea	Numbers,a1
		mulu	#NUMBER_SIZE,d0	; offset to char
		lea	(a1,d0.w),a1

		; bltcon0
		moveq	#$f,d3		; x scroll
		and.w	d1,d3
		ror.w	#4,d3
		or.w	#$dfc,d3

		; dest offset
		mulu	#SCREEN_BW,d2	; y byte offset
		asr.w	#4,d1
		add.w	d1,d1
		add.w	d1,d2		; x byte offset
		lea	(a0,d2.w),a2

		move.w	#NUMBER_H*64+NUMBER_BW/2,d7 ; blit size
		WAIT_BLIT
		move.w	d3,bltcon0(a6)
		clr.w	bltcon1(a6)
		move.l	#-1,bltafwm(a6)
		clr.w	bltamod(a6)
		move.w	#SCREEN_BW-NUMBER_BW,bltbmod(a6)
		move.w	#SCREEN_BW-NUMBER_BW,bltdmod(a6)
		move.l	a2,bltbpt(a6)
		movem.l	a1-a2,bltapt(a6)
		; Blit to all 3 bpls :-(
		move.w	d7,bltsize(a6)
		lea	SCREEN_BPL(a2),a2
		WAIT_BLIT
		move.l	a2,bltbpt(a6)
		movem.l	a1-a2,bltapt(a6)
		move.w	d7,bltsize(a6)
		lea	SCREEN_BPL(a2),a2
		WAIT_BLIT
		move.l	a2,bltbpt(a6)
		movem.l	a1-a2,bltapt(a6)
		move.w	d7,bltsize(a6)
		rts


********************************************************************************
; Blit a segment for the vertical line on the right of the radio
;-------------------------------------------------------------------------------
; d1.w - x
; d2.w - y
;-------------------------------------------------------------------------------
BlitRightSeg:
		move.l	FrontPF(pc),a0
		lea	RightSeg,a1

		; bltcon0
		moveq	#$f,d3		; x scroll
		and.w	d1,d3
		ror.w	#4,d3
		or.w	#$dfc,d3

		; dest offset
		mulu	#SCREEN_BW,d2	; y byte offset
		asr.w	#4,d1
		add.w	d1,d1
		add.w	d1,d2		; x byte offset
		lea	(a0,d2.w),a2

		move.w	#RIGHT_SEG_H*64+RIGHT_SEG_BW/2,d7 ; blit size
		WAIT_BLIT
		move.w	d3,bltcon0(a6)
		clr.w	bltcon1(a6)
		move.l	#-1,bltafwm(a6)
		clr.w	bltamod(a6)
		move.w	#SCREEN_BW-RIGHT_SEG_BW,bltbmod(a6)
		move.w	#SCREEN_BW-RIGHT_SEG_BW,bltdmod(a6)
		move.l	a2,bltbpt(a6)
		movem.l	a1-a2,bltapt(a6)
		move.w	d7,bltsize(a6)
		rts


********************************************************************************
; Set color regs from palette ptrs for both playfields
;-------------------------------------------------------------------------------
SetPal:
		lea	custom,a6
		move.l	PalBack(pc),a0
		move.l	PalFront(pc),a1
		lea	color00(a6),a2
		lea	color08(a6),a3
		moveq	#8-1,d0
.l0:
		move.w	(a0)+,(a2)+
		move.w	(a1)+,(a3)+
		dbf	d0,.l0

		move.w	MarkerCol(pc),color17(a6)
		move.w	NumberCol(pc),color21(a6)
		move.w	NumberCol(pc),color25(a6)
		move.w	NumberCol(pc),color29(a6)
		rts


********************************************************************************
SetMarkerCol:
		lea	MarkerCols,a0
		move.w	BpmFrame,d1
		lsr.w	#2,d1
		and.w	#$f,d1
		add.w	d1,d1
		move.w	(a0,d1.w),MarkerCol
		rts


********************************************************************************
; Draw the shape for the top rect of radio
;-------------------------------------------------------------------------------
DrawTop:
		move.l	Top(pc),a2
		LINE_H_BLIT 2,101,242,0
		LINE_V	231,0,53,5,0
		BLIT_FILL TOP_H,SCREEN_BW
		rts


********************************************************************************
; Draw the mask shape for the bars at bottom of radio
;-------------------------------------------------------------------------------
DrawMask:
		move.l	Mask(pc),a2
		LINE_H_BLIT 0,MASK_INCLINE,255,0
		LINE_V	255,0,MASK_H,5,0
		BLIT_FILL MASK_H,SCREEN_BW
		rts


********************************************************************************
; Draw the radio dial panel
;-------------------------------------------------------------------------------
DrawDial:
		move.l	BackPF(pc),a2

; Filled rects:
		; dial grey + blue
		LINE_H_BLIT 3,130,243,0
		LINE_V	247,82,30,4,0
		LINE_H_BLIT 4,161,249,0

		; dial blue
		LINE_H_BLIT 3,130,243,2
		LINE_V	247,82,17,5,2
		LINE_H_BLIT 0,149,250,2

		; Fill dial
		lea	DIAL_Y*SCREEN_BW(a2),a2
		BLIT_FILL DIAL_H,SCREEN_BW
		lea	SCREEN_BPL(a2),a2
		BLIT_FILL DIAL_H,SCREEN_BW
		lea	SCREEN_BPL(a2),a2
		BLIT_FILL DIAL_H,SCREEN_BW

; Strokes:
		move.l	BackPF(pc),a2
		LINE_H	0,110,244,5,1	; outline top
		LINE_V	244,62,51,5,1	; outline right
		LINE_H	0,162,254,5,1	; outline bottom
		LINE_H	0,131,247,3,1	; blue top
		LINE_H	0,150,250,1,1	; grey top

; Scale lines:
		lea	SCREEN_BW*145+SCREEN_BPL(a2),a0
		moveq	#1,d0
		moveq	#5,d3
		move.w	#-SCREEN_BW,d4
		moveq	#48-1,d5
		bsr	.lineRept

		lea	SCREEN_BW*139+SCREEN_BPL(a2),a0
		moveq	#10,d0
		moveq	#25,d3
		move.w	#-SCREEN_BW*5,d4
		moveq	#10-1,d5
		bsr	.lineRept

		lea	SCREEN_BW*135+SCREEN_BPL(a2),a0
		moveq	#9,d0
		move.w	#-SCREEN_BW*5,d4
		moveq	#10-1,d5
.lineRept:
.l:
		move.w	d0,d1
		asr.w	#3,d1
		move.w	d0,d2
		not.w	d2
		bchg.b	d2,(a0,d1.w)
		bchg.b	d2,SCREEN_BW(a0,d1.w)
		bchg.b	d2,SCREEN_BW*2(a0,d1.w)
		bchg.b	d2,SCREEN_BW*3(a0,d1.w)
		add.w	d3,d0
		lea	(a0,d4.w),a0
		dbf	d5,.l
		rts


********************************************************************************
; Draw numbers for dial
;-------------------------------------------------------------------------------
DrawNumbers:
		;100 - in front
		move.l	FrontPF(pc),a0
		BLIT_NUM 1,12,110+1
		BLIT_NUM 0,12+10,110-1
		BLIT_NUM 0,12+20,110-3

		; Other numbers in back
		move.l	BackPF(pc),a0
		;102
		BLIT_NUM 1,63,100+1
		BLIT_NUM 0,63+10,100-1
		BLIT_NUM 2,63+20,100-3
		;104
		BLIT_NUM 1,112,90+1
		BLIT_NUM 0,112+10,90-1
		BLIT_NUM 4,112+20,90-3
		;106
		BLIT_NUM 1,165,80+1
		BLIT_NUM 0,165+10,80-1
		BLIT_NUM 6,165+20,80-3
		;108
		BLIT_NUM 1,214,70+1
		BLIT_NUM 0,214+10,70-1
		BLIT_NUM 8,214+10+10,70-3
		rts


********************************************************************************
; Draw a single 5x4 block for blitting into right line
;-------------------------------------------------------------------------------
DrawRightSeg:
		lea	RightSeg,a0
		move.l	#%11110000000000000000000000000000,d0
		rept	5
		move.l	d0,(a0)+
		endr
		rts


********************************************************************************
; Draw the source image for a single bar to be blitted into bottom region
;-------------------------------------------------------------------------------
DrawBar:
		move.l	Bar(pc),a2
		; left outline
		moveq	#0,d0
		move.l	a2,a0
		bsr	.line
		; right outline
		moveq	#2,d0
		move.l	a2,a0
		bsr	.line
		; Fill with blitter
		BLIT_FILL BAR_H,BAR_BW

		; Highlight stroke
		moveq	#3,d0
		lea	BAR_H*BAR_BW(a2),a0 ; next bpl
		; fall through to draw line
.line:
		moveq	#BAR_H/5-1,d7
.l:
		move.w	d0,d3
		asr.w	#3,d3
		move.w	d0,d4
		not.w	d4
		bset.b	d4,(a0,d3.w)
		bset.b	d4,BAR_BW(a0,d3.w)
		bset.b	d4,BAR_BW*2(a0,d3.w)
		lea	BAR_BW*5(a0),a0
		bset.b	d4,-BAR_BW*2(a0,d3.w)
		bset.b	d4,-BAR_BW(a0,d3.w)
		addq	#1,d0
		dbf	d7,.l
		rts


********************************************************************************
; Masked bob blit line to screen
;-------------------------------------------------------------------------------
; d0.w - x
;-------------------------------------------------------------------------------
BlitBar:
		move.l	Bar(pc),a0
		move.l	FrontPF(pc),a1
		lea	BARS_Y*SCREEN_BW(a1),a1
		lea	SCREEN_BPL*2(a1),a1 ; bpl 2
		move.l	Mask(pc),a2

		; bplcon0
		moveq	#$f,d1
		and.w	d0,d1
		ror.w	#4,d1
		or.w	#$fea,d1	; cookie cut

		; byte offset
		asr.w	#4,d0
		add.w	d0,d0
		lea	(a1,d0.w),a1
		lea	(a2,d0.w),a2

		move.w	#SCREEN_BW-BAR_BW,d2 ; mod

		WAIT_BLIT
		move.w	d1,bltcon0(a6)
		clr.w	bltcon1(a6)
		move.l	#-1,bltafwm(a6)
		clr.w	bltamod(a6)
		move.w	d2,bltbmod(a6)
		move.w	d2,bltcmod(a6)
		move.w	d2,bltdmod(a6)
		movem.l	a1-a2,bltcpt(a6)
		movem.l	a0-a1,bltapt(a6)
		move.w	#BAR_H*64+BAR_BW/2,bltsize(a6)

		lea	-SCREEN_BPL(a1),a1 ; bpl 1
		WAIT_BLIT
		movem.l	a1-a2,bltcpt(a6)
		move.l	a1,bltdpt(a6)
		move.w	#BAR_H*64+BAR_BW/2,bltsize(a6)
		rts


********************************************************************************
; Blit top box
;-------------------------------------------------------------------------------
; d0.w - y
;-------------------------------------------------------------------------------
BlitTop:
		move.l	Top(pc),a0
		move.l	FrontPF(pc),a1
		move.l	Mask(pc),a2

		sub.w	#MASK_INCLINE,d0
		muls	#SCREEN_BW,d0
		blt	.neg
		lea	(a0,d0.w),a0
		lea	(a1,d0.w),a1
		bra	.blit
.neg:
		neg.w	d0
		lea	(a2,d0.w),a2
.blit:
		WAIT_BLIT
		move.l	#$fea0000,bltcon0(a6)
		move.l	#-1,bltafwm(a6)
		clr.l	bltcmod(a6)
		clr.l	bltamod(a6)
		movem.l	a0/a1,bltapt(a6)
		movem.l	a1/a2,bltcpt(a6)
		move.w	#(MASK_INCLINE+TOP_BLIT_H)*64+SCREEN_BW/2,bltsize(a6)
		rts


********************************************************************************
; Horizontal 5:1 line for blitter fill
;-------------------------------------------------------------------------------
; a0 - screen
; d0.w - x1
; d1.w - x2
;-------------------------------------------------------------------------------
LineHBlit:
.l:
		cmp.w	d1,d0
		bgt	.done
		move.w	d0,d3
		asr.w	#3,d3
		move.w	d0,d4
		not.w	d4
		bset.b	d4,(a0,d3.w)
		lea	-SCREEN_BW(a0),a0
		add.w	#5,d0
		bra	.l
.done:
		rts


********************************************************************************
; Horizontal 5:1 line
;-------------------------------------------------------------------------------
; a0 - screen
; d0.w - x1
; d1.w - x2
; d2.w - initial step size
;-------------------------------------------------------------------------------
LineH:
		tst.w	d2
		beq	.l
; first seg
		subq	#1,d2
.l0:
		move.w	d0,d3
		asr.w	#3,d3
		move.w	d0,d4
		not.w	d4
		bset.b	d4,(a0,d3.w)
		addq	#1,d0
		dbf	d2,.l0
		lea	-SCREEN_BW(a0),a0
; main loop
.l:
		rept	5
		move.w	d0,d3
		asr.w	#3,d3
		move.w	d0,d4
		not.w	d4
		bset.b	d4,(a0,d3.w)
		addq	#1,d0
		cmp.w	d1,d0
		bgt	.done
		endr
		lea	-SCREEN_BW(a0),a0
		bra	.l
.done:		rts


********************************************************************************
; Horizontal 5:1 line
;-------------------------------------------------------------------------------
; a0 - screen
; d0.w - x1
; d1.w - h
; d2.w - initial step size
;-------------------------------------------------------------------------------
LineV:
		move.w	d2,d5		; d5 = y pos (used to get remainder)
		beq	.l
; first seg
		subq	#1,d2
		move.w	d0,d3
		asr.w	#3,d3
		move.w	d0,d4
		not.w	d4
.l0:
		bset.b	d4,(a0,d3.w)
		lea	SCREEN_BW(a0),a0
		dbf	d2,.l0
		addq	#1,d0
; main loop
.l:
		addq	#5,d5
		move.w	d0,d3
		asr.w	#3,d3
		move.w	d0,d4
		not.w	d4
		cmp.w	d1,d5
		beq	.done
		bge	.remainder
		bset.b	d4,(a0,d3.w)
		bset.b	d4,SCREEN_BW(a0,d3.w)
		bset.b	d4,SCREEN_BW*2(a0,d3.w)
		lea	SCREEN_BW*5(a0),a0
		bset.b	d4,-SCREEN_BW*2(a0,d3.w)
		bset.b	d4,-SCREEN_BW(a0,d3.w)
		addq	#1,d0
		bra	.l
.remainder:
		subq	#5-1,d5		; -1 for dbf
		sub.w	d5,d1
.l1:
		bset.b	d4,(a0,d3.w)
		lea	SCREEN_BW(a0),a0
		dbf	d1,.l1
.done:
		rts


********************************************************************************
BlitClear:
		WAIT_BLIT
		clr.w	bltdmod(a6)
		move.l	#$01000000,bltcon0(a6)
		move.l	a0,bltdpt(a6)
		move.w	d0,bltsize(a6)
		rts

********************************************************************************
BlitFill:
		WAIT_BLIT
		move.l	#$09f0000a,bltcon0(a6)
		move.l	#-1,bltafwm(a6)
		clr.l	bltamod(a6)
		move.l	a0,bltapt(a6)
		move.l	a0,bltdpt(a6)
		move.w	d0,bltsize(a6)
		rts


********************************************************************************
BlitLogo:
		move.l	FrontPF(pc),a0
		lea	SCREEN_BW*30+7(a0),a0
		move.w	#LOGO_H*64+LOGO_BW/2,d0
		WAIT_BLIT
		move.l	Logo(pc),a1
		move.l	#$9f00000,bltcon0(a6)
		move.l	#-1,bltafwm(a6)
		clr.w	bltamod(a6)
		move.w	#SCREEN_BW-LOGO_BW,bltdmod(a6)
		move.l	a1,bltapt(a6)
		move.l	a0,bltdpt(a6)
		move.w	d0,bltsize(a6)
		WAIT_BLIT
		lea	SCREEN_BPL(a0),a0
		lea	LOGO_BPL(a1),a1
		move.l	a1,bltapt(a6)
		move.l	a0,bltdpt(a6)
		move.w	d0,bltsize(a6)
		WAIT_BLIT
		lea	SCREEN_BPL(a0),a0
		lea	LOGO_BPL(a1),a1
		move.l	a1,bltapt(a6)
		move.l	a0,bltdpt(a6)
		move.w	d0,bltsize(a6)
		rts


********************************************************************************
; Prepare common blit regs for line draw
;-------------------------------------------------------------------------------
InitDrawLine:
		WAIT_BLIT
		move.w	#SCREEN_BW,bltcmod(a6)
		move.l	#-$8000,bltbdat(a6)
		move.l	#-1,bltafwm(a6)
		rts


********************************************************************************
; Based on TEC
;-------------------------------------------------------------------------------
; d0.w - x1
; d1.w - y1
; d2.w - x2
; d3.w - y2
; a0 - Draw buffer
; a6 - Custom
;-------------------------------------------------------------------------------
DrawLine:
		movem.w	d0-d5,-(sp)
		cmp.w	d1,d3
		bgt.s	.l0
		beq.s	.done
		exg	d0,d2
		exg	d1,d3
.l0:		moveq	#0,d4
		move.w	d1,d4
		muls	#SCREEN_BW,d4
		move.w	d0,d5
		add.l	a0,d4
		asr.w	#3,d5
		ext.l	d5
		add.l	d5,d4		; fix - was word but needs to be long for high screen addresses
		moveq	#0,d5
		sub.w	d1,d3
		sub.w	d0,d2
		bpl.s	.l1
		moveq	#1,d5
		neg.w	d2
.l1:		move.w	d3,d1
		add.w	d1,d1
		cmp.w	d2,d1
		dbhi	d3,.l2
.l2:		move.w	d3,d1
		sub.w	d2,d1
		bpl.s	.l3
		exg	d2,d3
.l3:		addx.w	d5,d5
		add.w	d2,d2
		move.w	d2,d1
		sub.w	d3,d2
		addx.w	d5,d5
		and.w	#15,d0
		ror.w	#4,d0
		; or.w	#$a4a,d0	; or
		or.w	#$bca,d0	; xor

		WAIT_BLIT
		move.w	d2,bltaptl(a6)
		sub.w	d3,d2
		lsl.w	#6,d3
		addq.w	#2,d3
		move.w	d0,bltcon0(a6)
		move.b	.oct(pc,d5.w),bltcon1+1(a6)
		move.l	d4,bltcpt(a6)
		move.l	d4,bltdpt(a6)
		movem.w	d1/d2,bltbmod(a6)
		move.w	d3,bltsize(a6)
.done:
		movem.w	(sp)+,d0-d5
		rts
; .oct:		dc.b	3,3+64,19,19+64,11,11+64,23,23+64 ; onedot
.oct:		dc.b	1,1+64,17,17+64,9,9+64,21,21+64 ; normal


********************************************************************************
ClearScreen:
		WAIT_BLIT
		clr.w	bltdmod(a6)
		move.l	#$01000000,bltcon0(a6)
		move.l	a0,bltdpt(a6)
		move.w	#SCREEN_H*PF_BPLS*64+SCREEN_BW/2,bltsize(a6)
		rts


********************************************************************************
C2P_3bpl:
		move.l	#$55555555,d3
		move.l	#$33333333,d4
		move.l	#$00ff00ff,d5
.l:
		move.l	(a0)+,d0
		lsl.l	#4,d0
		or.l	(a0)+,d0
		move.l	(a0)+,d1
		lsl.l	#4,d1
		or.l	(a0)+,d1

; a3a2a1a0e3e2e1e0 b3b2b1b0f3f2f1f0 c3c2c1c0g3g2g1g0 d3d2d1d0h3h2h1h0
; i3i2i1i0m3m2m1m0 j3j2j1j0n3n2n1n0 k3k2k1k0o3o2o1o0 l3l2l1l0p3p2p1p0

		move.l	d1,d2
		lsr.l	#8,d2
		eor.l	d0,d2
		and.l	d5,d2
		eor.l	d2,d0
		lsl.l	#8,d2
		eor.l	d2,d1

; a3a2a1a0e3e2e1e0 i3i2i1i0m3m2m1m0 c3c2c1c0g3g2g1g0 k3k2k1k0o3o2o1o0
; b3b2b1b0f3f2f1f0 j3j2j1j0n3n2n1n0 d3d2d1d0h3h2h1h0 l3l2l1l0p3p2p1p0

		move.l	d1,d2
		lsr.l	#1,d2
		eor.l	d0,d2
		and.l	d3,d2
		eor.l	d2,d0
		add.l	d2,d2
		eor.l	d2,d1

; a3b3a1b1e3f3e1f1 i3j3i1j1m3n3m1n1 c3d3c1d1g3h3g1h1 k3l3k1l1o3p3o1p1
; a2b2a0b0e2f2f0f0 i2j2i0j0m2n2m0n0 c2d2c0d0g2h2g0h0 k2l2k0l0o2p2o0p0

		move.w	d1,d2
		move.w	d0,d1
		swap	d1
		move.w	d1,d0
		move.w	d2,d1

; a3b3a1b1e3f3e1f1 i3j3i1j1m3n3m1n1 a2b2a0b0e2f2f0f0 i2j2i0j0m2n2m0n0
; c3d3c1d1g3h3g1h1 k3l3k1l1o3p3o1p1 c2d2c0d0g2h2g0h0 k2l2k0l0o2p2o0p0

		move.l	d1,d2
		lsr.l	#2,d2
		eor.l	d0,d2
		and.l	d4,d2
		eor.l	d2,d0
		lsl.l	#2,d2
		eor.l	d2,d1

; a3b3c3d3e3f3g3h3 i3j3k3l3m3n3o3p3 a2b2c2d2e2f2g2h2 i2j2k2l2m2n2o2p2
; a1b1c1d1e1f1g1h1 i1j1k1l1m1n1o1p1 a0b0c0d0e0f0g0h0 i0j0k0l0m0n0o0p0

		move.w	d1,(a1)+
		swap	d1
		move.w	d1,(a2)+
		move.w	d0,(a3)+

		dbf	d7,.l
		rts


********************************************************************************
Vars:
********************************************************************************

BacklightBrightness:
		dc.w	0		; step 0-$4000

BackPF:		dc.l	0
FrontPF:	dc.l	0
FrontScrollPt:	dc.l	0

NumSprs:	dc.l	0

Top:		dc.l	0
Mask:		dc.l	0
Bar:		dc.l	0
Logo:		dc.l	0

LineBob:	dc.l	0

MarkerSpr:	dc.l	Marker1
MarkerPos:	dc.w	0
MarkerX:	dc.w	0
MarkerY:	dc.w	0

MarkerCol:	dc.w	COL_ORANGE
NumberCol:	dc.w	COL_LIGHT_BLUE

ScrollPos:	dc.w	0

MarkerSprites:	dc.l	Marker1,Marker2,Marker3

PalBack:	dc.l	BlankData
PalFront:	dc.l	BlankData

PalData:	dc.l	0
PalPtrs:	ds.l	32


********************************************************************************
; Data
********************************************************************************

Pal:
		dc.w	$000		; black BG	0
		dc.w	$222		; dark grey	1
		dc.w	$677		; light grey	2
		dc.w	$677		; light grey	1+2
		dc.w	$333		; mid grey	4
PalBlue:	dc.w	$000		; blue		4+1
		dc.w	$000		; UNUSED	4+2
PalLightBlue:	dc.w	$000		; light blue	4+2+1

PalTrails:
		dc.w	0
		dc.w	COL_DARK_BLUE
		dc.w	COL_MID_BLUE
		dc.w	COL_MID_BLUE
		dc.w	$677		; light grey
		dc.w	0
		dc.w	0
		dc.w	COL_BLUE

PalTrailsLight:
		dc.w	COL_MID_BLUE
		dc.w	$134
		dc.w	$478
		dc.w	$478
		dc.w	$677		; light grey
		dc.w	0
		dc.w	0
		dc.w	$6bd

PalTrailsFlash:
		dc.w	$6bd
		dc.w	$6bd
		dc.w	$6bd
		dc.w	$6bd
		dc.w	$677		; light grey
		dc.w	0
		dc.w	0
		dc.w	$6bd

PalTrailsOff:
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	$677		; light grey
		dc.w	0
		dc.w	0
		dc.w	0

MarkerCols:
		dc.w	$fff,$fff,$fee,$fdd,$fdc,$fcb,$fba,$fb9
		dc.w	$fa8,$f97,$f86,$f85,$f74,$f64,$f63,$e52


PalLogo:
		incbin	data/radio-logo.PAL

; PalLogoFlash:
; 	dc.w 0,$049,$06b,$08d,$0bf,$4ff,$fff,0


*******************************************************************************
		data_c
*******************************************************************************

RightSeg:	ds.l	5

Numbers:
		incbin	data/radio-numbers.BPL
LogoChunky:
		incbin	data/radio-logo.chk

Marker1:
		incbin	data/marker-1.SPR
Marker2:
		incbin	data/marker-2.SPR
Marker3:
		incbin	data/marker-3.SPR
Marker4:
		incbin	data/marker-4.SPR
Marker5:
		incbin	data/marker-5.SPR

Cop:
		dc.w	diwstrt,DIW_YSTRT<<8!DIW_XSTRT
		dc.w	diwstop,(DIW_YSTOP-256)<<8!(DIW_XSTOP-256)
		dc.w	ddfstrt,(DIW_XSTRT-17)>>1&$fc
		dc.w	ddfstop,(DIW_XSTRT-17+(DIW_W>>4-1)<<4)>>1&$fc-SCROLL*8
		dc.w	bpl1mod,DIW_MOD
		dc.w	bpl2mod,DIW_MOD
		dc.w	bplcon0,(BPLS<<12)!(DPF<<10)!$200
CopScroll:	dc.w	bplcon1,0
		dc.w	bplcon2,%1100100
CopBplPt:	rept	BPLS*2
		dc.w	bpl0pt+REPTN*2,0
		endr
CopSprPt:
		rept	8*2
		dc.w	sprpt+REPTN*2,0
		endr

		dc.l	-2
CopE:
