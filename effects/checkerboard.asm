		include	_main.i
		include	effects/checkerboard.i

********************************************************************************
* Rotating checkerboard effect
********************************************************************************

; Uses line patterns with scale/skew across 4 bitplanes to get bottom two layers,
; and draws the final layer on bpl 5.
;
; Bottom layers:
;-------------------------------------------------------------------------------
;
; We have 4 layers of rotating stripes, which the palette XORs together to get
; the grid pattern.
;
; PF2 (bpls 2/4) displays one stripe from each layer at the initial angle (0-90deg),
; setting X the position using modulo and horizontal scroll per line in the copper.
;
; PF1 (bpls 1/3) displays the alternate stripes, which are angled +90 degrees.
; Because we want to use bitplane 5 independently, we can't use the horizontal
; scroll register. Instead we need to set bpl pointers per line in the copper and
; have pre-shifted versions of each pattern.
;
; The pattern consists of a single row of alternating spans around the center.
; The width of the span depends on the angle and scale. These are too numerous
; to precompute, so we need to generate the four patterns each frame, and then
; create pre-shifted versions of half of them.
;
; spanWidth = gridWidth/sin(a)
;
; skewIncrement = 1/tan(a)
; The pattern is scrolled by this much every line to achieve the skew.
; To be centered it needs to start at -skewIncrement*(screenHeight/2).
;
; Creating the preshifted patterns is expensive. It takes time to blit and takes
; up a lot of chip RAM. The width of the pattern increases as the angle (and skew)
; increases. In fact they get exponentially large as the angle apporaches zero,
; as we are close to dividing by zero for the span size. We can take advantage of
; this and swap the angles so that PF2 gets the larger patterns, and can use
; hardware scroll, which avoids preshifting.
;
; The data that goes into the copper can be precalced per angle.
; For the pf2 modulo/scroll this is always the same, regardless of the scale.
; For the pf1 bitplane pointers, the deltas are always the same, but the starting
; address varies.
; These go in tables: Pf2ModLists and Pf1Deltas.
;
;
; Top layer:
;-------------------------------------------------------------------------------
;
; We add an additional layer in bpl 5. Because we only have bitplane left, XOR
; tricks go out of the window and we need to actually draw and fill the grid
; pattern with the blitter. This is by far the most expensive part of the routine.
;
; Where the lines clip with the right hand side of the screen, we need vertical
; lines for alternate spans to allow the blitter fill mode to fill the 'blocks'
; correctly. The problem is knowing which blocks should be filled based on where
; they intersect. Mapping the alternating pattern to the line intersections is...
; challenging. Instead, because we know the center should always be unfilled, we
; can count the pixels from the center to the right hand edge and set the initial
; fill state based on whether this is odd or even.
;
; The screen buffer for this is cleared with the CPU to get some parallelisation
; with the blitter. This requires that we use triple buffering
;
; Basic math to draw grid line top-to-bottom is:
; xo=x/math.cos(a)+cx
; ty=math.tan(a)*cy
; line(xo+ty,0,xo-ty,h,1)
;
; where x intersect is out of bounds, rotate 90deg and draw to left-to-right:
; yo=x/math.sin(a)+cy
; tx=math.tan(a+math.pi/2)*cx
; line(0,yo-tx,w,yo+tx,1)

; TODO:
; - Off-by-one on palette
; - calc grid reps

********************************************************************************
* Constants:
********************************************************************************

; Display window:
DIW_W = 288
DIW_H = 201				; multiple of unroll + 1
; don't forget to adjust remainder in ClearGrid

BPLS = 5

; Used to determine number of rows/cols needed to cover screen
DIAGONAL = 370				; sqrt(DIW_W*DIW_W+DIW_H*DIW_H)

VPAN = 48				; Maximum pos/neg y pan on layers 2/3

MAX_X = DIW_W-1

ROT_STEPS = 64

DEPTH_STEPS_POW = 7
DEPTH_STEPS = 1<<DEPTH_STEPS_POW
DEPTH_DIST = 25
DEPTH_FP = 5
DEPTH_MAX = 400<<DEPTH_FP

; Need to define line buffer as a 2D space to allow blitting
LINE_H = 12				; Enough space for widest line
LINE_BW = 126				; Max blit width
LINE_SZ = LINE_BW*LINE_H

; Size of buffer for initial pattern data, calculated to handle worst case
; Check if updating LINE_SZ
PAT_BUF_SIZE = $cec
; Offset between preshifted groups
PAT_SHIFT_OFFSET = $200

;-------------------------------------------------------------------------------
; Derived

COLORS = 1<<BPLS

DIW_BW = DIW_W/16*2
DIW_MOD = DIW_BW+2
DIW_XSTRT = ($242-DIW_W)/2
DIW_YSTRT = ($158-DIW_H)/2
DIW_XSTOP = DIW_XSTRT+DIW_W
DIW_YSTOP = DIW_YSTRT+DIW_H


Script:
		dc.l	0,CmdMoveIW,$400*3000/BPM,DepthSpeed
		; dc.l	7*3000/BPM,CmdLerpWord,-$1000,2,RotSpeed
		dc.l	14*3000/BPM,CmdLerpWord,-$400*3000/BPM,6,DepthSpeed
		dc.l	23*3000/BPM,CmdLerpWord,-$1000,2,RotSpeed
		dc.l	30*3000/BPM,CmdLerpWord,$400*3000/BPM,6,DepthSpeed
		dc.l	39*3000/BPM,CmdLerpWord,$1000,2,RotSpeed
		dc.l	46*3000/BPM,CmdLerpWord,-$400*3000/BPM,6,DepthSpeed
		dc.l	$8000


********************************************************************************
CB_Pre:
********************************************************************************
		jsr	MemFlip

		bsr	InitLineMuls

		ALLOC_PUBLIC (DIW_H+VPAN*2)*ROT_STEPS*4
		move.l	a0,OffsetData
		ALLOC_PUBLIC (DIW_H+VPAN*2)*ROT_STEPS*2
		move.l	a0,Pf1Deltas
		ALLOC_PUBLIC (DIW_H+VPAN*2)*ROT_STEPS*4
		move.l	a0,Pf2ModData

		bsr	InitOffsets
		bsr	InitPf1Deltas
		bsr	InitPf2Mods

		rts


********************************************************************************
CB_Effect:
********************************************************************************
		jsr	SetBgTask

		move.w	#$fff,color(a6)

		lea	Script,a0
		jsr	Commander_Init

		jsr	MemFreeLast

		; dots:
		ALLOC_CHIP LINE_SZ
		move.l	a0,Dots
		; Clear
		WAIT_BLIT
		move.l	#$1000000,bltcon0(a6)
		clr.w	bltdmod(a6)
		move.l	a0,bltdpt(a6)
		move.w	#LINE_H*64+LINE_BW/2,bltsize(a6)

		; patterns:
		ALLOC_CHIP_ALIGNED PAT_BUF_SIZE+PAT_SHIFT_OFFSET*15
		move.l	a0,DrawPatterns
		ALLOC_CHIP_ALIGNED PAT_BUF_SIZE+PAT_SHIFT_OFFSET*15
		move.l	a0,ViewPatterns

		; copper
		ALLOC_CHIP Cop_SizeOf
		move.l	a0,DrawCopList
		bsr	InitCopList
		ALLOC_CHIP Cop_SizeOf
		move.l	a0,ViewCopList
		bsr	InitCopList

		; screens
		ALLOC_CHIP DIW_BW*DIW_H*3
		move.l	a0,ClearScreen
		lea	DIW_BW*DIW_H(a0),a0
		move.l	a0,ViewScreen
		lea	DIW_BW*DIW_H(a0),a0
		move.l	a0,DrawScreen
		; Clear draw screen
		WAIT_BLIT
		move.l	a0,bltdpt(a6)
		move.w	#DIW_H*64+DIW_BW/2,bltsize(a6)

		PRINT_MEM_TOTALS

		jsr	WaitEOF
		bsr	SetBitplanes
		move.l	#Cop,cop1lc(a6)
		move.l	#CopEnd,cop2lc(a6)

;-------------------------------------------------------------------------------
MainLoop:
		lea	Vars(pc),a5
		lea	custom,a6

		bsr	Update
		bsr	SetDepth
		bsr	SetPan
		bsr	SetAngles
		bsr	CreatePatterns
		bsr	DrawGrid
		BLIT_UNHOG
		bsr	FillGrid
		bsr	SetBitplanes
		bsr	WriteCopperRows
		bsr	ClearGrid
		bsr	CreatePal
		bsr	SetPal
		bsr	FlipBuffers

		BLIT_HOG
		; WAIT_BLIT
		; move.w #$f00,$180(a6)
		jsr	VSyncWithBgTask

		jsr	PartOver
		blt	MainLoop
		rts


********************************************************************************
* Routines
********************************************************************************

Update:
		move.w	RotSpeed(pc),d0
		ext.l	d0
		lsl.l	#4,d0
		add.l	d0,Rotation-Vars(a5)

		; move.w	BpmFrame,d7
		; ; Linear movement
		; move.w	d7,d0
		; swap	d0
		; clr.w	d0
		;
		; ; Add sin movement
		; and.w	#SIN_MASK,d7
		; add.w	d7,d7
		; lea	Sin,a0
		; move.w	(a0,d7.w),d7
		; swap	d7
		; clr.w	d7
		; asr.l	#5,d7
		; add.l	d7,d0
		;
		; move.l	d0,Depth-Vars(a5)

		; add.l	#3000/BPM,Depth-Vars(a5)
		move.w	DepthSpeed(pc),d0
		ext.l	d0
		lsl.l	#4,d0
		add.l	d0,Depth-Vars(a5)

		rts


********************************************************************************
; Builds LUT of offsets per angle, split in byte/scroll word pairs
;-------------------------------------------------------------------------------
InitOffsets:
		lea	TanR,a0
		move.l	OffsetData(pc),a1
		lea	OffsetPtrs,a2
		move.w	#$f,d5
		move.w	#ROT_STEPS-1,d7
.step:
		move.l	(a0)+,d0	; d0 = inc
		move.l	a1,(a2)+
		move.l	d0,d1		; d1 = x
		asr.l	#7,d1
		muls	#-(DIW_H/2+VPAN)<<7,d1
		move.w	#DIW_H+VPAN*2-1,d6
.line:
		move.l	d1,d2
		swap	d2		; FP -> int
		move.w	d2,d3
		asr.w	#4,d3		; byte
		add.w	d3,d3
		neg.w	d3
		move.w	d3,(a1)+
		and.w	d5,d2		; scroll
		move.w	d2,(a1)+
		add.l	d0,d1
		dbf	d6,.line
		dbf	d7,.step
		rts


********************************************************************************
; Converts offsets table into deltas for PF1
; Each delta is a single word value to be added to the bpl address and includes
; jumps between pre-shifted patterns.
;-------------------------------------------------------------------------------
InitPf1Deltas:
		move.l	Pf1Deltas(pc),a0
		lea	Pf1DeltaPtrs,a1
		lea	OffsetPtrs,a2
		move.w	#ROT_STEPS-1,d7
.step:
		move.l	a0,(a1)+	; store ptr
		move.l	(a2)+,a3	; offsets

		; initial (current) offset
		move.w	(a3)+,d0	; bytes
		move.w	(a3)+,d2	; scroll
		ror.w	#7,d2		; conv scroll to preshift offset
		add.w	d2,d0

		move.w	#DIW_H+VPAN*2-2,d6
.line:
		move.w	(a3)+,d1	; bytes
		move.w	(a3)+,d2	; scroll
		ror.w	#7,d2		; conv scroll to preshift offset
		add.w	d2,d1

		; delta
		move.w	d1,d4
		sub.w	d0,d4
		move.w	d4,(a0)+

		move.w	d1,d0		; update current offset

		dbf	d6,.line
		dbf	d7,.step
		rts


********************************************************************************
; Generates LUT for modulo and hscroll values used in copper for PF2
;-------------------------------------------------------------------------------
InitPf2Mods:
		move.l	Pf2ModData(pc),a0
		lea	Pf2ModPtrs,a1
		lea	OffsetPtrs,a2
		moveq	#0,d0
		move.w	#ROT_STEPS-1,d7
.step:
		move.l	a0,(a1)+	; store ptr
		move.l	(a2,d0.w),a3	; pf2 offsets
		move.l	(a3)+,d4	; pf2 initial bytes / scroll
; scroll: set initial value
		lsl.w	#4,d4
		move.w	d4,(a0)+
		swap	d4		; Swap to initial byte offsets in lower word

		move.w	#DIW_H+VPAN*2-2,d6
.line:
		movem.w	(a3)+,d2-d3	; d2 = pf2 bytes, d3 = scroll
; modulo
		move.w	d2,a5		; offset delta from prev line
		sub.w	d4,a5
		move.w	d2,d4		; update last offset
		sub.w	#DIW_MOD,a5
		move.w	a5,(a0)+
; scroll
		lsl.w	#4,d3
		move.w	d3,(a0)+
		dbf	d6,.line

		addq	#4,d0

		dbf	d7,.step
		rts


********************************************************************************
; Builds template for double buffered copper lists
;-------------------------------------------------------------------------------
; a0 - Copper list
;-------------------------------------------------------------------------------
InitCopList:
		; bpl ptrs:
		move.w	#bpl0pt,d0
		moveq	#BPLS*2-1,d7
.bpl:		move.w	d0,(a0)+
		clr.w	(a0)+
		addq	#2,d0
		dbf	d7,.bpl

		move.w	#color,d0
		moveq	#COLORS-1,d7
.col:		move.w	d0,(a0)+
		clr.w	(a0)+
		addq	#2,d0
		dbf	d7,.col

		; wait y start
		move.w	#(DIW_YSTRT&$ff)<<8+5,(a0)+
		move.w	#$fffe,(a0)+

		; rows:
		move.w	#DIW_H-2,d6
.line:
		; pf2 scroll:
		move.l	#bplcon1<<16,(a0)+ ; 4
		; pf2 modulo:
		move.l	#bpl2mod<<16,(a0)+ ; 4
		; wait EOL
		move.w	#DIW_YSTRT+DIW_H-2,d2
		sub.w	d6,d2
		and.w	#$80,d2
		move.b	d2,(a0)+	; 1
		move.b	#$df,(a0)+	; 1
		move.w	#$80fe,(a0)+	; 2
		; pf1:
		move.l	#bpl0ptl<<16,(a0)+ ; 4
		move.l	#bpl2ptl<<16,(a0)+ ; 4
		; total 20 per line
		dbf	d6,.line

		move.l	#-2,(a0)+	; +8
		rts

; Copper structs:

		rsreset
Cop_Bpls	rs.l	BPLS*2
Cop_Cols	rs.l	COLORS
Cop_WaitY	rs.l	1
Cop_Rows	rs.b	CopRow_SizeOf*(DIW_H-1)
Cop_End		rs.l	1
Cop_SizeOf	rs.b	0

		rsreset
		rs.w	1		; bplcon1 reg
CopRow_Scroll	rs.w	1		; bplcon1 value
		rs.w	1		; bpl2mod reg
CopRow_Mod	rs.w	1		; bpl2mod value
		rs.l	1		; wait
		rs.w	1		; bpl reg
CopRow_Bpl0ptl	rs.w	1		; bpl value
		rs.w	1		; bpl reg
CopRow_Bpl2ptl	rs.w	1		; bpl value
CopRow_SizeOf	rs.b	0


********************************************************************************
; Creates LUT for screen height multiply, used by DrawLine routine
;-------------------------------------------------------------------------------
InitLineMuls:
		lea	LineMuls(pc),a0
		moveq	#0,d0
		move.w	#DIW_H-1,d7
.l:		move.w	d0,(a0)+
		add.w	#DIW_BW,d0
		dbf	d7,.l
		rts


********************************************************************************
; Set grid width for each layer based on depth
; Populates LayerXW varaibles
;-------------------------------------------------------------------------------
SetDepth:
		move.w	Depth(pc),d1
		and.w	#DEPTH_STEPS-1,d1
		add.w	#DEPTH_DIST,d1
		move.l	#DEPTH_DIST*DEPTH_MAX,d2

		move.w	d1,Layer1Depth-Vars(a5)
		move.l	d2,d0
		divu	d1,d0
		move.w	d0,Layer1Width-Vars(a5)
		moveq	#1,d3
		bsr	CalcReps
		move.w	d7,Layer1Reps-Vars(a5)

		add.w	#DEPTH_STEPS,d1
		move.w	d1,Layer2Depth-Vars(a5)
		move.l	d2,d0
		divu	d1,d0
		move.w	d0,Layer2Width-Vars(a5)
		moveq	#1,d3
		bsr	CalcReps
		move.w	d7,Layer2Reps-Vars(a5)

		add.w	#DEPTH_STEPS,d1
		move.w	d1,Layer3Depth-Vars(a5)
		move.l	d2,d0
		divu	d1,d0
		move.w	d0,Layer3Width-Vars(a5)
		moveq	#2,d3
		bsr	CalcReps
		move.w	d7,Layer3Reps-Vars(a5)

		rts


********************************************************************************
; Set x/y panning from depth
;-------------------------------------------------------------------------------
SetPan:
		lea	Sin,a0

		; x pan
		move.w	Depth(pc),d0
		lsl.w	#2,d0
		and.w	#SIN_MASK*2,d0
		move.w	(a0,d0.w),d0
		asr.w	#6,d0

		move.w	d0,d1
		muls	Layer1Depth(pc),d1
		; divs	#DEPTH_STEPS*3+DEPTH_DIST,d1
		asl.l	#7,d1		; approximation for max depth: d/512
		swap	d1
		move.w	d1,Layer1X-Vars(a5)

		move.w	d0,d1
		muls	Layer2Depth(pc),d1
		; divs	#DEPTH_STEPS*3+DEPTH_DIST,d1
		asl.l	#7,d1
		swap	d1
		move.w	d1,Layer2X-Vars(a5)

		move.w	d0,d1
		muls	Layer3Depth(pc),d1
		; divs	#DEPTH_STEPS*3+DEPTH_DIST,d1
		asl.l	#7,d1
		swap	d1
		move.w	d1,Layer3X-Vars(a5)

		; y pan
		move.w	Depth(pc),d0
		lsr.w	#1,d0
		add.w	#$400,d0
		and.w	#SIN_MASK*2,d0
		move.w	(a0,d0.w),d0
		asr.w	#8,d0
		asr.w	#1,d0

		move.w	d0,Layer1Y-Vars(a5)
		move.w	d0,Layer23Y-Vars(a5)

		rts


********************************************************************************
; Calculate number of pattern repetitions to fill screen
;-------------------------------------------------------------------------------
; d0 - width
; d3 - additional reps
; returns:
; d7  reps
;-------------------------------------------------------------------------------
CalcReps:
; Number of repetitions:
; d = screen diagonal (max width)
; (d + x/2) / (4*x)
		move.w	d0,d7
		ext.l	d7
		add.l	#(DIAGONAL*2)<<DEPTH_FP,d7 ; double for fp
		move.w	d0,d4
		lsl.w	#2,d4
		divu	d4,d7
		addq	#1,d7		; round up
		lsr.w	d7
		; Extra rep for pan
		add.w	d3,d7
		rts


********************************************************************************
; Create palette data
; Populates ColsOutRGB
;-------------------------------------------------------------------------------
CreatePal:
		lea	ColsIn(pc),a0
		lea	ColsOut(pc),a1
		move.w	Depth(pc),d4

		; Select start color from table for first layer
		move.w	d4,d5
		asr.w	#DEPTH_STEPS_POW,d5 ; depth>>pow
		and.w	#$f,d5		; color index (mod 16 cols in table)
		; mulu #6,d5
		move.w	d5,d6		; optimise out mulu
		add.w	d6,d6		; *2
		lsl.w	#2,d5		; *4
		add.w	d6,d5
		lea	(a0,d5.w),a0

		; Set initial fade step
		and.w	#DEPTH_STEPS-1,d4
		move.w	#DEPTH_STEPS,d3
		sub.w	d4,d3
		swap	d3
		clr.w	d3
		divu	#DEPTH_STEPS*3*4,d3

; Generate base color for each layer, faded according to depth
		moveq	#3-1,d7
.l:
		movem.w	(a0)+,d0-d2
		bsr	FadeRGBComponentsToWhite
		move.w	d0,(a1)+
		move.w	d1,(a1)+
		move.w	d2,(a1)+
		add.w	#$8000/3,d3	; increment fade step
		dbf	d7,.l

		; Blend 1+2
		movem.w	C1(pc),d0-d2
		movem.w	C2(pc),d3-d5
		bsr	BlendRGBComponents
		movem.w	d0-d2,C12

		; Blend 1+2+3
		; (1+2 already in d0-d2)
		movem.w	C3(pc),d3-d5
		bsr	BlendRGBComponents
		movem.w	d0-d2,C123

		; Blend 1+3
		movem.w	C1(pc),d0-d2
		movem.w	C3(pc),d3-d5
		bsr	BlendRGBComponents
		movem.w	d0-d2,C13

		; Blend 2+3
		movem.w	C2(pc),d0-d2
		movem.w	C3(pc),d3-d5
		bsr	BlendRGBComponents
		movem.w	d0-d2,C23

; Convert component values to RGB word
		lea	ColsOut,a0
		lea	ColsOutRGB,a1
		moveq	#7-1,d7
.l1:
		movem.w	(a0)+,d0-d2
		move.b	d0,(a1)+
		lsl.w	#4,d1
		or.w	d1,d2
		move.b	d2,(a1)+
		dbf	d7,.l1

		rts


********************************************************************************
; Fade from white to colour value
; mutates input
;-------------------------------------------------------------------------------
; d0.w - red
; d1.w - green
; d2.w - blue
; d3.w - fade amount 0-$8000
;-------------------------------------------------------------------------------
FadeRGBComponentsToWhite:
		mulu	d3,d0
		mulu	d3,d1
		mulu	d3,d2

		move.w	#$8000,d5
		sub.w	d3,d5
		moveq	#$f,d4
		mulu	d5,d4

		add.l	d4,d0
		add.l	d0,d0
		swap	d0
		add.l	d4,d1
		add.l	d1,d1
		swap	d1
		add.l	d4,d2
		add.l	d2,d2
		swap	d2

		rts


********************************************************************************
; Multiply two colours
; mutates colour 1 input
;-------------------------------------------------------------------------------
; d0.w - colour 1 red
; d1.w - colour 1 green
; d2.w - colour 1 blue
; d3.w - colour 2 red
; d4.w - colour 2 green
; d5.w - colour 2 blue
;-------------------------------------------------------------------------------
MultiplyRGBComponents:
		mulu	d3,d0
		; divu	#15,d0
		lsr.w	#4,d0
		mulu	d4,d1
		; divu	#15,d1
		lsr.w	#4,d1
		mulu	d5,d2
		; divu	#15,d2
		lsr.w	#4,d2
		rts


********************************************************************************
BlendRGBComponents:
		; movem.w	d3-d5,-(sp)
		; move.w	#$6000,d3
		; bsr	FadeRGBComponentsToWhite
		; movem.w	(sp)+,d3-d5
		bra	MultiplyRGBComponents


********************************************************************************
; Set palette in copperlist
; Looks up colour for each layer and sets colour registers for XOR layering
;-------------------------------------------------------------------------------
SetPal:
		move.w	#$fff,d0	; bg
		movem.w	ColsOutRGB(pc),d1-d7
; d1 = 1
; d2 = 2
; d3 = 3
; d4 = 1+2
; d5 = 1+2+3
; d6 = 1+3
; d7 = 2+3

; Hide layers:
		move.w	Depth(pc),a0
; layer 3 off?
		cmp.w	#0,a0
		bge	.l3on
		move.w	d0,d3		; bg -> 3
		move.w	d0,d5		; bg -> 1+2+3
		move.w	d0,d6		; bg -> 1+3
		move.w	d0,d7		; bg -> 2+3
.l3on:
; layer 2 off?
		cmp.w	#DEPTH_STEPS,a0
		bge	.l2on
		move.w	d0,d2		; bg -> 2
		move.w	d1,d4		; 1 -> 1+2
		move.w	d6,d5		; 1+3 -> 1+2+3
		move.w	d3,d7		; 3 -> 2+3
.l2on:
; layer 1 off?
		cmp.w	#DEPTH_STEPS*2,a0
		bge	.l1on
		move.w	d0,d1		; bg -> 1
		move.w	d2,d4		; 2 -> 1+2
		move.w	d7,d5		; 2+3 -> 1+2+3
		move.w	d3,d6		; 3 -> 1+3
.l1on:

		move.l	DrawCopList,a0
		lea	Cop_Cols(a0),a0
		move.w	d0,2(a0)	; 00000 none set		BG
		move.w	d2,6(a0)	; 00001 1 set			2
		move.w	d2,10(a0)	; 00010 2 set			2
		move.w	d0,14(a0)	; 00011 1/2 cancel		BG
		move.w	d1,18(a0)	; 00100 4 set			1
		move.w	d4,22(a0)	; 00101 1 set (priority)	1+2
		move.w	d4,26(a0)	; 00110 2 set (priority)	1+2
		move.w	d1,30(a0)	; 00111 4 set, 1/2 cancel	1
		move.w	d1,34(a0)	; 01000 8 set			1
		move.w	d4,38(a0)	; 01001 1 set (priority)	1+2
		move.w	d4,42(a0)	; 01010 2 set (priority)	1+2
		move.w	d1,46(a0)	; 01011 1/2 cancel, 8 set	1
		move.w	d0,50(a0)	; 01100 4/8 cancel		BG
		move.w	d2,54(a0)	; 01101 1 set			2
		move.w	d2,58(a0)	; 01110 2 set			2
		move.w	d0,62(a0)	; 01111 4/8 AND 1/2 cancel	BG

		move.w	d3,66(a0)	; 10000 none set		3
		move.w	d7,70(a0)	; 10001 1 set			3+2
		move.w	d7,74(a0)	; 10010 2 set			3+2
		move.w	d3,78(a0)	; 10011 1/2 cancel		3
		move.w	d6,82(a0)	; 10100 4 set			3+1
		move.w	d5,86(a0)	; 10101 1 set (priority)	3+1+2
		move.w	d5,90(a0)	; 10110 2 set (priority)	3+1+2
		move.w	d6,94(a0)	; 10111 4 set, 1/2 cancel	3+1
		move.w	d6,98(a0)	; 11000 8 set			3+1
		move.w	d5,102(a0)	; 11001 1 set (priority)	3+1+2
		move.w	d5,106(a0)	; 11010 2 set (priority)	3+1+2
		move.w	d6,110(a0)	; 11011 1/2 cancel, 8 set	3+1
		move.w	d3,114(a0)	; 11100 4/8 cancel		3
		move.w	d7,118(a0)	; 11101 1 set			3+2
		move.w	d7,122(a0)	; 11110 2 set			3+2
		move.w	d3,126(a0)	; 11111 4/8 AND 1/2 cancel	3
		rts


********************************************************************************
; Looks up Sin/Tan for angles, baased on rotation
; Orders angles by sin value to minimise preshifting
;-------------------------------------------------------------------------------
SetAngles:
		move.w	Rotation(pc),d0
		lea	SinR,a1
		lea	Tan,a2
; angle 1
		add.w	d0,d0
		and.w	#ROT_STEPS-1,d0
		move.w	(a1,d0.w),Sin1-Vars(a5)
		move.w	(a2,d0.w),Tan1-Vars(a5)
; angle 2 (+90deg)
		move.w	d0,d1
		add.w	#ROT_STEPS,d1	; +90deg
		move.w	(a1,d1.w),Sin2-Vars(a5) ; d3 = pf1 1/sin
		move.w	(a2,d1.w),Tan2-Vars(a5)

; For layers 2/3 don't use max/min angle - trig values out of range
; 		tst.w	d0
; 		bne	.notZero
; 		addq	#2,d0
; 		addq	#2,d1
; .notZero:
; 		cmp.w	#ROT_STEPS-2,d0
; 		bne	.notMax
; 		subq	#2,d0
; 		subq	#2,d1
; .notMax:
		move.w	(a1,d0.w),d2	; d2 = pf2 1/sin
		move.w	(a1,d1.w),d3	; d3 = pf1 1/sin
; Order by size to ensure patterns for PF1 are smaller, and reduce data to be pre-shifted
		cmp.w	d2,d3
		ble	.ok
		exg	d0,d1
		exg	d2,d3
.ok:
; Store longword indexes for angles to use later
		add.w	d0,d0
		add.w	d1,d1
		move.w	d0,AngleIdxPF2-Vars(a5)
		move.w	d1,AngleIdxPF1-Vars(a5)
; Store ordered sin values
		move.w	d2,SinPF2-Vars(a5)
		move.w	d3,SinPF1-Vars(a5)

		rts


********************************************************************************
; Create pattern data
; Populates DrawPatterns data and sets LayerXPatternPFX ptrs
;-------------------------------------------------------------------------------
CreatePatterns:
		move.l	DrawPatterns(pc),PatOffset-Vars(a5)

		; x pan scroll
		moveq	#$f,d0
		move.w	Layer2X(pc),d3
		; not.w d3
		and.w	d0,d3
		move.w	Layer3X(pc),d4
		; not.w d4
		and.w	d0,d4

		; PF2
		move.w	d3,d2		; scroll
		move.w	SinPF2(pc),d1
		move.w	Layer2Width(pc),d0
		move.w	Layer2Reps(pc),d7
		bsr	WritePattern
		move.l	a0,Layer2PatternPF2

		move.w	d4,d2
		move.w	Layer3Width-Vars(a5),d0
		move.w	Layer3Reps(pc),d7
		bsr	WritePattern
		move.l	a0,Layer3PatternPF2

		move.l	PatOffset(pc),a1 ; a1 = Start of PF2 patterns

		; PF1
		move.w	d3,d2
		move.w	SinPF1(pc),d1
		move.w	Layer2Width(pc),d0
		move.w	Layer2Reps(pc),d7
		bsr	WritePattern
		move.l	a0,Layer2PatternPF1

		move.w	d4,d2
		move.w	Layer3Width(pc),d0
		move.w	Layer3Reps(pc),d7
		bsr	WritePattern
		move.l	a0,Layer3PatternPF1

		move.l	PatOffset(pc),d1
		sub.l	a1,d1
		; TODO: round up?
		lsr.w	#3,d1
		or.w	#(4<<6),d1

;-------------------------------------------------------------------------------
; Preshift * 15
		clr.w	bltcon1(a6)
		move.w	#$19f0,d0	; bltcon0 - start with shift = 1
		lea	PAT_SHIFT_OFFSET(a1),a0 ; dest
		moveq	#15-1,d7
.shift:
		WAIT_BLIT
		move.w	d0,bltcon0(a6)
		move.l	a1,bltapth(a6)	; src: Start of PF1 patterns
		move.l	a0,bltdpth(a6)	; dest: offset
		move.w	d1,bltsize(a6)
		add.w	#1<<12,d0	; increase shift
		lea	PAT_SHIFT_OFFSET(a0),a0
		dbf	d7,.shift
		rts


********************************************************************************
; Write pattern to bitplane
;-------------------------------------------------------------------------------
; a0 = Bitplane buffer
; d0 = width
; d1 = 1/sin(a) FP 8:8
; d2 = scroll
; d7 = repetitons
; returns:
; a3 = Pattern center ptr
;-------------------------------------------------------------------------------
WritePattern:
		movem.l	d0-d4/a1,-(sp)
		move.l	Dots(pc),a1
		lea	LINE_SZ/2(a1),a1
		lea	DotClearOffsets,a2 ; Stores the byte offsets where we draw dots for clearing

		move.w	#$12,d5		; bltcon1 initial value

		mulu	d1,d0		; d0 = pan width FP 16:16
		lsl.l	#8-DEPTH_FP,d0
		move.l	d0,d1		; d1 = offset FP 16:16
		lsr.l	d1

		move.w	d2,d3		; scroll to FP
		swap	d3
		clr.w	d3
		add.l	d3,d1		; add scroll to initial offset

		move.w	d2,a5
		add.w	a5,a5		; a5 = scroll * 2

;-------------------------------------------------------------------------------
; Plot the dots:
		move.w	#LINE_SZ/2,a4
		move.w	#1<<2,d6

		WAIT_BLIT
.dotsRept:
		; Plot pairs of dots for filled regions:
		rept	2
		move.l	d1,d2
		swap	d2
		move.w	d2,d3
		not.w	d3		; d3 = bit
		move.w	d2,d4		; keep original for inverse
		lsr.w	#3,d2		; d2 = byte offset
		cmp.w	a4,d2
		bge	.overflow	; Terminate early if we exceed buffer width
		eor	d6,d5		; toggle FCI - so this will be correct if we terminate early
		lea	(a1,d2.w),a3
		bset.b	d3,(a3)		; Plot right dot
		move.l	a3,(a2)+
		; Mirror with negative offset:
		not.w	d4
		add.w	a5,d4
		move.w	d4,d3
		asr.w	#3,d3
		not.w	d4

		lea	(a1,d3.w),a3
		bset.b	d4,(a3)		; Plot left dot
		move.l	a3,(a2)+

		add.l	d0,d1		; Inc offset
		endr
		dbf	d7,.dotsRept
		bra	.dotsEnd
.overflow:
		move.w	#LINE_SZ/2-2,d2
.dotsEnd:

		lea	Vars,a5

;-------------------------------------------------------------------------------
; Fill with blitter:

		; Figure out blit size and start offsets:
		; right offset = byte width / 2 (blit width)
		and.w	#$fffe,d2	; even address for blitter

		move.l	PatOffset(pc),a0 ; a0 = screen pos

		lea	(a1,d2.w),a1	; src: right offset dots buffer for desc blit
		lea	(a0,d2.w),a0	; dest: ScreenPos + blit width (bytes)
		move.l	a0,a3		; store mid point (return value)
		lea	(a0,d2.w),a0	; add the other half offset (value is in words)

		move.l	a0,PatOffset	; update screen pos for next pattern
		add.l	#2,PatOffset

		; Set up common blit params:
		WAIT_BLIT
		move.w	#$09f0,bltcon0(a6)
		clr.l	bltamod(a6)
		move.l	#-1,bltafwm(a6)
		move.l	a1,bltapth(a6)
		move.l	a0,bltdpth(a6)

		; Need to split if > max blit width
		; On OCS blitter has a max width of 1023px.
		; Need to do this is multiple iterations, which is a PITA with fill...
.blitSplit:
		move.w	d2,d1		; blit width
		cmp.w	#LINE_BW/2,d1	; clamp to max width
		ble	.noClamp
		move.w	#LINE_BW/2,d1
.noClamp:
		move.w	#(1<<6),d0	; get bltsize from width: just set 1px height
		or.w	d1,d0

		; Do the blit
		move.w	d5,bltcon1(a6)
		move.w	d0,bltsize(a6)

		sub.w	d1,d2		; Update remaining words. Are we done?
		ble	.blitDone

		; Continue fill state from previous blit when splitting.
		; Set FCI on if last px is set. This is only reliable with exclusive fill.
		move.w	#$12,d5		; bltcon1 FCI off by default
		ext.l	d1		; Need to adjust dest ptr to end of blit so we can test.
		sub.l	d1,a0
		sub.l	d1,a0
		WAIT_BLIT		; Can't test until previous blit is finished
		btst.b	#7,2(a0)	; last px set?
		beq	.noFill
		move.w	#$16,d5		; bltcon1 FCI on
.noFill:
		bra	.blitSplit
.blitDone:

;-------------------------------------------------------------------------------
; Clear dots:

		; Get dot count from ClearList offset
		sub.l	#DotClearOffsets,a2
		move.l	a2,d0
		lsr.w	#3,d0
		subq	#1,d0
		lea	DotClearOffsets,a0
		WAIT_BLIT
.clr:
		rept	2		; dots should always come in pairs...
		move.l	(a0)+,a1
		clr.b	(a1)
		endr
		dbf	d0,.clr

		move.l	a3,a0		; return
		lea	-(DIW_BW/2+2)(a0),a0

		movem.l	(sp)+,d0-d4/a1
		rts


********************************************************************************
SetBitplanes:
; Initial offsets
		lea	OffsetPtrs,a0


		move.w	#VPAN,d1
		sub.w	Layer23Y(pc),d1
		lsl.w	#2,d1

		move.w	AngleIdxPF2(pc),d0
		move.l	(a0,d0.w),a2
		move.w	(a2,d1.w),a2	; a2 = pf2 offset

		move.w	AngleIdxPF1(pc),d0
		move.l	(a0,d0.w),a1
		lea	(a1,d1.w),a1
		move.w	(a1)+,a3
		move.w	(a1)+,d0
		ror.w	#7,d0		; scroll to preshift offset
		add.w	d0,a3		; a3 = pf1 offset

		move.w	Layer2X(pc),d1
		asr.w	#4,d1
		add.w	d1,d1
		move.w	Layer3X(pc),d2
		asr.w	#4,d2
		add.w	d2,d2

; Set bitplane ptrs in copper:
		move.l	DrawCopList,a1
; PF2:
		move.l	Layer2PatternPF2(pc),a0
		lea	(a0,a2.w),a0
		sub.w	d1,a0		; Layer2X bytes
		move.l	a0,d0
		move.w	d0,14(a1)
		swap	d0
		move.w	d0,10(a1)

		move.l	Layer3PatternPF2(pc),a0
		lea	(a0,a2.w),a0
		sub.w	d2,a0		; Layer3X bytes
		move.l	a0,d0
		move.w	d0,30(a1)
		swap	d0
		move.w	d0,26(a1)
; PF1:
		move.l	Layer2PatternPF1(pc),a0
		lea	(a0,a3.w),a0
		sub.w	d1,a0		; Layer2X bytes
		move.l	a0,d0
		move.w	d0,6(a1)
		swap	d0
		move.w	d0,2(a1)
		move.l	a0,Bpl0pt-Vars(a5)

		move.l	Layer3PatternPF1(pc),a2
		lea	(a2,a3.w),a0
		sub.w	d2,a0		; Layer3X bytes
		move.l	a0,d0
		move.w	d0,22(a1)
		swap	d0
		move.w	d0,18(a1)
		move.l	a0,Bpl2pt-Vars(a5)
; BPL 5
		move.l	DrawScreen,d0
		subq	#2,d0
		move.w	d0,38(a1)
		swap	d0
		move.w	d0,34(a1)

		rts


********************************************************************************
; Draw grid lines for top layer
;-------------------------------------------------------------------------------
DrawGrid:
		move.w	#DIW_H/2,d2
		move.w	#DIW_W/2,d3
		move.w	Layer1Y(pc),d4
		move.w	Layer1X(pc),d5
		move.w	Tan1(pc),d6
		move.w	Tan2(pc),d7

		move.w	d3,d1
		FPMULS8	d6,d1
		move.w	d1,GridT1X
		move.w	d2,d1
		FPMULS8	d6,d1
		move.w	d1,GridT1Y-Vars(a5)

		move.w	d5,d1
		FPMULS8	d6,d1
		move.w	d1,GridT1Xo-Vars(a5)
		move.w	d4,d1
		FPMULS8	d6,d1
		move.w	d1,GridT1Yo-Vars(a5)

		move.w	d3,d1
		FPMULS8	d7,d1
		move.w	d1,GridT2X-Vars(a5)
		move.w	d2,d1
		FPMULS8	d7,d1
		move.w	d1,GridT2Y-Vars(a5)

		move.w	d5,d1
		FPMULS8	d7,d1
		move.w	d1,GridT2Xo-Vars(a5)
		move.w	d4,d1
		FPMULS8	d7,d1
		move.w	d1,GridT2Yo-Vars(a5)

		move.w	Layer1Width(pc),d6
		lsr.w	#DEPTH_FP,d6
		move.w	d6,d7
		FPMULS8	Sin1(pc),d6	; d6 = D1 = r*s1
		FPMULS8	Sin2(pc),d7	; dy = D2 r*s2

		bsr	InitDrawLine
		move.l	DrawScreen(pc),a0
		lea	GridClipYs,a1

		move.w	d7,d0		; d0 = dx = D2/2
		lsr.w	d0
		move.w	d6,d1		; d0 = dy = D1/2
		lsr.w	d1

		move.w	d7,a2		; need to free a data reg for counter

		move.w	Layer1Reps(pc),d7
.l:
		; move.w	GridT1Y(pc),d2
		; move.w	GridT2X(pc),d3
		; move.w	GridT2Xo(pc),d4
		; move.w	GridT1Yo(pc),d5
		movem.w	GridT1Y(pc),d2-d5 ; opt ^

		bsr	GridLine	; draw(dx,dy,t1y,t2x,t2xo,t1yo)
		neg.w	d0
		neg.w	d1
		bsr	GridLine	; draw(-dx,-dy,t1y,t2x,t2xo,t1yo)
		neg.w	d1
		exg	d0,d1

		; move.w	GridT2Y(pc),d2
		; move.w	GridT1X(pc),d3
		; move.w	GridT1Xo(pc),d4
		; move.w	GridT2Yo(pc),d5
		movem.w	GridT2Y(pc),d2-d5 ; opt ^

		bsr	GridLine	; draw(dy,-dx,t2y,t1x,t1xo,t2yo)
		neg.w	d0
		neg.w	d1
		bsr	GridLine	; draw(-dy,dx,t2y,t1x,t1xo,t2yo)
		neg.w	d0
		exg	d0,d1

		add.w	a2,d0		; dx=dx+d2
		add.w	d6,d1		; dy=dy+d1
		dbf	d7,.l

		move.l	a1,d0		; Clip count
		sub.l	#GridClipYs,d0
		beq	.noClip
		lsr.w	d0
		subq	#1,d0
		beq	.sortDone

; Sort clipping y coords - Bubble Sort Algorithm
.l0:
		lea	GridClipYs,a0
		move.w	d0,d7		; count-1
		subq	#1,d7
		moveq	#0,d1		; swapped flag
.l1:
		move.w	(a0)+,d2	; current
		move.w	(a0),d3		; next
		cmp.w	d2,d3		; swap needed?
		bge	.noSwap
		move.w	d3,-2(a0)	; swap elements
		move.w	d2,(a0)
		moveq	#1,d1		; set swap flag to 1
.noSwap:
		dbf	d7,.l1
		tst.w	d1		; If no swaps were made, the array is sorted
		beq	.sortDone
		bra	.l0
.sortDone:

; Vertical lines:
		move.l	DrawScreen,a0
		lea	GridClipYs,a1
		move.w	d0,d7		; count-1
		moveq	#-1,d1
.l2:
		move.w	(a1)+,a2
		tst.w	d1
		blt	.noDraw
		move.w	#MAX_X,d0
		move.w	d0,d2
		move.w	a2,d3
		bsr	DrawLine
		moveq	#-1,d1
		bra	.next
.noDraw:
		move.w	a2,d1
.next:
		dbf	d7,.l2

		; last line?
		tst.w	d1
		blt	.noDrawLast
		move.w	#MAX_X,d0
		move.w	d0,d2
		move.w	#DIW_H,d3
		bsr	DrawLine
.noDrawLast:
.noClip:
		rts


********************************************************************************
; Draws grid a line, finding intersections and storing clipping points
;-------------------------------------------------------------------------------
; d0 = dx
; d1 = dy
; d2 = ty (swapped)
; d3 = tx ''
; d4 = txo
; d5 = tyo
; a0 = screen buffer
; a1 = clips list
;-------------------------------------------------------------------------------
GridLine:
		movem.w	d0-d6,-(sp)
		add.w	#DIW_W/2,d5
		add.w	Layer1X(pc),d5
		add.w	d0,d5		; d5 = x = dx+X+xo+tyo

		neg.w	d4
		add.w	#DIW_H/2,d4
		add.w	Layer1Y(pc),d4
		add.w	d1,d4		; d4 = y = dy+y=Y+yo-txo

		move.w	d5,d0
		add.w	d2,d0		; d0 = x1 = x+ty
		neg.w	d2
		add.w	d5,d2		; d2 =x2 = x-ty

		move.w	d3,d6		; d6 = tx

		clr.w	d1		; d1 = y1 = 0
		move.w	#DIW_H,d3	; d3 = y2 = MAX_Y

		cmp.w	d0,d2
		beq	.skip

; clip right
		cmp.w	#MAX_X,d0	; if x1>MAX_X
		ble	.x1MaxOK
		cmp.w	#MAX_X,d2	; oob?
		bgt	.skip
		move.w	#MAX_X,d0
		move.w	d4,d1		; y1=y+tx
		add.w	d6,d1
		blt	.skip
		cmp.w	#DIW_H,d1
		bgt	.skip
		move.w	d1,(a1)+	; store
		bra	.checkLeft
.x1MaxOK:
		cmp.w	#MAX_X,d2	; if x2>MAX_X
		ble	.x2MaxOK
		move.w	#MAX_X,d2
		move.w	d4,d3		; y1=y+tx
		add.w	d6,d3
		blt	.skip
		cmp.w	#DIW_H,d3
		bgt	.skip
		move.w	d3,(a1)+	; store
.x2MaxOK:

.checkLeft:
; clip left
		tst.w	d0		; if x1<0
		bge	.x1MinOK
		tst.w	d2		; oob?
		blt	.skip
		clr.w	d0
		move.w	d4,d1		; y1=y-tx
		sub.w	d6,d1
		blt	.skip
		cmp.w	#DIW_H,d1
		bgt	.skip
.x1MinOK:

		tst.w	d2		; if x2<0
		bge	.x2MinOK
		clr.w	d2
		move.w	d4,d3		; y1=y-tx
		sub.w	d6,d3
		blt	.skip
		cmp.w	#DIW_H,d3
		bgt	.skip
.x2MinOK:

		bsr	DrawLine

.skip:		movem.w	(sp)+,d0-d6
		rts


********************************************************************************
; Populates row values in copperlist
;-------------------------------------------------------------------------------
WriteCopperRows:
		move.l	DrawCopList(pc),a0
		lea	Cop_Rows(a0),a0
		; deltas for PF1 / angle B
		move.w	AngleIdxPF1(pc),d0
		lea	Pf1DeltaPtrs,a1
		move.l	(a1,d0.w),a1
		; modulos for PF2 / angle A
		move.w	AngleIdxPF2(pc),d0
		lea	Pf2ModPtrs,a2
		move.l	(a2,d0.w),a2

		; Apply vertical pan
		move.w	#VPAN,d0
		sub.w	Layer23Y(pc),d0
		add.w	d0,d0
		lea	(a1,d0.w),a1
		add.w	d0,d0
		lea	(a2,d0.w),a2

		; Popluate values in copper list
		move.w	Bpl0pt+2(pc),d0
		move.w	Bpl2pt+2(pc),d1

		move.w	#(DIW_H-1)/4-1,d7
.l:
		movem.w	(a1)+,d2-d5
		add.w	d2,d0
		add.w	d2,d1
		move.w	d0,0*CopRow_SizeOf+CopRow_Bpl0ptl(a0)
		move.w	d1,0*CopRow_SizeOf+CopRow_Bpl2ptl(a0)
		move.w	(a2)+,0*CopRow_SizeOf+CopRow_Scroll(a0)
		move.w	(a2)+,0*CopRow_SizeOf+CopRow_Mod(a0)

		add.w	d3,d0
		add.w	d3,d1
		move.w	d0,1*CopRow_SizeOf+CopRow_Bpl0ptl(a0)
		move.w	d1,1*CopRow_SizeOf+CopRow_Bpl2ptl(a0)
		move.w	(a2)+,1*CopRow_SizeOf+CopRow_Scroll(a0)
		move.w	(a2)+,1*CopRow_SizeOf+CopRow_Mod(a0)

		add.w	d4,d0
		add.w	d4,d1
		move.w	d0,2*CopRow_SizeOf+CopRow_Bpl0ptl(a0)
		move.w	d1,2*CopRow_SizeOf+CopRow_Bpl2ptl(a0)
		move.w	(a2)+,2*CopRow_SizeOf+CopRow_Scroll(a0)
		move.w	(a2)+,2*CopRow_SizeOf+CopRow_Mod(a0)

		add.w	d5,d0
		add.w	d5,d1
		move.w	d0,3*CopRow_SizeOf+CopRow_Bpl0ptl(a0)
		move.w	d1,3*CopRow_SizeOf+CopRow_Bpl2ptl(a0)
		move.w	(a2)+,3*CopRow_SizeOf+CopRow_Scroll(a0)
		move.w	(a2)+,3*CopRow_SizeOf+CopRow_Mod(a0)

		lea	CopRow_SizeOf*4(a0),a0
		dbf	d7,.l

		rts


********************************************************************************
; Use blitter to fill top layer grid
;-------------------------------------------------------------------------------
FillGrid:
		move.l	DrawScreen(pc),a0
; Count pixels to right of center to determine initial fill state
		move.w	#DIW_H/2,d0
		add.w	Layer1Y(pc),d0
		mulu	#DIW_BW,d0
		add.w	#DIW_BW/2,d0
		move.w	Layer1X(pc),d1
		asr.w	#3,d1
		add.w	d1,d0

		lea	(a0,d0.w),a1
		moveq	#0,d0
		moveq	#1,d3
		WAIT_BLIT
		moveq	#DIW_BW/2-1,d7
		sub.w	d1,d7
		; move.b #-1,(a1)
.l0:
		move.b	(a1)+,d1
		beq	.noBit
		; count bits set in byte
		rept	8
		move.b	d1,d2
		and.w	d3,d2
		add.w	d2,d0
		lsr.b	d1
		endr
.noBit:
		dbf	d7,.l0

		move.l	#$09f00012,d1
		and.w	d3,d0
		beq	.even
		move.b	#$16,d1
.even:
		lea	DIW_BW*DIW_H-2(a0),a0
		move.l	d1,bltcon0(a6)
		clr.l	bltamod(a6)
		move.l	a0,bltapt(a6)
		move.l	a0,bltdpt(a6)
		move.w	#(DIW_H<<6)!(DIW_BW/2),bltsize(a6)
		rts


********************************************************************************
; Clear grid using CPU
;-------------------------------------------------------------------------------
ClearGrid:
		move.l	sp,SpBak
		lea	BlankData,a7
		movem.l	(a7)+,d0-a6
		move.l	ClearScreen(pc),a7
		lea	DIW_BW*DIW_H(a7),a7
		rept	(DIW_BW*DIW_H)/(15*4)
		movem.l	d0-a6,-(a7)
		endr
		; remainder: need to calculate number of regs
		movem.l	d0-a0,-(a7)
		move.l	SpBak(pc),sp
		lea	Vars,a5
		lea	custom,a6
		rts
SpBak:		dc.l	0


********************************************************************************
; Swap double buffered data at end of frame
;-------------------------------------------------------------------------------
FlipBuffers:
		movem.l	DblBuffers(pc),d0-d3
		exg	d0,d1		; patterns
		exg	d2,d3		; coplist
		movem.l	d0-d3,DblBuffers-Vars(a5)

		move.l	d3,cop2lc(a6)

		movem.l	ScreenBuffers(pc),d0-d2
		exg	d0,d2
		exg	d1,d2
		movem.l	d0-d2,ScreenBuffers-Vars(a5)
		rts


********************************************************************************
; Prepare common blit regs for line draw
;-------------------------------------------------------------------------------
InitDrawLine:
		WAIT_BLIT
		move.w	#DIW_BW,bltcmod(a6)
		move.l	#-$8000,bltbdat(a6)
		move.l	#-1,bltafwm(a6)
		rts


********************************************************************************
; Draw a line for filling using the blitter
; Based on TEC, but with muls LUT
;-------------------------------------------------------------------------------
; d0.w - x1
; d1.w - y1
; d2.w - x2
; d3.w - y2
; a0 - Draw buffer
; a6 - Custom
;-------------------------------------------------------------------------------
DrawLine:
		cmp.w	d1,d3
		bgt.s	.l0
		beq.s	.done
		exg	d0,d2
		exg	d1,d3
.l0:		moveq	#0,d4
		move.w	d1,d4
		add.w	d4,d4
		move.w	LineMuls(pc,d4.w),d4
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
		or.w	#$a4a,d0
		; or.w	#$bca,d0

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
.done:		rts
.oct:		dc.b	3,3+64,19,19+64,11,11+64,23,23+64

LineMuls:	ds.w	DIW_H


********************************************************************************
Vars:
********************************************************************************

RotSpeed:	dc.w	$1000
Rotation:	dc.l	0
Depth:		dc.l	0

DepthSpeed:	dc.w	0

DblBuffers:
DrawPatterns:	dc.l	0
ViewPatterns:	dc.l	0
DrawCopList:	dc.l	0
ViewCopList:	dc.l	0

ScreenBuffers:
DrawScreen:	dc.l	0
ViewScreen:	dc.l	0
ClearScreen:	dc.l	0

; Layers:

Layer1Depth:	dc.w	0
Layer2Depth:	dc.w	0
Layer3Depth:	dc.w	0

Layer1Width:	dc.w	0
Layer2Width:	dc.w	0
Layer3Width:	dc.w	0

Layer1Reps:	dc.w	0
Layer2Reps:	dc.w	0
Layer3Reps:	dc.w	0

Layer1X:	dc.w	0
Layer2X:	dc.w	0
Layer3X:	dc.w	0

Layer1Y:	dc.w	0
Layer23Y:	dc.w	0		; combined

; Keep track of buffer position when creating patterns
PatOffset:	dc.l	0

; Pattern ptrs
Layer2PatternPF1: dc.l	0
Layer2PatternPF2: dc.l	0
Layer3PatternPF1: dc.l	0
Layer3PatternPF2: dc.l	0

; Longword indexes for angles, ordered by size
AngleIdxPF2:	dc.w	0
AngleIdxPF1:	dc.w	0

SinPF2:		dc.w	0
SinPF1:		dc.w	0

; Initial bitplane ptrs for PF1
Bpl0pt:		dc.l	0
Bpl2pt:		dc.l	0

Tan1:		dc.w	0
Tan2:		dc.w	0
Sin1:		dc.w	0
Sin2:		dc.w	0

GridT1Y:	dc.w	0
GridT2X:	dc.w	0
GridT2Xo:	dc.w	0
GridT1Yo:	dc.w	0
GridT2Y:	dc.w	0
GridT1X:	dc.w	0
GridT1Xo:	dc.w	0
GridT2Yo:	dc.w	0

ColsIn:
;https://gradient-blaster.grahambates.com/?points=717@0,309@2,12d@4,0a4@9,e94@11,905@13,717@15&steps=16&blendMode=oklab&ditherMode=blueNoise&target=amigaOcs&ditherAmount=0
		; dc.w	$0,$8,$9
		dc.w	$0,$9,$7
		; dc.w	$0,$a,$4
		; dc.w	$a,$a,$4
		dc.w	$e,$9,$4
		; dc.w	$c,$5,$5
		dc.w	$9,$0,$0
		; dc.w	$8,$1,$6
		dc.w	$7,$1,$7
		dc.w	$5,$1,$8
		dc.w	$3,$0,$9
		; dc.w	$2,$1,$b
		dc.w	$1,$2,$d
		dc.w	$0,$7,$b

		dc.w	$0,$8,$9
		dc.w	$0,$9,$7
		; dc.w	$0,$a,$4
		; dc.w	$a,$a,$4
		dc.w	$e,$9,$4

; https://gradient-blaster.grahambates.com/?points=505@0,317@2,12b@4,0a4@8,d60@11,905@13,505@15&steps=16&blendMode=oklab&ditherMode=blueNoise&target=amigaOcs&ditherAmount=0
; ; shuffled
; 		dc.w	$0,$9,$7
; 		dc.w	$4,$1,$6
; 		dc.w	$5,$0,$5
; 		dc.w	$0,$7,$8
; 		dc.w	$0,$a,$4
; 		dc.w	$5,$0,$5
; 		dc.w	$8,$9,$3
; 		dc.w	$b,$4,$4
; 		dc.w	$d,$6,$0
; 		dc.w	$0,$5,$a
; 		dc.w	$9,$0,$5
; 		dc.w	$1,$2,$b
; 		dc.w	$3,$1,$7
; 		dc.w	$2,$1,$9
; 		dc.w	$b,$8,$2
; 		dc.w	$7,$0,$5
; ; repeat
; 		dc.w	$0,$9,$7
; 		dc.w	$4,$1,$6


ColsOut:
C1:		dc.w	0,0,0
C2:		dc.w	0,0,0
C3:		dc.w	0,0,0
C12:		dc.w	0,0,0
C123:		dc.w	0,0,0
C13:		dc.w	0,0,0
C23:		dc.w	0,0,0

ColsOutRGB:
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0

Dots:		dc.l	0

; Offset array data
OffsetData:	dc.l	0
; Delta array data
Pf1Deltas:	dc.l	0
; Mod/scroll array data
Pf2ModData:	dc.l	0


********************************************************************************
		data
********************************************************************************

		include	data/checkertbls.i


*******************************************************************************
		data_c
*******************************************************************************

; Main copper list
Cop:
		dc.w	diwstrt,DIW_YSTRT<<8!DIW_XSTRT
		dc.w	diwstop,(DIW_YSTOP-256)<<8!(DIW_XSTOP-256)
		dc.w	ddfstrt,(DIW_XSTRT-17-16)>>1&$fc
		dc.w	ddfstop,(DIW_XSTRT-17+(DIW_W>>4-1)<<4)>>1&$fc
		dc.w	bplcon0,BPLS<<12!$200
		dc.w	bpl1mod,-2
		dc.w	copjmp2,0	; Jump to dbl buffered list
; Placeholder cop2
CopEnd:		dc.l	-2


*******************************************************************************
		bss
*******************************************************************************

; Byte offsets of dots drawn for patterns
DotClearOffsets: ds.l	100		; TODO: max size?

; Pointer to offset array for per angle
OffsetPtrs:	ds.l	ROT_STEPS

; Pointer to delta array for per angle for PF1
Pf1DeltaPtrs:	ds.l	ROT_STEPS

; Pointer to mod/scroll array for per angle for PF2
Pf2ModPtrs:	ds.l	ROT_STEPS

; Clipping Y positions on RHS of grid
GridClipYs:	ds.w	20
