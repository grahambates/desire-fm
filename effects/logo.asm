		include	_main.i
		include	effects/logo.i
		; reuses some assets from here
		include	effects/city.i


********************************************************************************
* Constants:
********************************************************************************

; Display window:
DIW_W = 320
DIW_H = 256
BPLS = 2
DPF = 0

; Screen buffer:
SCREEN_W = DIW_W
SCREEN_H = DIW_H

LOGO_W = SCREEN_W
LOGO_H = 106
LOGO_BW = LOGO_W/16*2
LOGO_SIZE = LOGO_BW*LOGO_H*BPLS

MASK_H = LOGO_H-2-20
MASK_SIZE = SCREEN_BW*MASK_H

BG_W = 512
BG_BW = BG_W/8
SKYLINE_H = 23
SKYLINE_Y = 220
LANDSCAPE_H = 35
LANDSCAPE_Y = 190

COP_SIZE = 132

DMASET = DMAF_SETCLR!DMAF_MASTER!DMAF_RASTER!DMAF_COPPER!DMAF_BLITTER

;-------------------------------------------------------------------------------
; Derived

COLORS = 1<<BPLS

SCREEN_BW = SCREEN_W/16*2		; byte-width of 1 bitplane line
SCREEN_BPL = SCREEN_BW*SCREEN_H		; bitplane offset
SCREEN_SIZE = SCREEN_BW*SCREEN_H*BPLS

DIW_BW = DIW_W/16*2
DIW_MOD = SCREEN_BW-DIW_BW
DIW_SIZE = DIW_BW*DIW_H*BPLS
DIW_XSTRT = ($242-DIW_W)/2
DIW_YSTRT = ($158-DIW_H)/2
DIW_XSTOP = DIW_XSTRT+DIW_W
DIW_YSTOP = DIW_YSTRT+DIW_H
DIW_STRT = (DIW_YSTRT<<8)!DIW_XSTRT
DIW_STOP = ((DIW_YSTOP-256)<<8)!(DIW_XSTOP-256)
DDF_STRT = ((DIW_XSTRT-17)>>1)&$00fc
DDF_STOP = ((DIW_XSTRT-17+(((DIW_W>>4)-1)<<4))>>1)&$00fc

********************************************************************************
; Interrupt handler for simple blitter op chaining
;-------------------------------------------------------------------------------
BlitInt:
		movem.l	d0/a0-a1,-(sp)
		move.l	NextBlit(pc),d0
		beq	.done
		move.l	d0,a0
		jsr	(a0)
.done:		movem.l	(sp)+,d0/a0-a1
		rts

********************************************************************************
Logo_Pre:
********************************************************************************
		jsr	MemFlip

		ALLOC_PUBLIC 28*16*2
		move.l	a0,PalData

		lea	FillData,a0
		lea	Pal,a1
		move.l	PalData(pc),a2
		lea	PalPtrs,a3
		moveq	#16-1,d0
		moveq	#8-1,d1
		jsr	PreLerpPal

		lea	Pal,a0
		lea	BlankData,a1
		moveq	#16-1,d0
		moveq	#8-1,d1
		jsr	PreLerpPal
		rts


********************************************************************************
* Entry points:
********************************************************************************

Script:
		dc.l	0,CmdLerpWord,15,5,FadePos
		dc.l	24*3000/BPM,P61_End
		dc.l	32*3000/BPM-1<<6,CmdLerpWord,31,6,FadePos
		dc.l	$8000

********************************************************************************
Logo_Effect:
		jsr	MemFlip
		jsr	MemFreeLast

		lea	Script,a0
		jsr	Commander_Init

		bsr	SetPal

		ALLOC_CHIP SCREEN_SIZE,DrawScreen
		WAIT_BLIT
		move.l	#$1000000,bltcon0(a6)
		clr.w	bltdmod(a6)
		move.l	a0,bltdpt(a6)
		move.w	#SCREEN_H*BPLS*64+SCREEN_BW/2,bltsize(a6)

		ALLOC_CHIP SCREEN_SIZE,ViewScreen
		WAIT_BLIT
		move.l	#$1000000,bltcon0(a6)
		clr.w	bltdmod(a6)
		move.l	a0,bltdpt(a6)
		move.w	#SCREEN_H*BPLS*64+SCREEN_BW/2,bltsize(a6)

		ALLOC_CHIP SCREEN_BW*(MASK_H+24)

		WAIT_BLIT
		move.l	a0,bltdpt(a6)
		move.w	#(MASK_H+24)*64+SCREEN_BW/2,bltsize(a6)

		lea	SCREEN_BW*12(a0),a0
		move.l	a0,DrawMask

		ALLOC_CHIP SCREEN_BW*(MASK_H+24)

		WAIT_BLIT
		move.l	a0,bltdpt(a6)
		move.w	#(MASK_H+24)*64+SCREEN_BW/2,bltsize(a6)

		lea	SCREEN_BW*12(a0),a0
		move.l	a0,ViewMask

; Set bg bpl ptrs
		move.l	#Bg,d1
		lea	CopBplPt2+2,a1
		moveq	#BPLS-1,d7
.bpl:
		move.w	d1,4(a1)
		swap	d1
		move.w	d1,(a1)
		swap	d1
		add.l	#DIW_BW*DIW_H,d1
		lea	8(a1),a1
		dbf	d7,.bpl

; Copy landscape
		lea	Bg+SCREEN_BW*LANDSCAPE_Y,a0
		lea	SCREEN_BW*SCREEN_H(a0),a2
		lea	Landscape+10,a1
		WAIT_BLIT
		move.l	#$d300000,bltcon0(a6)
		move.l	#-1,bltafwm(a6)
		clr.w	bltdmod(a6)
		clr.w	bltbmod(a6)
		move.l	a2,bltdpt(a6)
		move.l	a0,bltbpt(a6)
		move.l	a1,bltapt(a6)
		move.w	#BG_BW-SCREEN_BW,bltamod(a6)
		move.w	#LANDSCAPE_H*64+SCREEN_BW/2,bltsize(a6)

; Fill below landscape
		WAIT_BLIT
		lea	LANDSCAPE_H*SCREEN_BW(a0),a0
		lea	LANDSCAPE_H*SCREEN_BW(a2),a2
		move.w	#$533,bltcon0(a6)
		move.l	a0,bltbpt(a6)
		move.l	a2,bltdpt(a6)
		move.w	#(SCREEN_H-LANDSCAPE_Y-LANDSCAPE_H)*64+SCREEN_BW/2,bltsize(a6)

; Copy landscape
		lea	Bg+SCREEN_BW*SKYLINE_Y,a0
		lea	Skyline+4,a1
		WAIT_BLIT
		move.w	#$dfc,bltcon0(a6)
		move.l	a0,bltdpt(a6)
		move.l	a0,bltbpt(a6)
		move.l	a1,bltapt(a6)
		move.w	#BG_BW-SCREEN_BW,bltamod(a6)
		move.w	#SKYLINE_H*64+SCREEN_BW/2,bltsize(a6)

; Fill below skyline
		WAIT_BLIT
		lea	SKYLINE_H*SCREEN_BW(a0),a0
		move.w	#$1ff,bltcon0(a6)
		move.l	a0,bltdpt(a6)
		move.w	#(SCREEN_H-SKYLINE_Y-SKYLINE_H)*64+SCREEN_BW/2,bltsize(a6)

; Copy logo to bg
		lea	Bg+DIW_BW*(100-2),a0
		lea	Logo+LOGO_BW*LOGO_H,a1

		; subtracting opposite bitplanes to avoid creating a mask...
		WAIT_BLIT
		move.w	#$d0c,bltcon0(a6)
		move.w	#LOGO_BW-DIW_BW,bltamod(a6)
		move.l	a0,bltdpt(a6)
		move.l	a0,bltbpt(a6)
		move.l	a1,bltapt(a6)
		move.w	#LOGO_H*64+DIW_BW/2,bltsize(a6)

		lea	-LOGO_BW*LOGO_H(a1),a1
		WAIT_BLIT
		move.w	#$dfc,bltcon0(a6)
		move.l	a0,bltdpt(a6)
		move.l	a0,bltbpt(a6)
		move.l	a1,bltapt(a6)
		move.w	#LOGO_H*64+DIW_BW/2,bltsize(a6)

		lea	DIW_BW*DIW_H(a0),a0
		WAIT_BLIT
		move.w	#$d0c,bltcon0(a6)
		move.l	a0,bltdpt(a6)
		move.l	a0,bltbpt(a6)
		move.l	a1,bltapt(a6)
		move.w	#LOGO_H*64+DIW_BW/2,bltsize(a6)

		lea	LOGO_BW*LOGO_H(a1),a1
		WAIT_BLIT
		move.w	#$dfc,bltcon0(a6)
		move.l	a0,bltdpt(a6)
		move.l	a0,bltbpt(a6)
		move.l	a1,bltapt(a6)
		move.w	#LOGO_H*64+DIW_BW/2,bltsize(a6)

; Set common blitter regs
		WAIT_BLIT
		move.l	#-1,bltafwm(a6)
		clr.l	bltamod(a6)
		clr.l	bltcmod(a6)

; Enable blitter interrupt
		move.w	#INTF_SETCLR!INTF_BLIT,intena(a6)
		move.l	#BlitInt,BlitterInterrupt

		bsr	SwapBuffers

		move.l	#Cop,cop1lc(a6)

;-------------------------------------------------------------------------------
MainLoop:
		bsr	StartRingsBlit

		bsr	SetPal

		move.l	DrawMask(pc),a0
		lea	SCREEN_BW/2(a0),a0 ; center x
		lea	Radii,a1

MIN_RADIUS = 8

GROUP1_SPACE = 64
GROUP1_WIDTH = 4

GROUP2_SPACE = 128
GROUP2_WIDTH = 8
GROUP2_OFFSET = 37

		moveq	#-1,d6		; circle count (-1 for dbf)

;-------------------------------------------------------------------------------
; Prepare Group 1 data:
; Don't draw yet - just store radii so we can remove / adjust where they overlap with group 2
		move.w	LocalFrame+2,d0
		; lsr	d0		; /= 2
		and.w	#GROUP1_SPACE-1,d0 ; frame%space
		add.w	#MIN_RADIUS,d0
.l:
		move.w	d0,(a1)+	; store radius
		add.w	#GROUP1_SPACE,d0 ; radius += space
		addq.w	#1,d6		; inc circle count
		cmp.w	#SCREEN_W/2-GROUP1_WIDTH,d0 ; next > max?
		blt	.l

;-------------------------------------------------------------------------------
; Group 2:
		move.w	LocalFrame+2,d0
		add.w	d0,d0		; frame*2
		add.w	#GROUP2_OFFSET,d0 ; offset start
		and.w	#GROUP2_SPACE-1,d0 ; (frame*2+64)%128
		add.w	#MIN_RADIUS,d0	; min radius
.l0:

; Check for overlap with any rings in group 1
;
; Case 1: G2 entirely left of G1
; g2e < g1s
; no change
;                   G1s G1e
;       G2s     G2e

; Case 2: G2 entirely left of G1
; g2s > g1e
; no change
;       G1s G1e
;                G2s     G2e

; Case 3: G1 completely covered by G2
; g2s <= g1s && g2e >= g1e
; remove g1
;           G1s G1e
;       G2s         G2e

; Case 4: G1 partly covers RHS of G2
; g2s <= g1s
; remove g1
; adjust g2e to g1e
;             G1s   G1e
;       G2s      G2e

; Case 5: G1 partly covers LHS of G2
; remove g1
; adjust g2s to g1s
;       G1s   G1e
;            G2s      G2e

g2s		equr	d0
g2e		equr	d1
g1s		equr	d2
g1e		equr	d3

		move.w	g2s,g2e
		add.w	#GROUP2_WIDTH,g2e

		lea	Radii,a2
		move.w	d6,d7
.g1Loop:
		move.w	(a2)+,g1s	; d2 = g1s
		; case 1
		cmp.w	g1s,g2e		; g2e < g1s ?
		blt	.nextG1		; yep - no hit

		move.w	g1s,g1e
		add.w	#GROUP1_WIDTH,g1e
		; case 2
		cmp.w	g1e,g2s		; g2s > g1e ?
		ble	.g1Hit		; nope - hit
.nextG1:	dbf	d7,.g1Loop
		bra	.done

; Rings must be touching:
; can't be overlapping more than one so done iterating
.g1Hit:
		; from here on we always remove g1
		clr.w	-2(a2)		; set zero to skip

		; case 3
		cmp.w	g1s,g2s		; g2s <= g1s ?
		bgt	.case5		; nope - not case 3 or 4
		cmp.w	g1e,g2e		; g2e >= g1e ?
		bge	.done		; yep - nothing else to do for case 3, just remove
		; case 4
		move.w	g1e,g2e
		bra	.done
.case5:
		move.w	g1s,g2s
.done:

		; draw circles in this group immediately
		move.w	g2e,-(sp)	; stash end value
		bsr	DrawCircleFill
		move.w	(sp)+,d0
		bsr	DrawCircleFill
		add.w	#GROUP2_SPACE,d0 ; radius += space
		cmp.w	#SCREEN_W/2-GROUP2_WIDTH,d0 ; next > max
		blt	.l0

;-------------------------------------------------------------------------------
; Draw group 1:
		lea	Radii,a2
.l1:		move.w	(a2)+,d0
		beq	.skip		; skip removed radii
		bsr	DrawCircleFill
		add.w	#GROUP1_WIDTH,d0
		bsr	DrawCircleFill
.skip:		dbf	d6,.l1

.tstBlitQueue:	tst.l	NextBlit
		bne	.tstBlitQueue

		; Be careful not to do this until blit queue is done
		; WIll clear wrong mask on fast machines
		lea	custom,a6
		bsr	SwapBuffers

		jsr	VSync

		cmp.l	#32*3000/BPM,LocalFrame
		blt	MainLoop
		rts

Radii:		ds.w	16

********************************************************************************
* Routines:
********************************************************************************

SetPal:
		lea	PalPtrs,a0
		move.w	FadePos(pc),d0
		lsl.w	#2,d0
		move.l	(a0,d0.w),a0

		move.w	(a0)+,color00(a6)
		move.w	(a0)+,color01(a6)
		move.w	(a0)+,color02(a6)
		move.w	(a0)+,color03(a6)

		move.w	(a0)+,color09(a6)
		move.w	(a0)+,color10(a6)
		move.w	(a0)+,ColBgLogo+2
		move.w	(a0)+,ColBuildings+2
		rts


********************************************************************************
SwapBuffers:
		movem.l	DblBuffers(pc),d0-d3
		exg	d0,d1		; screen
		exg	d2,d3		; mask
		movem.l	d0-d3,DblBuffers

		lea	CopBplPt+2,a1
		moveq	#BPLS-1,d7
.l:
		move.w	d1,4(a1)
		swap	d1
		move.w	d1,(a1)
		swap	d1
		add.l	#SCREEN_BPL,d1
		lea	8(a1),a1
		dbf	d7,.l

		rts


********************************************************************************
; Bresenham circle for blitter fill
;-------------------------------------------------------------------------------
; a0 - Dest ptr
; d0 - Radius
;-------------------------------------------------------------------------------
DrawCircleFill:
		movem.w	d0-d6,-(sp)
		move.w	d0,d1		; d1 = x = r
		move.w	d1,d7		; d7 = x * SCREEN_BW
		muls	#SCREEN_BW,d7
		moveq	#0,d2		; d2 = y = 0
		moveq	#0,d6		; d6 = y * SCREEN_BW
		neg.w	d0		; d0 = P = 1 - r
		addq	#1,d0

		move.w	#SCREEN_BW,a1

;--------------------------------------------------------------------------------
; d3 = x (trashed)
; d4 = y * SCREEN_BW
;--------------------------------------------------------------------------------
.plot2		macro
		move.w	d3,d5
		not.w	d3
		asr.w	#3,d5
		move.w	d5,a3
		add.w	d4,d5
		bset	d3,(a0,d5.w)
		move.w	a3,d5
		not.w	d5
		add.w	d4,d5
		not.w	d3
		bset	d3,(a0,d5.w)
		endm

; Plot first point:
		move.w	d1,d3		; X,Y, -X,Y
		moveq	#0,d4
		.plot2
.l:
		cmp.w	d2,d1		; x > y?
		ble	.done

		tst.w	d0		; P < 0?
		blt	.inside
		subq	#1,d1		; x--;
		sub.w	a1,d7
		sub.w	d1,d0		; P -= x
		sub.w	d1,d0		; P -= x

.inside:
		addq	#1,d2		; y++
		add.w	a1,d6

		add.w	d2,d0		; P += y
		add.w	d2,d0		; P += y
		addq	#1,d0		; P += 1

		cmp.w	#(MASK_H),d2
		beq	.done

; Plot:
		cmp.w	#(MASK_H),d1
		bge	.noMirror
; Only mirror y if x will  change on next loop
; Avoid multiple pixels on same row as the breaks blitter fill
		tst.w	d0		; if (P >= 0)
		blt	.noMirror
		; cmp.w	d2,d1		; if (x != y):
		; beq	.noMirror
		move.w	d2,d3		; Y,X, -Y,X
		move.w	d7,d4
		.plot2
.noMirror:
		move.w	d1,d3		; X,Y, -X,Y
		move.w	d6,d4
		.plot2

		bra	.l

.done:
		movem.w	(sp)+,d0-d6
		rts


********************************************************************************
StartRingsBlit:
		WAIT_BLIT
FillMask:
		move.l	#CopyBpl1,NextBlit
		move.l	ViewMask(pc),a0
		lea	MASK_SIZE-2(a0),a0
		move.l	#$09f00012,bltcon0(a6)
		move.l	a0,bltdpth(a6)
		move.l	a0,bltapth(a6)
		move.w	#(MASK_H<<6)!(SCREEN_BW/2),bltsize(a6)
		rts


********************************************************************************
CopyBpl1:
		move.l	#CopyBpl2,NextBlit
		move.l	ViewMask(pc),a0
		lea	MASK_SIZE-2(a0),a0
		move.l	DrawScreen(pc),a1
		lea	SCREEN_SIZE-2-SCREEN_BW*60(a1),a1
		move.l	#$0dc00002,bltcon0(a6)
		movem.l	a0-a1,bltapth(a6)
		move.l	#Logo+LOGO_SIZE+(DIW_MOD/2)-2-SCREEN_BW*10,bltbpth(a6)
		move.w	#(LOGO_H<<6)!(SCREEN_BW/2),bltsize(a6)
		rts

********************************************************************************
CopyBpl2:
		move.l	#ClearMask,NextBlit
		move.l	ViewMask(pc),a0
		lea	MASK_SIZE-2(a0),a0
		move.l	DrawScreen(pc),a1
		lea	SCREEN_SIZE-2-SCREEN_BW*60-SCREEN_BPL(a1),a1
		movem.l	a0-a1,bltapth(a6)
		move.l	#Logo+LOGO_SIZE+(DIW_MOD/2)-2-(SCREEN_BW*LOGO_H)-SCREEN_BW*10,bltbpth(a6)
		move.w	#((LOGO_H-2)<<6)!(SCREEN_BW/2),bltsize(a6)
		rts

********************************************************************************
ClearMask:
		clr.l	NextBlit
		move.l	#$01000000,bltcon0(a6)
		move.l	ViewMask(pc),bltdpth(a6)
		move.w	#(MASK_H<<6)!(SCREEN_BW/2),bltsize(a6)
		rts



********************************************************************************
* Variables
********************************************************************************

Logo_Buffers:
DblBuffers:
ViewScreen:	dc.l	0
DrawScreen:	dc.l	0
ViewMask:	dc.l	0
DrawMask:	dc.l	0

NextBlit:	dc.l	0
PalData:	dc.l	0
FadePos:	dc.l	0


********************************************************************************
* Data
********************************************************************************

PalPtrs:	ds.l	32

Pal:
		; fg
		dc.w	$edf		; 0
		dc.w	$000		; 1
		dc.w	$87a		; 2
		dc.w	$eef		; 3
		; bg
		dc.w	$000		; 9
		dc.w	$87a		; 10
		dc.w	$fff		; 11 (bg logo)
		dc.w	$546		; 11 (buildings)

*******************************************************************************
		data_c
*******************************************************************************

Logo:
		; incbin	"data/broadcast-logo.BPL"
		incbin	"data/desire3.BPL"
Bg:
		incbin	"data/dipole.BPL"

;--------------------------------------------------------------------------------
; Main copper list:
Cop:
		dc.w	diwstrt,DIW_STRT
		dc.w	diwstop,DIW_STOP
		dc.w	ddfstrt,DDF_STRT
		dc.w	ddfstop,DDF_STOP
CopBplCon:
		dc.w	bplcon0,(BPLS*2<<12)!(1<<10)!$200
		dc.w	bplcon1,0
		dc.w	bplcon2,0
CopBplMod:
		dc.w	bpl1mod,DIW_MOD
		dc.w	bpl2mod,0
CopBplPt:
		dc.w	bpl0pt,0
		dc.w	bpl0ptl,0
		dc.w	bpl2pt,0
		dc.w	bpl2ptl,0
CopBplPt2:
		dc.w	bpl1pt,0
		dc.w	bpl1ptl,0
		dc.w	bpl3pt,0
		dc.w	bpl3ptl,0
ColBgLogo:	dc.w	color11,$dff

		dc.w	$ff07,$fffe
ColBuildings:	dc.w	color11,$fff	dc.w $5007,$fffe

		dc.l	-2

