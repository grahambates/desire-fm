ENABLE_SKIP = 0
DMASET = DMAF_SETCLR!DMAF_MASTER!DMAF_RASTER!DMAF_COPPER!DMAF_BLITTER
INTSET = INTF_SETCLR!INTF_INTEN!INTF_VERTB
MAX_VPOS_FOR_BG_TASK = 308

		include	"_main.i"
_start:
		include	"PhotonsMiniWrapper1.04.i"

		include	"sequence.i"


********************************************************************************
Demo:
********************************************************************************
; Blank screen while precalcing
		jsr	WaitEOF
		clr.l	color00(a6)
		clr.l	color02(a6)
		move.l	#BlankCop,cop1lc(a6)
		move.w	#DMASET,dmacon(a6)
		move.l	#MainInterrupt,$6c(a4)

; Precalc
		jsr	Tables_Precalc

; Init sequence pos:
		lea	Sequence,a0
		ifne	TEST_PART
; Jump to part in sequence
		lea	Part_SIZEOF*TEST_PART(a0),a0
; Adjust music start pos
		move.w	-2(a0),d0
		move.w	d0,P61_InitPos
		move.w	d0,P61_Pos
		endc

; call first precalc before starting
		move.l	(a0)+,d0
		ifne	TEST_PART
		beq	.noPre
		move.l	d0,a1
		movem.l	a0/a6,-(sp)
		jsr	(a1)
		movem.l	(sp)+,a0/a6
.noPre:
		endif

		move.l	a0,SequencePos

; Start music
		lea	Module,a0
		sub.l	a1,a1
		move.l	a1,a2
		moveq	#0,d0
		bsr	P61_Init

; Start interrupt
		move.w	#INTSET,intena(a6)

;-------------------------------------------------------------------------------
; Effects sequence


.effect:
		lea	custom,a6
; Reset between effects:
; Blank copper
		jsr	WaitEOF
		move.l	#BlankCop,cop1lc(a6)
; Restore standard DMA and interrupt
		move.w	#DMAF_SPRITE!DMAF_BLITHOG,dmacon(a6)
		move.w	#INTF_COPER!INTF_BLIT,intena(a6)
		move.w	#DMASET,dmacon(a6)
		move.w	#INTSET,intena(a6)
		clr.w	copcon(a6)	; Copper danger off
		clr.l	VBlankInterrupt
		clr.l	BlitterInterrupt

		move.l	SequencePos(pc),a1
; Next effect routine?
		move.l	(a1)+,d0
		beq	.endDemo

		move.w	(a1)+,EndPos	; Set end position for PartOver
		clr.l	LocalFrame	; Reset local frame counter

; Standard registers on entry
		move.l	(a1)+,a0	; Precalc for next part

		move.l	a1,SequencePos	; update position

; Call effect routine
		move.l	d0,a1
		jsr	(a1)

		bra	.effect
.endDemo:
		rts


SkipPressed:	dc.w	0


********************************************************************************
; Check if current part is over according to sequence timings
;-------------------------------------------------------------------------------
PartOver:

		ifne	ENABLE_SKIP
; mouse pressed
		btst	#CIAB_GAMEPORT0,ciaa
		bne	.noPressed
; already detected?
		tst.w	SkipPressed
		bne	.checkFrame
; skip to next part
		lea	custom,a6
		move.w	EndPos,d0
		bsr	P61_SetPosition
		move.w	#1,SkipPressed
		tst.w	P61_Pos
		rts
.noPressed:
		clr.w	SkipPressed
.checkFrame:
		endc
		move.w	P61_Pos,d0
		cmp.w	EndPos(pc),d0
		rts


********************************************************************************
MainInterrupt:
; VBI?
		btst.b	#INTB_VERTB,intreqr+custom+1
		beq	.notVbi
;-------------------------------------------------------------------------------
; Background task?
; Check if we're currently in executing a background task and switch back to main:
		tst.l	MainUSP
		beq	.notBgTask
		; Store background task state
		move.l	a6,-(sp)	; save a6, we need a spare register
		move.l	usp,a6		; get USP
		move.l	4+2(sp),-(a6)	; store PC
		move.w	4(sp),-(a6)	; store SR
		move.l	(sp)+,-(a6)	; store a6 in stack frame
		movem.l	d0-a5,-(a6)	; store the rest of the registers
		move.l	a6,BgTaskUSP	; Store bg USP
		; Restore main task
		move.l	MainUSP(pc),a0	; primary USP from before
		move.l	(a0)+,2(sp)	; store return PC to exception frame (keep SR unchanged)
		move.l	a0,usp		; restore primary USP (now at position before calling the vblank wait)
		clr.l	MainUSP		; make sure we will not reload it until has been set again
.notBgTask:

;-------------------------------------------------------------------------------
; Regular VBI stuff:
		movem.l	d0-a6,-(sp)
; Increment frame counters:
		lea	Vars(pc),a5
		addq.l	#1,GlobalFrame-Vars(a5)
		addq.l	#1,LocalFrame-Vars(a5)

		move.w	LocalFrame+2,d0
		mulu	#$100000*BPM/3000,d0
		add.l	d0,d0
		swap	d0
		move.w	d0,BpmFrame-Vars(a5)

; Process active lerps
		jsr	LerpWordsStep
		jsr	LerpPalStep
		jsr	Commander_Process

		move.l	VBlankInterrupt(pc),d0
		beq	.noCustom
		move.l	d0,a0
		jsr	(a0)
.noCustom:

		movem.l	(sp)+,d0-a6

		move.w	#INTF_VERTB,intreq+custom
		move.w	#INTF_VERTB,intreq+custom
		rte
.notVbi:

;-------------------------------------------------------------------------------
; Blitter interrupt?
		btst.b	#INTB_BLIT,intreqr+custom+1
		beq	.notBlit
		move.w	#INTF_BLIT,intreq+custom
		; Jump to blitter interrupt routine without using any registers
		move.l	#.notBlit,-(sp)
		move.l	BlitterInterrupt(pc),-(sp)
		rts
.notBlit:
		rte


********************************************************************************
; Reset LocalFrame to zero
;-------------------------------------------------------------------------------
ResetFrameCounter:
		clr.l	LocalFrame
		rts


********************************************************************************
; Switch to background task until vbi
;-------------------------------------------------------------------------------
; Big thanks to Platon42 for this writeup:
; https://dump.platon42.de/hameager/
;-------------------------------------------------------------------------------
VSyncWithBgTask:
		; Is bg task set?
		move.l	BgTaskUSP(pc),d1
		beq	VSync

		; Is there enough time left on frame to switch?
		move.l	#$1ff00,d0
		and.l	vposr(a6),d0
		cmp.l	#MAX_VPOS_FOR_BG_TASK<<8,d0
		bgt	VSync

		movem.l	a5-a6,-(sp)
		bsr	.switch
		movem.l	(sp)+,a5-a6
		rts

.switch:
		; Switch to bg task
		move.l	sp,MainUSP	; Back up main SP
		move.l	d1,sp		; Restore BG task SP
		movem.l	(sp)+,d0-a6	; Restore BG registers
		rtr


********************************************************************************
; Sync with vbi
;-------------------------------------------------------------------------------
VSync:
; Wait for global frame to update
		move.w	GlobalFrame+2(pc),d0
.loop:		cmp.w	GlobalFrame+2(pc),d0
		beq.s	.loop
		rts


********************************************************************************
CmdSetBgTask:
		move.l	(a5)+,a0
		; fall through...


********************************************************************************
; Set background task
;-------------------------------------------------------------------------------
; a0 - Task
;-------------------------------------------------------------------------------
SetBgTask:
		lea	BgStackE(pc),a1
		exg	a1,sp		; temporarily swap stack pointers
		pea	.taskDone(pc)	; routine to call after RTS from backgroundtask
		move.l	a0,-(sp)	; background task to jump to
		clr.w	-(sp)		; initial ccr
		movem.l	d0-a6,-(sp)	; initial register dump from caller
		move.l	sp,BgTaskUSP
		exg	a1,sp
		rts

.taskDone:
		clr.l	BgTaskUSP
.l:
		bra	.l


********************************************************************************
Vars:
********************************************************************************

SequencePos:	dc.l	0
GlobalFrame:	dc.l	0
LocalFrame:	dc.l	0
BpmFrame:	dc.w	0
EndPos:		dc.w	0

MainUSP:	dc.l	0
BgTaskUSP:	dc.l	0
BgStack:	ds.b	128
BgStackE:

VBlankInterrupt: dc.l	0
BlitterInterrupt: dc.l	0

; Zeroed data for use as black palette, null sprite etc.
BlankData:	ds.w	32
; Fill bytes e.g. white palette
FillData:	dcb.w	16,$fff



		include	include/p61settings.i
		include	"include/P6112-Play.i"


*******************************************************************************
		data_c
*******************************************************************************

Module:
		incbin	"data/P61.tune"
		even

;-------------------------------------------------------------------------------
; Initial Copperlist for blank screen
BlankCop:
		dc.w	fmode,0
		dc.w	diwstrt,$2c81
		dc.w	diwstop,$2cc1
		dc.w	bplcon0,$200
		dc.l	-2
