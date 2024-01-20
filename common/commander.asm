
		include	_main.i
		include	common/commander.i

********************************************************************************
* Simple command scripter
* Processes a list of subroutines to call on a given frame
*:
* Each line of the script follows the format:
* dc.l  {frameNumber},{subroutine}[,...{args}]
*:
* frameNumber: Frame to wait for before proceeding
* subroutine:  Subroutine to call or zero to end script
* args:        Additional data that the routine will read in as arguments.
*              The routine is responsible for reading args, incrementing from
*              (a5)+, leaving it intact and pointing to the next line.

********************************************************************************

********************************************************************************
; Load command script
;-------------------------------------------------------------------------------
; a0 - Command script ptr
;-------------------------------------------------------------------------------
Commander_Init:
		move.l	a0,cmdP
		rts


********************************************************************************
; Read script and process commands for this frame
;-------------------------------------------------------------------------------
; returns:
; d0.l - 1 = Finished script
;-------------------------------------------------------------------------------
Commander_Process:
		move.l	cmdP(pc),d1
		beq	.finished
		move.l	d1,a5
.next:		move.l	LocalFrame,d0
		cmp.l	(a5)+,d0	; Read frameNumber and comapre to current frame
		blt	.waiting	; Return 0 if waiting for frame
		move.l	(a5)+,d1	; Read routine to call
		beq	.finished	; Zero means the script is finished - return 1
		move.l	d1,a1		; Call the routine
		jsr	(a1)
		move.l	a5,cmdP		; Update position
		bra	.next
.waiting:	moveq	#0,d0
		rts
.finished:	moveq	#1,d0
		rts

; Current script position ptr
cmdP:		dc.l	0


********************************************************************************
; Standard Commands:
;-------------------------------------------------------------------------------
; Equivalent to standard ASM mnemonics where args map to operators
;
; e.g.
; dc.l 100,CmdAddIL,10,XSpeed
;
; On frame 100, add 10 to longword address XSpeed
; equivalent to
; addi.l #10,XSpeed
********************************************************************************

CmdMoveIL:
		move.l	(a5)+,d0
		move.l	(a5)+,a1
		move.l	d0,(a1)
		rts
CmdMoveIW:
		move.l	(a5)+,d0
		move.l	(a5)+,a1
		move.w	d0,(a1)
		rts
; CmdMoveIB:
; 		move.l	(a5)+,d0
; 		move.l	(a5)+,a1
; 		move.b	d0,(a1)
; 		rts
; CmdAddIL:
; 		move.l	(a5)+,d0
; 		move.l	(a5)+,a1
; 		add.l	d0,(a1)
; 		rts
; CmdAddIW:
; 		move.l	(a5)+,d0
; 		move.l	(a5)+,a1
; 		add.w	d0,(a1)
; 		rts
; CmdAddIB:
; 		move.l	(a5)+,d0
; 		move.l	(a5)+,a1
; 		add.b	d0,(a1)
; 		rts
; CmdSubIL:
; 		move.l	(a5)+,d0
; 		move.l	(a5)+,a1
; 		sub.l	d0,(a1)
; 		rts
; CmdSubIW:
; 		move.l	(a5)+,d0
; 		move.l	(a5)+,a1
; 		sub.w	d0,(a1)
; 		rts
; CmdSubIB:
; 		move.l	(a5)+,d0
; 		move.l	(a5)+,a1
; 		sub.b	d0,(a1)
; 		rts

CmdLerpWord:
		movem.l	(a5)+,d0-d1/a1
		jmp	LerpWord

CmdLerpWordU:
		movem.l	(a5)+,d0-d1/a1
		jmp	LerpWordU

CmdLerpPal:
		movem.l	(a5)+,d0/d1/a0/a1/a2
		jmp	StartPalLerp
