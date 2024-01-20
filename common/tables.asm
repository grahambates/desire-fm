		include	_main.i
		include	common/tables.i

EXTRA_ACC = 1

********************************************************************************
Tables_Precalc:
		bsr	InitSin
		bra	InitScreenMuls


********************************************************************************
; Populate sin table
;-------------------------------------------------------------------------------
; https://eab.abime.net/showpost.php?p=1471651&postcount=24
; maxError = 26.86567%
; averageError = 8.483626%
;-------------------------------------------------------------------------------
InitSin:
		lea	Sin,a0
		moveq	#0,d0		; amp=16384, len=1024
		move.w	#511+2,a1
.l
		subq.l	#2,a1
		move.l	d0,d1

		ifne	EXTRA_ACC
		move.w	d1,d2
		neg.w	d2
		mulu.w	d1,d2
		divu.w	#74504/2,d2	; 74504=amp/scale
		lsr.w	#2+1,d2
		sub.w	d2,d1
		endc

		asr.l	#2,d1
		move.w	d1,(a0)+
		neg.w	d1
		move.w	d1,(1024-2,a0)
		add.l	a1,d0
		bne.b	.l

; Copy extra 90 deg for cosine
		lea	Sin,a0
		lea	Sin+1024*2,a1
		move.w	#256/2,d0
.copy
		move.l	(a0)+,(a1)+
		dbf	d0,.copy

		rts


********************************************************************************
InitScreenMuls:
		lea	ScreenMuls,a0
		moveq	#0,d0
		move.w	#256-1,d7
.l		move.w	d0,(a0)+
		add.w	#320/8,d0
		dbf	d7,.l
		rts


*******************************************************************************
		bss
*******************************************************************************
BSSData:

; FP 2/14
; +-16384
; ($c000-$4000) over 1024 ($400) steps
Sin:		ds.w	256
Cos:		ds.w	1024

ScreenMuls:	ds.w	256


		printt	"Tables: BSSData"
		printv	*-BSSData
