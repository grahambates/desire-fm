		include	"include/exec/exec_lib.i"

WAIT_BLIT	macro
; tst.w	(a6)					;for compatibility with A1000
.\@:		btst	#DMAB_BLTDONE,dmaconr(a6)
		bne.s	.\@
		endm

BLIT_HOG	macro
		move.w	#DMAF_SETCLR!DMAF_BLITHOG,dmacon(a6)
		endm

BLIT_UNHOG	macro
		move.w	#DMAF_BLITHOG,dmacon(a6)
		endm

********************************************************************************
; Fixed point to integer (15)
; \1 - Fixed point value (mutated)
;-------------------------------------------------------------------------------
FP2I15		macro
		add.l	\1,\1
		swap	\1
		endm

********************************************************************************
; Fixed point to integer (14)
; \1 - Fixed point value (mutated)
;-------------------------------------------------------------------------------
FP2I14		macro
		lsl.l	#2,\1
		swap	\1
		endm

********************************************************************************
; Fixed point to integer (8)
; \1 - Fixed point value (mutated)
;-------------------------------------------------------------------------------
FP2I8		macro
		lsr.l	#8,\2
		endm


********************************************************************************
FPMULS15	macro
;-------------------------------------------------------------------------------
		muls.w	\1,\2
		FP2I15	\2
		endm


********************************************************************************
FPMULU		macro
;-------------------------------------------------------------------------------
		mulu.w	\1,\2
		FP2I15	\2
		endm

********************************************************************************
FPMULS14	macro
;-------------------------------------------------------------------------------
		muls.w	\1,\2
		FP2I14	\2
		endm

********************************************************************************
FPMULU14	macro
;-------------------------------------------------------------------------------
		mulu.w	\1,\2
		FP2I14	\2
		endm

********************************************************************************
FPMULS8		macro
;-------------------------------------------------------------------------------
		muls	\1,\2
		lsr.l	#8,\2
		endm

********************************************************************************
FPMULU8		macro
;-------------------------------------------------------------------------------
		mulu	\1,\2
		lsr.l	#8,\2
		endm

********************************************************************************
; Copper
********************************************************************************

COP_MOVE:	macro
		dc.w	(\2)&$1fe,\1
		endm

COP_WAIT:	macro
		dc.w	(((\1)&$ff)<<8)+((\2)&$fe)+1,$fffe
		endm

COP_WAITV:	macro
		COP_WAIT \1&$ff,4
		endm

COP_WAITH:	macro
		dc.w	((\1&$80)<<8)+(\2&$fe)+1,$80fe
		endm

COP_WAITBLIT:	macro
		dc.l	$10000
		endm

COP_SKIP:	macro
		dc.w	(((\1)&$ff)<<8)+((\2)&$fe)+1,$ffff
		endm

COP_SKIPV:	macro
		COP_SKIP \1,4
		endm

COP_SKIPH:	macro
		dc.w	(((\1)&$80)<<8)+((\2)&$fe)+1,$80ff
		endm

COP_NOP:	macro
		COP_MOVE 0,$1fe
		endm

COP_END:	macro
		dc.l	$fffffffe
		endm

; Copper blitter queue:

COPQ_WAITBLIT	macro
		move.l	#$10000,(a0)+
		endm

COPQ_END	macro
		move.l	#-2,(a0)+
		endm

COPQ_MOVEI	macro
		move.l	#(\2<<16)!\1,(a0)+
		endm

COPQ_MOVEW	macro
		move.w	#\2,(a0)+
		move.w	\1,(a0)+
		endm

COPQ_NOP	macro
		move.l	#$1fe<<16,(a0)+
		endm

CACHE_FLUSH	macro
		move.l	$4.w,a6
		cmp.w	#37,$14(a6)	; lib_version
		blo.b	.noflush\@
		jsr	_LVOCacheClearU(a6)
.noflush\@:
		endm


CACHE_OFF	macro
		move.l	$4.w,a6
		cmp.w	#37,$14(a6)	; lib_version
		blo.b	.noset\@
		moveq	#0,d0
		moveq	#-1,d1
		jsr	_LVOCacheControl(a6)
.noset\@:
		endm

SET_COP_PTR	macro
		move.w	\1,6(\2)
		swap	\1
		move.w	\1,2(\2)
		swap	\1
		add.l	#8,\2
		endm
