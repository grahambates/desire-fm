		include	_main.i
		include	effects/dots.i

********************************************************************************
* 3D object dots plotter
********************************************************************************

; Idea: ring buffer for VeriticesOut
; allow larger lerp

;
; [Initial object data]-----------
;                                |
;                                V
; [IncDecCode] ---------> [TransformCode] ----------> [VerticesOut] ------> [PlotCode]
;               updates         ^_|        generates                 blit
;               (lerp)        blit to
;                            duplicate
;

********************************************************************************
* Constants:
********************************************************************************

; number of vertices to draw on each frame
; (half of total object vertices)
VERTS = (Object1DeltasE-Object1Deltas)/3
OBJ_SIZE = VERTS*2*3

; Display window:
DIW_W = 288
DIW_H = 256
BPLS = 3

; Screen buffer:
SCREEN_W = 256
SCREEN_H = 256

TEXT_W = 64
TEXT_H = 12
TEXT_BW = TEXT_W/8

;-------------------------------------------------------------------------------
; Derived

COLORS = 1<<BPLS

SCREEN_BW = SCREEN_W/16*2		; byte-width of 1 bitplane line
SCREEN_BPL = SCREEN_BW*SCREEN_H		; bitplane offset (non-interleaved)

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
* Entry points:
********************************************************************************

FRAME_LERP1 = 28*3000/BPM		; formula for beats to frames
FRAME_LERP2 = 60*3000/BPM
FRAME_LERP3 = 90*3000/BPM

NORM_SPEED = 7

S = 1*3000/BPM

Script:
		dc.l	0,CmdLerpWord,$8000,8,WaveVol

		dc.l	S,CmdMoveIL,Pal,PalOut
		dc.l	S+8,CmdMoveIL,BlankData,PalOut
		dc.l	S+8+8,CmdMoveIL,Pal,PalOut
		dc.l	S+8+8+4,CmdMoveIL,BlankData,PalOut
		dc.l	S+8+8+4+4+4,CmdMoveIL,Pal,PalOut
		dc.l	S+8+8+4+4+4+2,CmdMoveIL,BlankData,PalOut
		dc.l	S+8+8+4+4+4+2+2,CmdMoveIL,Pal,PalOut

		dc.l	FRAME_LERP1-$80,CmdSetBgTask,PrepareObj1

		dc.l	FRAME_LERP1,StartLerp
		dc.l	FRAME_LERP1,CmdLerpWord,7*2,3,RotationSpeedY
		dc.l	FRAME_LERP1,CmdLerpWord,3*2,4,RotationSpeedX
		dc.l	FRAME_LERP1,CmdLerpWord,1*2,5,RotationSpeedZ
		dc.l	FRAME_LERP1,CmdLerpWord,VERTS,8,Limit

		dc.l	FRAME_LERP2-$100,CmdSetBgTask,PrepareObj2
		dc.l	FRAME_LERP2,StartLerp

		dc.l	8+66*3000/BPM,CmdMoveIW,-NORM_SPEED*2,RotationSpeedY
		dc.l	8+66*3000/BPM,CmdMoveIW,-2*2,RotationSpeedX

		dc.l	8+74*3000/BPM,CmdMoveIW,-1*2,RotationSpeedZ
		dc.l	8+74*3000/BPM,CmdMoveIW,NORM_SPEED*2,RotationSpeedY

		dc.l	8+82*3000/BPM,CmdMoveIW,-NORM_SPEED*2,RotationSpeedY

		dc.l	FRAME_LERP3-$100,CmdSetBgTask,PrepareObj3
		dc.l	FRAME_LERP3,StartLerp

		dc.l	96*3000/BPM-(1<<6),CmdLerpPal,4,6,Pal,BlankData,PalOut

		dc.l	$8000

********************************************************************************
Dots_Pre:
********************************************************************************
		jsr	MemFlip

		ALLOC_CHIP PLOTS_CODE_SZ
		move.l	a0,WritePlotCode
		ALLOC_CHIP PLOTS_CODE_SZ
		move.l	a0,RunPlotCode

		move.l	WritePlotCode(pc),a0
		bsr	GeneratePlotCode
		move.l	RunPlotCode(pc),a0
		bra	GeneratePlotCode


********************************************************************************
Dots_Effect:
********************************************************************************
		jsr	MemFreeLast

		ALLOC_CHIP TRANS_CODE_SZ
		move.l	a0,TransformA
		move.l	a0,WriteTransformCode
		ALLOC_CHIP TRANS_CODE_SZ
		move.l	a0,TransformB
		move.l	a0,RunTransformCode

		ALLOC_CHIP VERTS*2*2
		move.l	a0,DrawVerticesOut
		ALLOC_CHIP VERTS*2*2
		move.l	a0,ViewVerticesOut

		; Clear view vertices
		WAIT_BLIT
		move.l	#$1000000,bltcon0(a6)
		clr.w	bltdmod(a6)
		move.l	a0,bltdpt(a6)
		move.w	#VERTS*64+2,bltsize(a6)

		ALLOC_CHIP SCREEN_BPL*4
		lea	ScreenBuffers,a1
		move.l	a0,(a1)+	; ClearBuffer

		WAIT_BLIT
		move.l	#$1000000,bltcon0(a6)
		clr.w	bltdmod(a6)
		move.l	a0,bltdpt(a6)
		move.w	#SCREEN_H*3*64+SCREEN_BW/2,bltsize(a6)
		lea	SCREEN_BPL(a0),a0
		move.l	a0,(a1)+	; DrawBuffer
		lea	SCREEN_BPL(a0),a0
		move.l	a0,(a1)+	; ViewBuffer
		lea	SCREEN_BPL(a0),a0
		move.l	a0,(a1)+	; PrevBuffer

		ALLOC_PUBLIC 256*256
		move.l	a0,MulsTable

		ALLOC_PUBLIC 1024
		move.l	a0,SampleBuf

INC_LIST_SZ = VERTS*3*4+2		; Max size based on sub/add per vertex + rts
; INC_LIST_SZ = 7500			; TODO: Realistic size
		ALLOC_PUBLIC INC_LIST_SZ*6
		move.l	a0,IncDecBuffA
		move.l	a0,ViewIncDec
		ALLOC_PUBLIC INC_LIST_SZ*6
		move.l	a0,IncDecBuffB
		move.l	a0,DrawIncDec

		ALLOC_PUBLIC OBJ_SIZE
		move.l	a0,ObjectA
		ALLOC_PUBLIC OBJ_SIZE
		move.l	a0,ObjectB

		PRINT_MEM_TOTALS

; copper danger flag - allow copper driven blits
		lea	custom,a6
		move.w	#2,copcon(a6)

		bsr	PokeCopper
		bsr	SetSprites

; Set bg image bpl
		lea	CopBplPtBg+2,a1
		move.l	#Bg,d0
		move.w	d0,4(a1)
		swap	d0
		move.w	d0,(a1)

		move.l	#Cop,cop1lc(a6)
		jsr	ResetFrameCounter

		lea	Script,a0
		jsr	Commander_Init


;-------------------------------------------------------------------------------
OsciLoop:
		bsr	SetCols
		bsr	GetOsciData
		bsr	DrawOsciData

		bsr	SwapBuffers
		bsr	PokeCopper

; Flash text
		move.w	LocalFrame+2,d0
		moveq	#0,d1
		cmp.w	#16*3000/BPM,d0	; start flashing on frame
		blt	.n
		move.w	#1,LightVisible	; Light comes on too
		bsr	SetLightCol
		move.w	LocalFrame+2,d0
		and.w	#1<<5,d0	; flash speed
		beq	.n
		moveq	#1,d1
.n:
		move.w	d1,TextVisible
		bsr	SetSprites

		lea	custom,a6
		jsr	VSyncWithBgTask

		move.l	LocalFrame,d0
		cmp.w	#FRAME_LERP1,d0
		blt	OsciLoop

		clr.w	TextVisible
		bsr	SetSprites

;-------------------------------------------------------------------------------
MainLoop:
		bsr	SetCols
		bsr	SetLightCol
		bsr	Update
		bsr	DoLerp
		bsr	BuildMatrix
		bsr	Transform
		bsr	Plot

		bsr	SwapBuffers
		bsr	PokeCopper

		lea	custom,a6
		WAIT_BLIT
		; move.w	#$f00,color00(a6)
		jsr	VSyncWithBgTask

		jsr	PartOver
		blt	MainLoop
		rts

********************************************************************************
* Routines:
********************************************************************************

SetCols:
		move.l	PalOut(pc),a0
		move.w	(a0)+,ColBg
		move.w	(a0)+,ColCont1
		move.w	(a0)+,ColCont2
		move.w	(a0)+,ColDot3
		rts

SetLightCol:
		moveq	#0,d0
		move.w	BpmFrame,d0
		lsl	#3,d0
		and.w	#SIN_MASK,d0
		add.w	d0,d0
		lea	Sin,a0
		move.w	(a0,d0),d0
		lsl.l	#4,d0
		swap	d0
		addq	#4,d0
		lsl.w	#8,d0
		move.w	d0,ColLight1
		add.w	#$500,d0
		move.w	d0,ColLight2
		rts

********************************************************************************
PrepareObj1:
		bsr	InitMulsTbl

		lea	Object1Deltas,a0
		move.l	ObjectA(pc),a1
		bsr	ApplyDeltasMirror

		lea	Object1Deltas,a0
		move.l	ObjectB(pc),a1
		bsr	ApplyDeltasMirror

		bsr	CopyOsciData

		move.l	ObjectB(pc),a0
		move.l	TransformA(pc),a1
		bsr	GenerateTransformCode
		move.l	TransformB(pc),a1
		bsr	GenerateTransformCode

		move.l	ObjectB(pc),a1
		move.l	ObjectA(pc),a2
		move.l	IncDecBuffA(pc),a0
		bsr	GenerateLerpCode
		move.l	IncDecBuffB(pc),a0
		bra	GenerateLerpCode

********************************************************************************
PrepareObj2:
		lea	Object2Deltas,a0
		move.l	ObjectB(pc),a1
		bsr	ApplyDeltas

		move.l	ObjectA(pc),a1
		move.l	ObjectB(pc),a2
		move.l	IncDecBuffA(pc),a0
		bsr	GenerateLerpCode
		move.l	IncDecBuffB(pc),a0
		bra	GenerateLerpCode

********************************************************************************
PrepareObj3:
		move.l	ObjectA(pc),a0
		move.w	#VERTS*3-1,d7
.l:		clr.w	(a0)+
		dbf	d7,.l
		move.l	ObjectB(pc),a1
		move.l	ObjectA(pc),a2
		move.l	IncDecBuffA(pc),a0
		bsr	GenerateLerpCode
		move.l	IncDecBuffB(pc),a0
		bra	GenerateLerpCode


********************************************************************************
GetOsciData:
		lea	P61_temp0,a0
		jsr	P61_osc
		; Output:
		; d0 = WrapCount.w
		; d1 = Count.w
		; a2= SamplePtr

		; if d0.w>0:
		; d2 = LoopEndPtr
		; d4 = Replen
		; a1 = LoopStartPtr

		; skip if d1 = 0
		tst.w	d1
		beq	.end

		move.l	SampleBuf(pc),a0

		; If d0<=0, just read d1 bytes from a2-ptr.
		tst.w	d0
		bgt	.gt0
		cmp.w	#256,d1
		blt	.ok1
		move.w	#256,d1
.ok1:
		subq	#1,d1
.l:		move.b	(a2)+,(a0)+
		dbf	d1,.l

		rts

.gt0:
		;If d0>0, subtract it from d1 and read d1 bytes from a2-ptr.
		sub.w	d0,d1

		cmp.w	#256,d1
		blt	.ok2
		move.w	#256,d1
.ok2:
		subq	#1,d0
.l0:		move.b	(a2)+,(a0)+
		dbf	d1,.l0

		;Backup Loopstart in a1 to (for example) a3.
		;Then read d0 bytes from a1-ptr, if you hit the
		;LoopEnd address in d2, then read the remaining bytes from Loopstart that
		;you saved in a3.
		move.l	a1,a3
		cmp.w	#256,d0
		blt	.ok3
		move.w	#256,d0
.ok3:
		subq	#1,d0
.l1:		move.b	(a1)+,(a0)+
		cmp.l	d2,a1
		bne	.ok
		move.l	a3,a1
.ok:
		dbf	d0,.l1
.end:
		rts


********************************************************************************
DrawOsciData:
		move.l	SampleBuf(pc),a0
		move.l	DrawVerticesOut(pc),a1
		moveq	#0,d0
		moveq	#0,d1
		move.b	#$80,d2
.l2:
		move.b	(a0)+,d1
		ext.w	d1
		muls	WaveVol(pc),d1
		swap	d1
		add.b	d2,d1		; center Y
		move.b	d1,(a1)+
		move.b	d0,(a1)+
		addq.b	#1,d0
		bcc	.l2

		move.w	#256,Limit
		bra	Plot


********************************************************************************
CopyOsciData:
		move.l	SampleBuf(pc),a0
		move.l	ObjectB(pc),a1
		lea	VERTS*3(a1),a2
		moveq	#-127,d0
		moveq	#0,d1
		move.w	#256-1,d7
.l:
		move.b	d0,(a1)+	; x
		move.b	d0,(a2)+	; x
		addq.b	#1,d0
		move.b	(a0)+,d1
		asr.b	#1,d1
		move.b	d1,(a1)+	; y
		move.b	d1,(a2)+	; y
		clr.b	(a1)+		; z
		clr.b	(a2)+		; z
		dbf	d7,.l
		rts



********************************************************************************
; Generate speed code for matrix transformation
;-------------------------------------------------------------------------------
; a0 - initial vertex data
; a1 - code output
;-------------------------------------------------------------------------------
; Object vertex data is baked into code which does a 3D matrix tranformation.
;
; The transformation is done without using MULs by using a 256*256 LUT.
; The x,y and z components are used as row offsets against six tables, pre-offset
; by each of the matrix component values (A-F).
;
; This is all made possible because the cooridates are bytes values, and we are
; doing an orthographic transformation, so only need 6 matrix components, and
; therefore have enough registers.
;
; The output of the speed code is an array of transformed x/y coordinates, which
; can then be blitted into oue plot routine.
GenerateTransformCode:
		move.w	#VERTS-1,d7
.l:
		move.b	(a0)+,d0	; x
		move.b	(a0)+,d1	; y
		move.b	(a0)+,d2	; z
; These will be Y values in the MUL lookup, so need to be *256
		lsl.w	#8,d0
		lsl.w	#8,d1
		lsl.w	#8,d2
		neg.b	d1		; invert Y axis
TRANS_CODE_LOOP_SZ = 6*4+2*2		; bytes
; y'=D*x+E*y+F*z
		move.w	#$102c,(a1)+	; move.b x(a4),d0	; D*x
		move.w	d0,(a1)+
		move.w	#$d02d,(a1)+	; add.b y(a5),d0	; E*y
		move.w	d1,(a1)+
		move.w	#$d02e,(a1)+	; add.b z(a6),d0	; F*z
		move.w	d2,(a1)+
		move.w	#$10c0,(a1)+	; move.b d0,(a0)+
; x'=A*x+B*y+C*z
		move.w	#$1029,(a1)+	; move.b x(a1),d0	; A*x
		move.w	d0,(a1)+
		move.w	#$d02a,(a1)+	; add.b y(a2),d0	; B*y
		move.w	d1,(a1)+
		move.w	#$d02b,(a1)+	; add.b z(a3),d0	; C*z
		move.w	d2,(a1)+
		move.w	#$10c0,(a1)+	; move.b d0,(a0)+

		dbf.w	d7,.l
		move.w	#$4e75,(a1)+	; rts
		rts

TRANS_CODE_SZ = TRANS_CODE_LOOP_SZ*VERTS+2


********************************************************************************
; a0 - deltas
; a1 - out
;-------------------------------------------------------------------------------
ApplyDeltas:
		lea	VERTS*2(a0),a2	; y
		lea	VERTS*2(a2),a3	; z
		moveq	#0,d0
		moveq	#0,d1
		moveq	#0,d2
		move.w	#VERTS*2-1,d7
.l:
		add.b	(a0)+,d0
		add.b	(a2)+,d1
		add.b	(a3)+,d2
		move.b	d0,(a1)+
		move.b	d1,(a1)+
		move.b	d2,(a1)+
		dbf	d7,.l
		rts

********************************************************************************
; a0 - deltas
; a1 - out
;-------------------------------------------------------------------------------
ApplyDeltasMirror:
		lea	VERTS(a0),a2	; y
		lea	VERTS(a2),a3	; z
		lea	OBJ_SIZE/2(a1),a4
		moveq	#0,d0
		moveq	#0,d1
		moveq	#0,d2
		move.w	#VERTS-1,d7
.l:
		add.b	(a0)+,d0
		add.b	(a2)+,d1
		add.b	(a3)+,d2
		move.b	d0,(a1)+
		move.b	d1,(a1)+
		move.b	d2,(a1)+
		neg.b	d2
		move.b	d0,(a4)+
		move.b	d1,(a4)+
		move.b	d2,(a4)+
		neg.b	d2
		dbf	d7,.l
		rts


********************************************************************************
; Populate multiplication lookup table for transform
; [-127 to 127] * [-128 to 127] >> 7
; output + 128 to keep positive offsets
;-------------------------------------------------------------------------------
InitMulsTbl:
		move.l	MulsTable(pc),a0
		move.w	#-128,d0	; d0 = x = -128-127
		move.w	#256-1,d7
.xLoop:
		move.w	d0,d1
		add.w	#128,d1		; + 128 to keep positive offsets
		lsl.w	#7,d1		; acc =  x * -128
		neg.w	d1
		move.w	#256-1,d6
.yLoop:
		move.w	d1,d2
		asr.w	#7,d2		; d2 = (x*y)>>7
		move.b	d2,(a0)+	; write to table
		add.w	d0,d1		; acc += x
		dbf	d6,.yLoop

		addq	#1,d0
		dbf	d7,.xLoop
		rts


********************************************************************************
; Generate speed code for pixel plot
;-------------------------------------------------------------------------------
; a0 - code output
;-------------------------------------------------------------------------------
; The actual dot plotting is done by one big unrolled sequence of bsets.
; The byte offsets and bit numbers are updated each frame by blitting the
; transformed vertex data.
GeneratePlotCode:
		move.w	#$8e8,d0
		move.w	#VERTS-1,d7
.l:
		move.w	d0,(a0)+	; bset.b  #0,0(a0)
		clr.l	(a0)+
		dbf	d7,.l
		move.w	#$4e75,(a0)+	; rts
		rts

PLOTS_CODE_SZ = VERTS*6+2


********************************************************************************
; Generate increment/decrement list speedcode for LERP
;-------------------------------------------------------------------------------
; a0 - inc/dec lists
; a1 - src object
; a2 - dest object
;-------------------------------------------------------------------------------
; Because the object vertex data has now been transformed into speed code in
; TransformCode, in order to LERP to a new object we need gradually modify the
; offsets in that code. And for it to be performant it needs to be done with...
; yet more speed code!
;
; Each vertex component appears twice in the transform code, but we can just
; alter the first occurence and later copy it with the blitter.
;
; Over 64 frames we need to increment/decrement each offset in the transform
; code by the total delta to get to the new value.
;
; Because these are integers we'll be incrementing different amounts on each
; frame to reach the total delta *but* it turns out we can do this with 7
; possible lists of inc/dec values: one for each bit.
;
; Looking at the bits set in the delta, we know how many times we need to
; increment over the 64 frames to get that value.
; e.g.
; Bit 0 represents a value of 1, so we only need to increment once over the 64 frames
; Bit 3 is 8, so we need to increment 8 times
; Bit 6 is 64, so we need to increment on every frame
;
; For each bit 0-6, we generate a routine that increments the values where the
; delta contains that bit (or decrements if the delta is negative).
;
; We can determine which list(s) to apply based on the bits in the frame number:
;
;                                                                    frame mask:
;                                                                0        111111
;                                1                               1        *11111
;                2               2               2               2        **1111
;        3       3       3       3       3       3       3       3        ***111
;    4   4   4   4   4   4   4   4   4   4   4   4   4   4   4   4        ****11
;  5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5        *****1
; 6666666666666666666666666666666666666666666666666666666666666666        ******
; 7777777777777777777777777777777777777777777777777777777777777777        ****** (+-2)
; ----------------------------------------------------------------
; 0                                                             63
;
; There are a few things we can improve though:
; To get a more evenly distributed increment amount per frame, we can offset these:
;
;                                                                0
;                               1                               1   +1
;              2               2               2               2    +2
;     3       3       3       3       3       3       3       3     +3
;    4   4   4   4   4   4   4   4   4   4   4   4   4   4   4   4  +4
; 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5   +5
; 6666666666666666666666666666666666666666666666666666666666666666  +6
; 7777777777777777777777777777777777777777777777777777777777777777  +7
; ----------------------------------------------------------------
; 0                                                             63
;
; We can also combine the lists, because they always align in the same groups.
; A '3' frame is always a '5' and a '6' frame.
; Rather than calling 3 routines that each increment by one, we can call a single
; routine that increments between 1-3.
;
; The combinations are:
; 046, 156, 26, 356, 46, 56, 6
;
; For 32 steps:
;-------------------------------------------------------------------------------
;                                   frame mask:
;                                0        11111
;                1               1        *1111
;        2       2       2       2        **111
;    3   3   3   3   3   3   3   3        ***11
;  4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4        ****1
; 55555555555555555555555555555555        *****
; 66666666666666666666666666666666        ***** x2
; 77777777777777777777777777777777        ***** x4
; --------------------------------
; 0                              31
;
;                                   frame mask:
;                                0        11111
;               1               1         *1111
;      2       2       2       2          **111
;     3   3   3   3   3   3   3           ***11
;  4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4        ****1
; 55555555555555555555555555555555        *****
; 66666666666666666666666666666666        ***** x2
; 77777777777777777777777777777777        ***** x4
; --------------------------------
; 0                              31
;
; 045 15 245 35 45 5
GenerateLerpCode:
		; Move src/dest to free up all address regs
		exg	a1,d2
		exg	a2,d3
		; Get inc/dec code addresses x 6
		lea	INC_LIST_SZ(a0),a1
		lea	INC_LIST_SZ(a1),a2
		lea	INC_LIST_SZ(a2),a3
		lea	INC_LIST_SZ(a3),a4
		lea	INC_LIST_SZ(a4),a5
		; Offset of value in transform code potentially being inc/decremented
		moveq	#2,d4

		move.w	#VERTS-1,d7	; each vertex
.l0:

; local macros for use in rept:
;-------------------------------------------------------------------------------
		macro	.doDelta
		; Calculate delta
		exg	a1,d2		; restore src/dest while we read from them
		exg	a2,d3
		move.b	(a2)+,d0
		move.b	(a1)+,d1
		exg	a1,d2
		exg	a2,d3
		ext.w	d0
		ext.w	d1
		sub.w	d1,d0		; d0 = delta

		move.w	#$5028,d1	; instruction template: default addq.b for positive
		tst.w	d0
		bge	.notNeg\@
		neg.w	d0		; make abs
		move.w	#$5128,d1	; subq.b for negative
.notNeg\@:
		.checkBit 0,4,5
		.checkBit 1,5
		.checkBit 2,4,5
		.checkBit 3,5
		.checkBit 4,5
		.checkBit 5

		; Increment offset for x/y/z component
		addq	#4,d4
		endm

;-------------------------------------------------------------------------------
; Checks 1-3 bits in delta value, and writes inc/dec instruction with amount for
; number of bits set
		macro	.checkBit
		moveq	#0,d6		; add/sub amount
		btst	#\1,d0
		beq	.notSet\@
		addq	#1,d6
.notSet\@:
		ifnc	"\2",""
		btst	#\2,d0
		beq	.notSet2\@
		addq	#1,d6
.notSet2\@:
		endc
		ifnc	"\3",""
		btst	#\3,d0
		beq	.notSet3\@
		addq	#1,d6
.notSet3\@:
		endc
; Always check bit 6, and double increment
		btst	#6,d0
		beq	.notSet4\@
		addq	#2,d6
; Always check bit 7, and quadruple increment
.notSet4\@:
		btst	#7,d0
		beq	.notSet5\@
		addq	#4,d6
.notSet5\@:

		; Any bits set?
		tst.w	d6
		beq	.done\@
		lsl.w	#8,d6		; combine inc amount with addq/subq instruction template
		add.w	d6,d6
		add.w	d1,d6
		; write instruction
		move.w	d6,(a\1)+	; addq/subq.b #x,...
		move.w	d4,(a\1)+	; offset(a0)
.done\@:
		endm

		; Repeat for vertex components x/y/z:
		rept	3
		.doDelta
		endr

		; Increment offset for next vertex
		add.w	#TRANS_CODE_LOOP_SZ-12,d4
		dbf	d7,.l0

		; Finalise all inc/dec code routines:
		move.w	#$4e75,d0	; rts
		move.w	d0,(a0)+
		move.w	d0,(a1)+
		move.w	d0,(a2)+
		move.w	d0,(a3)+
		move.w	d0,(a4)+
		move.w	d0,(a5)+

		exg	a1,d2
		exg	a2,d3
		rts


********************************************************************************
; Start new lerp
;-------------------------------------------------------------------------------
; a0 - list a
; a1 - list b
;-------------------------------------------------------------------------------
StartLerp:
		move.w	#65,LerpStepsRemaining
		rts


********************************************************************************
; Apply increment/decrement lists to update transform code if LERP in progress
;-------------------------------------------------------------------------------
DoLerp:
		; Lerp in progress?
		move.w	LerpStepsRemaining(pc),d0
		lsr	#1,d0		; /2 because of alternating update lists
		ble	.done

		sub.w	#1,LerpStepsRemaining

		move.l	WriteTransformCode(pc),a0 ; transform code to update
		move.l	ViewIncDec(pc),a1 ; inc/dec lists to apply

		; Frame mask tells us which list should be applied
		move.w	#%11111,d1	; d1 = frame mask
		moveq	#6-1,d7
.l:
		; does frame match mask?
		move.w	d1,d2
		and.w	d0,d2
		cmp.w	d1,d2
		bne	.noMatchMask
		jmp	(a1)		; apply the list - notice this doesn't return - only first match applied
.noMatchMask:
		addq	#1,d0		; adjust frame number for even distribution
		lsr.w	#1,d1		; shift mask
		lea	INC_LIST_SZ(a1),a1 ; next list
		dbf	d7,.l
		rts

.done:
		rts


********************************************************************************
SwapBuffers:
; Rotate screen buffers
		lea	ScreenBuffers(pc),a0
		movem.l	(a0),d0-d3
		exg	d3,d0
		exg	d3,d2
		exg	d2,d1
		movem.l	d0-d3,(a0)

; Other double buffered data
		lea	DoubleBuffers,a5
		movem.l	(a5),d0-d7
		exg	d0,d1
		exg	d2,d3
		exg	d4,d5
		exg	d6,d7
		movem.l	d0-d7,(a5)

		rts


********************************************************************************
; Set address pointers in copperlist
;-------------------------------------------------------------------------------
PokeCopper:
; Set bpl pointers
		lea	CopBplPt+2,a1
		move.l	ViewBuffer(pc),d0
		move.l	PrevBuffer(pc),d1
		move.w	d0,4(a1)
		swap	d0
		move.w	d0,(a1)
		move.w	d1,12(a1)
		swap	d1
		move.w	d1,8(a1)

; Set blit address pointers
; Clear:
		move.l	ClearBuffer(pc),d0
		move.w	d0,CopCleardptl
		swap	d0
		move.w	d0,CopCleardpth

; Transform code:
; dest
		lea	CopTrans,a0
		move.l	RunTransformCode(pc),d0
		addq	#2,d0
		move.l	d0,d1
		add.l	#TRANS_CODE_LOOP_SZ/2,d1
		moveq	#3-1,d7
.l:
; src
		move.w	d0,6(a0)	;aptl
		swap	d0
		move.w	d0,2(a0)	;apth
		swap	d0
		move.w	d1,14(a0)	;dptl
		swap	d1
		move.w	d1,10(a0)	;dpth
		swap	d1

		lea	COP_TRANS_LOOP_SZ(a0),a0
		addq	#4,d0
		addq	#4,d1
		dbf	d7,.l

; Plots code:
; dest * 2
		move.l	RunPlotCode(pc),d0
		addq	#4,d0
		move.w	d0,CopPlot1dptl
		swap	d0
		move.w	d0,CopPlot1dpth
		swap	d0
		subq	#2,d0
		move.w	d0,CopPlot2dptl
		swap	d0
		move.w	d0,CopPlot2dpth
; src * 2
		move.l	ViewVerticesOut(pc),d1
		move.w	d1,CopPlot1aptl
		move.w	d1,CopPlot2aptl
		swap	d1
		move.w	d1,CopPlot1apth
		move.w	d1,CopPlot2apth

		rts


********************************************************************************
SetSprites:
		lea	CopSprPt+2,a1

		; 1 empty
		lea	BlankData,a0
		move.l	a0,d0
		swap	d0
		move.w	d0,(a1)
		move.w	a0,4(a1)
		lea	8(a1),a1
		; 2 - LED Light
		tst.w	LightVisible
		beq	.noLight
		lea	SprLight+2,a0
.noLight:
		move.l	a0,d0
		swap	d0
		move.w	d0,(a1)
		move.w	a0,4(a1)
		lea	8(a1),a1
		; 3 - controls
		lea	SprControls+2,a0
		move.l	a0,d0
		swap	d0
		move.w	d0,(a1)
		move.w	a0,4(a1)
		lea	8(a1),a1
		; 4 - right border
		lea	SprR+2,a0
		move.l	a0,d0
		swap	d0
		move.w	d0,(a1)
		move.w	a0,4(a1)
		lea	8(a1),a1

		tst.w	TextVisible
		beq	.setNull
		lea	SprText,a2
		move.l	a2,a3
		moveq	#4-1,d7
.l:
		move.w	(a3)+,d1
		lea	(a2,d1),a0
		move.l	a0,d0
		swap	d0
		move.w	d0,(a1)
		move.w	a0,4(a1)
		lea	8(a1),a1
		dbf	d7,.l
		rts

.setNull:
		lea	BlankData,a0
		move.l	a0,d0
		swap	d0
		moveq	#4-1,d7
.l1:
		move.w	d0,(a1)
		move.w	a0,4(a1)
		lea	8(a1),a1
		dbf	d7,.l1
		rts

********************************************************************************
; Update object transform for this frame
;-------------------------------------------------------------------------------
Update:
		movem.w	Rotation(pc),d0-d5
		add.w	d3,d0
		add.w	d4,d1
		add.w	d5,d2
		movem.w	d0-d2,Rotation
		rts


********************************************************************************
; Calculate rotation matrix
;
; This is generated once per frame and used as offset in the transform routine
;
; A = cos(Y)*cos(Z)    B = sin(X)*sin(Y)*cos(Z)−cos(X)*sin(Z)     C = cos(X)*sin(Y)*cos(Z)+sin(X)*sin(Z)
; D = cos(Y)*sin(Z)    E = sin(X)*sin(Y)*sin(Z)+cos(X)*cos(Z)     F = cos(X)*sin(Y)*sin(Z)−sin(X)*cos(Z)
;-------------------------------------------------------------------------------
BuildMatrix:
		lea	Sin,a4
		lea	Cos,a5
		; Nasty hack to prevent overflow - restored later
		move.w	#$3fff,512(a4)
		move.w	#$c001,1536(a4)

		movem.w	Rotation(pc),d5-d7
		; Overflow example
		; move.w	#$200,d5
		; move.w	#$e00,d6
		; move.w	#$600,d7
		move.w	#SIN_MASK*2,d3
		and.w	d3,d5
		and.w	d3,d6
		and.w	d3,d7
		lea	Matrix(pc),a0

sin		equr	a4
cos		equr	a5
x		equr	d5
y		equr	d6
z		equr	d7

		move.w	(sin,x),d2
		FPMULS14 (sin,y),d2	; d2 = sin(X)*sin(Y)
		move.w	(cos,x),d3
		FPMULS14 (sin,z),d3	; d3 = sin(Z)*cos(X)
		move.w	(cos,x),d4
		FPMULS14 (cos,z),d4	; d4 = cos(X)*cos(Z)

; A = cos(Y)*cos(Z)
		move.w	(cos,y),d0
		FPMULS14 (cos,z),d0	; cos(Y)*cos(Z)
		lsr.w	#7,d0
		move.w	d0,(a0)+
; B = sin(X)*sin(Y)*cos(Z)−cos(X)*sin(Z)
		move.w	d2,d0		; sin(X)*sin(Y)
		FPMULS14 (cos,z),d0	; sin(X)*sin(Y)*cos(Z)
		move.w	(cos,x),d1	; cos(X)
		FPMULS14 (sin,z),d1	; cos(X)*sin(Z)
		sub.w	d1,d0		; sin(X)*sin(Y)*cos(Z)-cos(X)*sin(Z)
		lsr.w	#7,d0
		move.w	d0,(a0)+
; C = cos(X)*sin(Y)*cos(Z)+sin(X)*sin(Z)
		move.w	d4,d0		; cos(X)*cos(Z)
		FPMULS14 (sin,y),d0	; cos(X)*cos(Z)*sin(Y)
		move.w	(sin,x),d1	; sin(X)
		FPMULS14 (sin,z),d1	; sin(X)*sin(Z)
		add.w	d1,d0		; cos(X)*cos(Z)*sin(Y)+sin(X)*sin(Z)
		lsr.w	#7,d0
		move.w	d0,(a0)+
; D = cos(Y)*sin(Z)
		move.w	(cos,y),d0
		FPMULS14 (sin,z),d0	; cos(Y)*sin(Z)
		lsr.w	#7,d0
		move.w	d0,(a0)+
; E = sin(X)*sin(Y)*sin(Z)+cos(X)*cos(Z)
		move.w	d2,d0		; sin(X)*sin(Y)
		FPMULS14 (sin,z),d0	; sin(X)*sin(Y)*sin(Z)
		add.w	d4,d0		; sin(X)*sin(Y)*sin(Z)+cos(X)*cos(Z)
		lsr.w	#7,d0
		move.w	d0,(a0)+
; F = cos(X)*sin(Y)*sin(Z)−sin(X)*cos(Z)
		move.w	d3,d0		; sin(Z)*cos(X)
		FPMULS14 (sin,y),d0	; cos(X)*sin(Y)*sin(Z)
		move.w	(sin,x),d1	; sin(X)
		FPMULS14 (cos,z),d1	; sin(X)*cos(Z)
		sub.w	d1,d0
		lsr.w	#7,d0
		move.w	d0,(a0)+

		; Nasty hack restore
		move.w	#$4000,512(a4)
		move.w	#$c000,1536(a4)

		rts


********************************************************************************
Transform:
; Transform vertices
		move.l	MulsTable(pc),a1
		lea	255*128(a1),a1	; start at middle of table
		; Offset table for each value in matrix
		movem.w	Matrix(pc),d1-d6
		lea	(a1,d6.w),a6
		lea	(a1,d5.w),a5
		lea	(a1,d4.w),a4
		lea	(a1,d3.w),a3
		lea	(a1,d2.w),a2
		lea	(a1,d1.w),a1
		move.l	DrawVerticesOut(pc),a0
		move.l	RunTransformCode(pc),-(sp) ; jump to code without using a register
		rts


********************************************************************************
Plot:
; Call generated plot routine
		move.l	DrawBuffer(pc),a0
		move.l	RunPlotCode(pc),a1

; Limit range:
		move.w	Limit(pc),d0
		mulu	#6,d0
		lea	(a1,d0.w),a2
		move.l	a2,LimitRestore
		move.w	#$4e75,(a2)	; replace bset with rts

; Call plots routine
		jsr	(a1)

		move.l	LimitRestore(pc),a2
		move.w	#$8e8,(a2)	; restore bset
		rts


********************************************************************************
Vars:
********************************************************************************

LerpStepsRemaining: dc.l 0		; Number of steps remaining for active lerp

Limit:		dc.w	0		; Max vertices to draw
LimitRestore:	dc.l	0		; Remember where we replaced a bset with an rts so we can revert it

Rotation:	ds.w	3		; x/y/z rotation
RotationSpeeds:
RotationSpeedX:	ds.w	1
RotationSpeedY:	ds.w	1
RotationSpeedZ:	ds.w	1

Matrix:		ds.w	6

DeltaTotals:
XTotal:		dc.w	0
YTotal:		dc.w	0
ZTotal:		dc.w	0

SampleBuf:	dc.l	0
WaveVol:	dc.w	0

TextVisible:	dc.w	0
LightVisible:	dc.w	0

;-------------------------------------------------------------------------------
; Quadruple buffering?
; Previous buffer is still visible, to allow updating half of vertices on each frame
;-------------------------------------------------------------------------------
ScreenBuffers:
;-------------------------------------------------------------------------------
ClearBuffer:	dc.l	0
DrawBuffer:	dc.l	0
ViewBuffer:	dc.l	0
PrevBuffer:	dc.l	0

;-------------------------------------------------------------------------------
DoubleBuffers:
;-------------------------------------------------------------------------------
; Plot generated code buffers
WritePlotCode:	dc.l	0
RunPlotCode:	dc.l	0
; Transform genereated code buffers
WriteTransformCode:
		dc.l	0
RunTransformCode:
		dc.l	0
; Transformed vertex buffers
ViewVerticesOut:
		dc.l	0
DrawVerticesOut:
		dc.l	0
; Increment/decrement lists
ViewIncDec:	dc.l	0
DrawIncDec:	dc.l	0

; Allocated RAM:
MulsTable:	dc.l	0
IncDecBuffA:	dc.l	0
IncDecBuffB:	dc.l	0
ObjectA:	dc.l	0
ObjectB:	dc.l	0
TransformA:	dc.l	0
TransformB:	dc.l	0

PalOut:		dc.l	BlankData


********************************************************************************
		data
********************************************************************************

Object1Deltas:
		include	"data/head.i"
Object1DeltasE:
		even

Object2Deltas:
		include	"data/hand.i"
		even

Pal:
		dc.w	$034
		dc.w	$123
		dc.w	$234
		dc.w	$fff


*******************************************************************************
		data_c
*******************************************************************************

Bg:		incbin	"data/scope-bg.BPL"
SprR:		incbin	"data/scope-r.SPR"
SprControls:	incbin	"data/scope-controls.SPR"
SprLight:	incbin	"data/scope-light.SPR"
SprText:	incbin	"data/scope-text.SPR"

;--------------------------------------------------------------------------------
; Main copper list:
Cop:
		dc.w	dmacon,DMAF_SETCLR!DMAF_SPRITE ; Enable sprite DMA
		dc.w	diwstrt,DIW_STRT
		dc.w	diwstop,DIW_STOP
		dc.w	ddfstrt,DDF_STRT
		dc.w	ddfstop,DDF_STOP
CopBplCon:
		dc.w	bplcon0,BPLS<<12+$200
		dc.w	bplcon1,0
CopBplMod:
		dc.w	bpl1mod,DIW_MOD
		dc.w	bpl2mod,DIW_MOD
CopBplPt:
		dc.w	bpl0pt,0
		dc.w	bpl0ptl,0
		dc.w	bpl1pt,0
		dc.w	bpl1ptl,0
CopBplPtBg:
		dc.w	bpl2pt,0
		dc.w	bpl2ptl,0
CopSprPt:
		rept	8*2
		dc.w	sprpt+REPTN*2,0
		endr
CopPal:
		dc.w	color00,$0
		dc.w	color01,$0
		dc.w	color02,$0
		dc.w	color03
ColDotMid:	dc.w	$5a9
		; Main pal
		dc.w	color04
ColBg:		dc.w	$034
		dc.w	color05
ColDot1:	dc.w	$5a9
		dc.w	color06
ColDot2:	dc.w	$5fd
		dc.w	color07
ColDot3:	dc.w	$fff
		; 1/2 - light
		dc.w	color17,$011
		dc.w	color18
ColLight1:	dc.w	$800
		dc.w	color19
ColLight2:	dc.w	$f00
		; 3/4 - border / controls
		dc.w	color21,$000
		dc.w	color22
ColCont1:	dc.w	$123
		dc.w	color23
ColCont2:	dc.w	$234
		; 5/6 - text
		dc.w	color25
ColText1:	dc.w	$5fd
		; 7/8 - text
		dc.w	color29
ColText2:	dc.w	$5fd


BLIT_VERTS	macro
		ifgt	VERTS-1023
		dc.w	bltsize,((VERTS-1023)<<6)!1
		dc.w	1,0
		dc.w	1,0
		dc.w	bltsize,(1023<<6)!1
		dc.w	1,0
		dc.w	1,0
		else
		dc.w	bltsize,(VERTS<<6)!1
		dc.w	1,0
		dc.w	1,0
		endc
		endm

;--------------------------------------------------------------------------------
; Copper driven blits:
; Clear screen
		dc.w	bltcon0,$0100
		dc.w	bltcon1,$0000
		dc.w	bplcon2,%1100100
		dc.w	bltdmod,SCREEN_BW-DIW_BW
		dc.w	bltdpth
CopCleardpth:	dc.w	0
		dc.w	bltdptl
CopCleardptl:	dc.w	0
		dc.w	bltsize,DIW_H*64+DIW_BW/2
		dc.w	1,0		; blit wait
		dc.w	1,0

; Transform - duplicate vertex offsets in transform code
		dc.w	bltcon0,$9f0
		dc.w	bltafwm,-1
		dc.w	bltalwm,-1
		dc.w	bltamod,TRANS_CODE_LOOP_SZ-2
		dc.w	bltdmod,TRANS_CODE_LOOP_SZ-2
CopTrans:
		rept	3		; two copies for each component
		dc.w	bltapth,0	; 2
		dc.w	bltaptl,0	; 6
		dc.w	bltdpth,0	; 10
		dc.w	bltdptl,0	; 14
		BLIT_VERTS
		endr
		ifgt	VERTS-1023
COP_TRANS_LOOP_SZ = 10*4
		else
COP_TRANS_LOOP_SZ = 7*4
		endc

; Plot: Byte offsets:
		dc.w	bltcon0,$39f0
		dc.w	bltafwm,-1
		dc.w	bltalwm,(-1)<<3
		dc.w	bltamod,0
		dc.w	bltdmod,4
		dc.w	bltapth
CopPlot1apth:	dc.w	0
		dc.w	bltaptl
CopPlot1aptl:	dc.w	0
		dc.w	bltdpth
CopPlot1dpth:	dc.w	0
		dc.w	bltdptl
CopPlot1dptl:	dc.w	0
		BLIT_VERTS
; Plot: Bit to set:
		dc.w	bltcon0,$90f
		dc.w	bltalwm,-1
		dc.w	bltapth
CopPlot2apth:	dc.w	0
		dc.w	bltaptl
CopPlot2aptl:	dc.w	0
		dc.w	bltdpth
CopPlot2dpth:	dc.w	0
		dc.w	bltdptl
CopPlot2dptl:	dc.w	0
		BLIT_VERTS

		dc.l	-2
