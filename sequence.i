		include	effects/intro.i
		include	effects/city.i
		include	effects/uvtbl.i
		include	effects/checkerboard.i
		include	effects/dots.i
		include	effects/logo.i
		include	effects/roto.i
		include	effects/graff.i

TEST_PART = 2

Pos		set	0
PART		macro
		dc.l	\1		; precalc
		dc.l	\2		; effect
Pos		set	Pos+\3
		dc.w	Pos
		endm

		rsreset
Part_Pre	rs.l	1
Part_Effect	rs.l	1
Part_Length	rs.w	1
Part_SIZEOF	rs.b	0

********************************************************************************
Sequence:
		PART	0,Intro_Effect,4
		PART	UvTbl_Pre,UvTbl_Effect,4
		PART	Dots_Pre,Dots_Effect,6
		PART	0,Graff_Effect,4
		PART	0,City_Effect,4
		PART	Roto_Pre,Roto_Effect,4
		PART	CB_Pre,CB_Effect,4
		PART	Logo_Pre,Logo_Effect,1
		dc.l	0,0
