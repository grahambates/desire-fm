		incdir	"include"
		incdir	"../include"

		include	"include/hw.i"
		include	"include/debug.i"
		include	"include/macros.i"
		include	"include/blitbits.i"

		include	"common/tables.i"
		include	"common/commander.i"
		include	"common/memory.i"
		include	"common/transitions.i"

		xref	_start

		xref	WaitEOF
		xref	WaitRaster
		xref	WaitBlitter

		xref	GlobalFrame
		xref	LocalFrame
		xref	BpmFrame

		xref	PartOver

		xref	VSync
		xref	VSyncWithBgTask
		xref	SetBgTask
		xref	CmdSetBgTask

		xref	VBlankInterrupt
		xref	BlitterInterrupt

		xref	BlankData
		xref	FillData

		xref	P61_End
		xref	P61_Master	; Master volume (w)
		xref	P61_Tempo	; Use tempo (w)
		xref	P61_Play	; Play flag (w)
		xref	P61_E8		; E8 info (w)
		xref	P61_VBR		; Use VBR (ptr)
		xref	P61_Pos		; Position
		xref	P61_Patt	; Pattern
		xref	P61_CRow	; Row

		; xref	P61_visuctr0
		; xref	P61_visuctr1
		; xref	P61_visuctr2
		; xref	P61_visuctr3

		xref	P61_osc
		xref	P61_temp0
		xref	P61_temp1
		xref	P61_temp2
		xref	P61_temp3

BPM = 175

