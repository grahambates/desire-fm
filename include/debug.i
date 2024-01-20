		ifnd	DEBUG_I
DEBUG_I		set	1

; Debug mode: off by default
		ifnd	DEBUG
DEBUG = 0
		endc

********************************************************************************
* CPU idle status:
********************************************************************************

;-------------------------------------------------------------------------------
; CPU idle mode on
DebugStartIdle	macro
		DebugCmd DEBUG_CMD_SET_IDLE,#1,#0,#0
		endm

;-------------------------------------------------------------------------------
; CPU idle mode off
DebugStopIdle	macro
		DebugCmd DEBUG_CMD_SET_IDLE,#0,#0,#0
		endm


********************************************************************************
* Debug overlay:
********************************************************************************

;-------------------------------------------------------------------------------
; Clear all overlay graphics
DebugClear	macro
		DebugCmd DEBUG_CMD_CLEAR,#0,#0,#0
		endm

;-------------------------------------------------------------------------------
; Draw stroked rectangle with fixed args
; \1 = x1
; \2 = y1
; \3 = x2
; \4 = y2
; \5 = color
DebugRect	macro
		DebugCmd DEBUG_CMD_RECT,#(\1<<16)!\2,#(\3<<16)!\4,#\5
		endm

;-------------------------------------------------------------------------------
; Draw stroked rectangle with dynamic args
; \1 = x1/y1 (upper/lower word)
; \2 = x2/y2 (upper/lower word)
; \3 = color
DebugRectDyn	macro
		DebugCmd DEBUG_CMD_RECT,\1,\2,\3
		endm

;-------------------------------------------------------------------------------
; Draw filled rectangle with fixed args
; \1 = x1
; \2 = y1
; \3 = x2
; \4 = y2
; \5 = color
DebugFilledRect	macro
		DebugCmd DEBUG_CMD_FILLED_RECT,#(\1<<16)!\2,#(\3<<16)!\4,#\5
		endm

;-------------------------------------------------------------------------------
; Draw filled rectangle with dynamic args
; \1 = x1/y1 (upper/lower word)
; \2 = x2/y2 (upper/lower word)
; \3 = color
DebugFilledRectDyn macro
		DebugCmd DEBUG_CMD_FILLED_RECT,\1,\2,\3
		endm

;-------------------------------------------------------------------------------
; Draw text overlay with fixed args
; \1 = x
; \2 = y
; \3 = string data address
; \4 = color
DebugText	macro
		DebugCmd DEBUG_CMD_TEXT,#(\1<<16)!\2,#\3,#\4
		endm

;-------------------------------------------------------------------------------
; Draw text overlay with dynamic args
; \1 = x/y (upper/lower word)
; \2 = string data address
; \3 = color
DebugTextDyn	macro
		DebugCmd DEBUG_CMD_TEXT,\1,#\2,\3
		endm


********************************************************************************
* Graphics resources:
********************************************************************************

; Resource flags:

DEBUG_RESOURCE_BITMAP_INTERLEAVED = 1<<0
DEBUG_RESOURCE_BITMAP_MASKED = 1<<1
DEBUG_RESOURCE_BITMAP_HAM = 1<<2

;-------------------------------------------------------------------------------
; Register graphic resource
; \1 = struct address
DebugRegisterResource macro
		DebugCmd DEBUG_CMD_REGISTER_RESOURCE,#\1,#0,#0
		endm

;-------------------------------------------------------------------------------
; Unregister graphic resource
; \1 = resource address
DebugUnregisterResource macro
		DebugCmd DEBUG_CMD_UNREGISTER_RESOURCE,#\1,#0,#0
		endm

;-------------------------------------------------------------------------------
; Load graphic resource
; \1 = struct address
DebugLoadResource macro
		DebugCmd DEBUG_CMD_LOAD,#\1,#0,#0
		endm

;-------------------------------------------------------------------------------
; Save graphic resource
; \1 = struct address
DebugSaveResource macro
		DebugCmd DEBUG_CMD_SAVE,#\1,#0,#0
		endm

; Resource structs:

;-------------------------------------------------------------------------------
; Create bitmap resource struct
; \1 = address
; \2 = name
; \3 = width px
; \4 = height px
; \5 = bitplane count
; \6 = flags (optional)
DebugResourceBitmap macro
		ifne	DEBUG
		DebugResource \1,\3/8*\4*\5,\2,DEBUG_RESOURCE_TYPE_BITMAP,\6
		dc.w	\3					; Width
		dc.w	\4					; Height
		dc.w	\5					; Bitplanes
		endc
		endm

;-------------------------------------------------------------------------------
; Create palette resource struct
; \1 = address
; \2 = name
; \3 = entries count
; \4 = flags (optional)
DebugResourcePalette macro
		ifne	DEBUG
		DebugResource \1,\3*2,\2,DEBUG_RESOURCE_TYPE_PALETTE,\4
		dc.w	\3					; Entries
		endc
		endm

;-------------------------------------------------------------------------------
; Create copperlist resource struct
; \1 = address
; \2 = name
; \3 = size (bytes)
; \4 = flags (optional)
DebugResourceCopperlist macro
		DebugResource \1,\3,\2,DEBUG_RESOURCE_TYPE_COPPERLIST,\4
		endm


********************************************************************************
; Internals
********************************************************************************

; Commands:

DEBUG_CMD_CLEAR = 0
DEBUG_CMD_RECT = 1
DEBUG_CMD_FILLED_RECT = 2
DEBUG_CMD_TEXT = 3
DEBUG_CMD_REGISTER_RESOURCE = 4
DEBUG_CMD_SET_IDLE = 5
DEBUG_CMD_UNREGISTER_RESOURCE = 6
DEBUG_CMD_LOAD = 7
DEBUG_CMD_SAVE = 8

; Resource types:

DEBUG_RESOURCE_TYPE_BITMAP = 0
DEBUG_RESOURCE_TYPE_PALETTE = 1
DEBUG_RESOURCE_TYPE_COPPERLIST = 2

;-------------------------------------------------------------------------------
; Call debug command
; \1 = command
; \2 = arg1
; \3 = arg2
; \4 = arg3
DebugCmd	macro
		ifne	DEBUG
		move.l	d0,-(sp) ; d0 gets trashed
		move.l	\4,-(sp)
		move.l	\3,-(sp)
		move.l	\2,-(sp)
		pea	\1
		pea	88
		jsr	$f0ff60
		lea	20(sp),sp
		move.l	(sp)+,d0
		endc
		endm

;-------------------------------------------------------------------------------
; Resource struct common fields
; \1 = address
; \2 = size (bytes)
; \3 = name
; \4 = resource type
; \5 = flags (optional)
DebugResource	macro
		ifne	DEBUG
		dc.l	\1					; Address
		dc.l	\2					; Size
		dc.b	\3					; Name
		ds.b	32+2-\?3				; pad string to 32 chars (length includes quotes)
		dc.w	\4					; Type
		ifnb	\5
		dc.w	\5					; Flags
		else
		dc.w	0
		endc
		endc
		endm

		endc
