		ifnd	_MEMORY_I
_MEMORY_I = 1

		xref	MemAllocChip
		xref	MemAllocChipAligned
		xref	MemAllocPublic
		xref	MemFreeLast
		xref	MemFlip

;-------------------------------------------------------------------------------
; Macro helpers with logging

LOG_ALLOCS = 0

_CHIP		set	0
_PUBLIC		set	0

		macro	ALLOC_CHIP
		move.l	#\1,d0
		jsr	MemAllocChip
_CHIP		set	_CHIP+\1
		ifne	LOG_ALLOCS
		printt	"Alloc chip: \1 \2"
		printv	\1
		endc
		ifnc	"\2",""
		move.l	a0,\2
		endc
		endm

		macro	ALLOC_CHIP_ALIGNED
		move.l	#\1,d0
		jsr	MemAllocChipAligned
_CHIP		set	_CHIP+\1
		ifne	LOG_ALLOCS
		printt	"Alloc chip: \1 \2"
		printv	\1
		endc
		ifnc	"\2",""
		move.l	a0,\2
		endc
		endm

		macro	ALLOC_PUBLIC
		move.l	#\1,d0
		jsr	MemAllocPublic
_PUBLIC		set	_PUBLIC+\1
		ifne	LOG_ALLOCS
		printt	"Alloc public: \1 \2"
		printv	\1
		endc
		ifnc	"\2",""
		move.l	a0,\2
		endc
		endm

		macro	PRINT_MEM_TOTALS
		printt	"Chip:"
		printv	_CHIP
		printt	"Public:"
		printv	_PUBLIC
		endm

		endc
