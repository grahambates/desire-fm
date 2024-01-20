WORD_POS	macro
		dc.w	\1,\2
		endm
LETTER		macro
		dc.w	\1
		dc.b	\2
		dc.b	\3
		endm
WORD_END	macro
		dc.w	-1
		endm

Greets:
MoodsPlateau:
		WORD_POS 200,100
		LETTER	0,0,"M"
		LETTER	16,9,"O"
		LETTER	27,10,"O"
		LETTER	38,4,"D"
		LETTER	56,5,"S"
		LETTER	82,3,"p"
		LETTER	96,13,"L"
		LETTER	106,0,"a"
		LETTER	118,5,"t"
		LETTER	131,6,"E"
		LETTER	146,0,"a"
		LETTER	161,2,"u"
		WORD_END
FocusDesign:
		WORD_POS 380,200
		LETTER	0,0,"f"
		LETTER	18,7,"O"
		LETTER	33,2,"C"
		LETTER	44,7,"u"
		LETTER	55,2,"s"
		LETTER	83,1,"D"
		LETTER	101,7,"E"
		LETTER	113,3,"S"
		LETTER	127,2,"i"
		LETTER	133,2,"G"
		LETTER	153,0,"n"
		WORD_END
Rift:
		WORD_POS 530,10
		LETTER	0,8,"R"
		LETTER	20,11,"I"
		LETTER	30,8,"f"
		LETTER	44,1,"T"
		WORD_END
Resistance:
		WORD_POS 650,120
		LETTER	0,7,"R"
		LETTER	22,9,"E"
		LETTER	33,4,"s"
		LETTER	48,2,"i"
		LETTER	56,5,"S"
		LETTER	59,0,"T"
		LETTER	76,4,"A"
		LETTER	95,3,"N"
		LETTER	109,8,"C"
		LETTER	119,6,"e"
		WORD_END
Abyss:
		WORD_POS 850,40
		LETTER	0,14,"A"
		LETTER	14,12,"B"
		LETTER	39,1,"Y"
		LETTER	56,11,"S"
		LETTER	71,7,"s"
		WORD_END

		dc.w	-1
Greets2:

NahKolor:
		WORD_POS 240,140
		LETTER	0,2,"N"
		LETTER	11,4,"a"
		LETTER	20,3,"H"
		LETTER	43,7,"K"
		LETTER	60,9,"O"
		LETTER	75,12,"L"
		LETTER	85,6,"O"
		LETTER	91,0,"R"
		WORD_END
Melon:
		WORD_POS 180,40
		LETTER	0,0,"m"
		LETTER	22,4,"E"
		LETTER	38,9,"L"
		LETTER	46,7,"O"
		LETTER	60,0,"n"
		WORD_END
SlipStream:
		WORD_POS 300,95
		LETTER	0,3,"s"
		LETTER	19,18,"L"
		LETTER	27,7,"i"
		LETTER	32,9,"P"
		LETTER	52,8,"S"
		LETTER	55,0,"T"
		LETTER	71,6,"R"
		LETTER	96,10,"E"
		LETTER	109,3,"a"
		LETTER	122,2,"m"
		WORD_END
Insane:
		WORD_POS 490,40
		LETTER	0,3,"i"
		LETTER	10,0,"N"
		LETTER	22,0,"S"
		LETTER	38,0,"a"
		LETTER	50,0,"N"
		LETTER	58,3,"e"
		WORD_END
Smfx:
		WORD_POS 610,140
		LETTER	0,3,"s"
		LETTER	19,0,"M"
		LETTER	34,6,"F"
		LETTER	49,5,"X"
		WORD_END
Dhs:
		WORD_POS 650,0
		LETTER	0,6,"d"
		LETTER	17,3,"h"
		LETTER	32,0,"s"
		WORD_END
Tek:
		WORD_POS 700,100
		LETTER	0,6,"T"
		LETTER	19,10,"E"
		LETTER	32,4,"k"
		WORD_END

		dc.w	-1
Greets3:
Fatzone:
		WORD_POS 280,55
		LETTER	0,7,"F"
		LETTER	17,1,"a"
		LETTER	20,0,"T"
		LETTER	40,5,"Z"
		LETTER	55,8,"O"
		LETTER	70,2,"N"
		LETTER	82,7,"E"
		WORD_END
Tbl:
		WORD_POS 280,0
		LETTER	0,4,"t"
		LETTER	13,1,"b"
		LETTER	32,0,"l"
		WORD_END
Proxima:
		WORD_POS 370,160
		LETTER	0,9,"p"
		LETTER	9,4,"r"
		LETTER	39,7,"O"
		LETTER	50,8,"X"
		LETTER	64,7,"i"
		LETTER	73,1,"m"
		LETTER	96,0,"a"
		WORD_END
Darkage:
		WORD_POS 460,000
		LETTER	0,6,"d"
		LETTER	17,1,"a"
		LETTER	22,4,"R"
		LETTER	44,3,"K"
		LETTER	60,2,"A"
		LETTER	75,0,"G"
		LETTER	90,2,"e"
		WORD_END
Talent:
		WORD_POS 530,90
		LETTER	0,8,"t"
		LETTER	14,0,"a"
		LETTER	25,3,"l"
		LETTER	38,5,"E"
		LETTER	52,1,"N"
		LETTER	57,0,"T"
		WORD_END
Lemon:
		WORD_POS 500,220
		LETTER	0,13,"L"
		LETTER	10,7,"E"
		LETTER	26,0,"m"
		LETTER	46,5,"O"
		LETTER	59,0,"N"
		LETTER	71,14,"["
		WORD_END
Ffp:
		WORD_POS 600,180
		LETTER	0,15,"F"
		LETTER	15,13,"f"
		LETTER	28,11,"P"
		WORD_END

		dc.w	-1

; Draw directions
DIR_L = 1<<0
DIR_R = 1<<1
DIR_U = 1<<2
DIR_D = 1<<3

; Strokes
		rsreset
V1		rs.b	1		; word width so we can shift to get total offset
V2		rs.b	1
V3		rs.b	1
V4		rs.b	1
V5		rs.b	1
V6		rs.b	1
V7		rs.b	1
V8		rs.b	1
V9		rs.b	1
H1		rs.b	1
H2		rs.b	1
H3		rs.b	1
H4		rs.b	1
H5		rs.b	1
H6		rs.b	1
C1		rs.b	1
C2		rs.b	1
C3		rs.b	1
C4		rs.b	1
C5		rs.b	1
C6		rs.b	1
D1		rs.b	1
D2		rs.b	1
D3		rs.b	1
D4		rs.b	1
S1		rs.b	1


CharOffsets:
		dc.w	A-CharData
		dc.w	B-CharData
		dc.w	C-CharData
		dc.w	D-CharData
		dc.w	E-CharData
		dc.w	F-CharData
		dc.w	G-CharData
		dc.w	H-CharData
		dc.w	I-CharData
		; dc.w	J-CharData
		dc.w	0
		dc.w	K-CharData
		dc.w	L-CharData
		dc.w	M-CharData
		dc.w	N-CharData
		dc.w	O-CharData
		dc.w	P-CharData
		dc.w	0
		dc.w	R-CharData
		dc.w	S-CharData
		dc.w	T-CharData
		; dc.w	U-CharData
		dc.w	0
		; dc.w	V-CharData
		dc.w	0
		; dc.w	W-CharData
		dc.w	0
		dc.w	X-CharData
		dc.w	Y-CharData
		dc.w	Z-CharData
		; symbols mapped to punctuation
		dc.w	Point-CharData	; [
		dc.w	0		; \
		dc.w	0		; ]
		dc.w	0		; ^
		dc.w	0		; _
		dc.w	0		; `
		; alternates mapped to lower case
		dc.w	a-CharData
		dc.w	b-CharData
		dc.w	0
		dc.w	d-CharData
		dc.w	e-CharData
		dc.w	f-CharData
		; dc.w	g-CharData
		dc.w	0
		dc.w	h-CharData
		dc.w	i-CharData
		; dc.w	j-CharData
		dc.w	0
		dc.w	k-CharData
		dc.w	l-CharData
		dc.w	m-CharData
		dc.w	n-CharData
		dc.w	0
		dc.w	p-CharData
		dc.w	0
		dc.w	r-CharData
		dc.w	s-CharData
		dc.w	t-CharData
		dc.w	u-CharData
		; dc.w	v-CharData
		dc.w	0
		dc.w	0
		dc.w	0
		; dc.w	y-CharData
		dc.w	0
		dc.w	0

CharData:
A:
		dc.b	V8,6,10		;DIR_D
		dc.b	V6,9,1		;DIR_U
		dc.b	V5,12,0		;DIR_D
		dc.b	H5,0,10		;DIR_R
		dc.b	-1

B:
		dc.b	C4,0,0		;DIR_D
		dc.b	C3,10,5		;DIR_D!DIR_R
		dc.b	V4,6,5		;DIR_D
		dc.b	-1

C:
		dc.b	D1,0,0		;DIR_U
		dc.b	C2,0,2		;DIR_R
		dc.b	-1

D:
		dc.b	H4,0,0		;DIR_R
		dc.b	D3,4,3		;DIR_U!DIR_R
		dc.b	V4,4,5		;DIR_D
		dc.b	-1

E:
		dc.b	V7,1,4		;DIR_D
		dc.b	H1,2,10		;DIR_R
		dc.b	H3,0,0		;DIR_R
		dc.b	H3,0,6		;DIR_R
		dc.b	-1

F:
		dc.b	H4,0,0		;DIR_L
		dc.b	V4,2,4		;DIR_D
		dc.b	H3,5,7		;DIR_R
		dc.b	-1

G:
		dc.b	D1,7,0		;DIR_L!DIR_U
		dc.b	C2,7,6		;DIR_R
		dc.b	H5,0,10		;DIR_R
		dc.b	-1

H:
		dc.b	V4,1,6		;DIR_D
		dc.b	V5,10,0		;DIR_D
		dc.b	H3,3,11		;DIR_R
		dc.b	-1

I:
		dc.b	V5,6,0		;DIR_D
		dc.b	H3,0,0		;DIR_R
		dc.b	H3,6,13		;DIR_R
		dc.b	-1

; J:
; 		dc.b	V4,6,0;DIR_D
; 		dc.b	C2,0,9;DIR_L
; 		dc.b	H1,2,2;DIR_R
; 		dc.b	-1

K:
		dc.b	D1,0,8		;DIR_R
		dc.b	C6,3,2		;DIR_D
		dc.b	D2,0,0		;DIR_R
		dc.b	V9,16,0		;DIR_D
		dc.b	-1

L:
		dc.b	V1,0,0		;DIR_D
		dc.b	H3,0,6		;DIR_R
		dc.b	-1

M:
		dc.b	V7,0,14		;DIR_U
		dc.b	D1,0,13		;DIR_R!DIR_D
		dc.b	V2,7,3		;DIR_U
		dc.b	V3,10,0		;DIR_D
		dc.b	-1

N:
		dc.b	V1,0,11		;DIR_U
		dc.b	D1,1,10		;DIR_D!DIR_R
		dc.b	V2,8,0		;DIR_U
		dc.b	-1

O:
		dc.b	C1,0,0		;DIR_R
		dc.b	-1

P:
		dc.b	C4,0,0		;DIR_D
		dc.b	V4,8,6		;DIR_D
		dc.b	-1

R:
		dc.b	C4,0,0		;DIR_D
		dc.b	V4,8,6		;DIR_D
		dc.b	D4,13,11	;DIR_D!DIR_R
		dc.b	-1

S:
		dc.b	D1,0,0		;DIR_U
		dc.b	V1,2,5		;DIR_D
		dc.b	C3,1,7		;DIR_D!DIR_R
		dc.b	-1

T:
		dc.b	V9,15,3		;DIR_D
		dc.b	H2,0,0		;DIR_R
		dc.b	-1

; U:
; 		dc.b	C6,0,0;DIR_D
; 		dc.b	H1,0,9;DIR_R
; 		dc.b	D1,9,0;DIR_U
; 		dc.b	-1

; V:
; 		dc.b	D1,0,10;DIR_D
; 		dc.b	V2,7,0;DIR_U
; 		dc.b	-1

; W:
; 		dc.b	V8,0,12;DIR_D
; 		dc.b	V1,5,11;DIR_U
; 		dc.b	D1,6,10;DIR_D
; 		dc.b	V2,13,0;DIR_U
; 		dc.b	-1

X:
		dc.b	V4,4,3		;DIR_D
		dc.b	D2,0,0		;DIR_U!DIR_R
		dc.b	-1

Y:
		dc.b	C6,0,10		;DIR_D
		dc.b	H1,1,18		;DIR_R
		dc.b	V2,14,0		;DIR_U
		dc.b	V9,9,10		;DIR_D
		dc.b	-1

Z:
		dc.b	H1,2,0		;DIR_R
		dc.b	D2,0,0		;DIR_D!DIR_L
		dc.b	H4,0,13		;DIR_R
		dc.b	H3,2,7		;DIR_R
		dc.b	-1

; Alternates

a:
		dc.b	V2,0,0		;DIR_U
		dc.b	V5,4,5		;DIR_D
		dc.b	H3,3,12		;DIR_R
		dc.b	-1
b:
		dc.b	C3,0,0		;DIR_R
		dc.b	C3,3,7		;DIR_R
		dc.b	V6,4,4		;DIR_D
		dc.b	-1
d:
		dc.b	H4,0,0		;DIR_R
		dc.b	C5,2,2		;DIR_L
		dc.b	V4,2,2		;DIR_D
		dc.b	-1
e:
		dc.b	H3,6,0		;DIR_R
		dc.b	H1,6,7		;DIR_R
		dc.b	H2,0,15		;DIR_R
		dc.b	V2,5,0		;DIR_D
		dc.b	-1
f:
		dc.b	V5,5,2		;DIR_D
		dc.b	H6,0,0		;DIR_R
		dc.b	H1,5,8		;DIR_R
		dc.b	-1
; g:
; 		dc.b	H4,0,0;DIR_L
; 		dc.b	C2,3,4;DIR_R
; 		dc.b	H3,8,7;DIR_L
; 		dc.b	-1
h:
		dc.b	V9,2,0		;DIR_D
		dc.b	V6,12,4		;DIR_D
		dc.b	H1,0,13		;DIR_R
		dc.b	-1
i:
		dc.b	V5,0,0		;DIR_D
		dc.b	-1
; j:
; 		dc.b	C5,0,2;DIR_R
; 		dc.b	H1,5,0;DIR_L
; 		dc.b	-1
k:
		dc.b	D1,0,6		;DIR_D
		dc.b	C6,3,0		;DIR_D
		dc.b	D4,6,7		;DIR_R
		dc.b	-1
l:
		dc.b	V2,0,0		;DIR_D
		dc.b	H2,0,14		;DIR_R
		dc.b	-1
m:
		dc.b	V9,0,4		;DIR_U
		dc.b	D1,0,3		;DIR_D
		dc.b	C6,8,0		;DIR_U
		dc.b	V4,13,0		;DIR_D
		dc.b	-1
n:
		dc.b	V5,0,4		;DIR_U
		dc.b	D4,2,3		;DIR_D
		dc.b	V4,10,0		;DIR_U
		dc.b	-1
p:
		dc.b	V4,0,8		;DIR_U
		dc.b	C3,0,0		;DIR_R
		dc.b	-1
r:
		dc.b	C4,0,2		;DIR_D!DIR_R
		dc.b	D2,11,2		;DIR_R
		dc.b	V9,26,2		;DIR_D
		dc.b	V4,6,7		;DIR_D
		dc.b	-1
s:
		dc.b	D1,3,0		;DIR_U
		dc.b	C6,1,4		;DIR_D
		dc.b	H5,0,10		;DIR_R
		dc.b	C5,2,10		;DIR_L
		dc.b	-1
t:
		dc.b	V6,6,2		;DIR_D
		dc.b	H1,0,0		;DIR_R
		dc.b	-1
u:
		dc.b	C2,0,0		;DIR_R
		dc.b	-1
; v:
; 		dc.b	D1,0,3;DIR_R
; 		dc.b	C6,8,0;DIR_U
; 		dc.b	-1
; y:
; 		dc.b	D1,0,3;DIR_R
; 		dc.b	C6,8,0;DIR_U
; 		dc.b	V7,6,10;DIR_D
; 		dc.b	-1

; Symbols

Point:
		dc.b	S1,0,0		;DIR_R
		dc.w	-1
