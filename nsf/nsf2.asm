;;; cl65 -d -vm -l nsf.lst -g -t nes -C nsf.cfg -m nsf.map -Ln nsf.lbl -o driar.nsf nsf.asm

.define loop_pattern_num 0
.define porta_once 0

.define chnum 5

.segment "INESHDR"
	.macro pad32 str
	 .if (.strlen(str) > 31)
	  .error "pad32 given too long input"
	 .endif
	 .byte str,0
	 .res 31-.strlen(str)
	.endmacro
	
	.byte "NESM",$1a
	.byte 2     ; version
	.byte 1    ; number of songs
	.byte 1     ; starting song
	.word $e000 ;;; This is where I was wrong. loadaddr should be the start of the ROM0 segment
	.word $e000
	.word nmi_handler
	;; ....0123456789a123456789b123456789c1
	pad32 "furNES Test"
	pad32 "AArt1256"
	pad32 "2024 AAr1t256"
	.word 16639 ; Real NTSC rate
	.byte 0,0,0,0,1,125,126,127 ; disable bankswitching
	.word 19997	; Real PAL rate
	.byte 0	; Prefer PAL, compatible with both
	.byte 0	; no expansion audio
	.byte 16|32,0,0,0

;tick_speed = 3

.ZEROPAGE
.org $10
init_count: .res 2
patzp: .res 3
macroIns: .res 2
temp_index: .res 2
nmis: .res 1
;.org $20
NUM1: .res 2
NUM2: .res 2
RESULT: .res 4
;.org $30
periods: .res 4
;.org $40
tfx_mode: .res 2
tfx_off: .res 2
tfx_arp: .res 2
tfx_bound: .res 2
;.org $50
tfx_lo_pitch: .res 2
tfx_hi_pitch: .res 2
tfx_lo_phase: .res 2
tfx_hi_phase: .res 2
vol_or: .res 2
vol_pulse: .res 2

.segment "BSS"
.org $200
OAM: .res 256
.org $300
patbank: .res chnum
patseq: .res chnum*2
dur: .res chnum
duty: .res chnum
nextpat: .res 1
patind: .res 1
ch: .res 1
vol: .res chnum
volm: .res chnum
volout: .res chnum
tick: .res 1
temp: .res 4
ins: .res chnum
mframeA: .res chnum
mframeV: .res chnum
mframeD: .res chnum
doMacroA: .res chnum
doMacroV: .res chnum
doMacroD: .res chnum
timerH: .res chnum
arp: .res chnum
absarp: .res chnum
isoff: .res chnum
effects_temp: .res 2
tick_speeds: .res 2
tick_sel: .res 1
slide_amt: .res chnum
slide_amt_sign: .res chnum
slide_buffer_lo: .res chnum
slide_buffer_hi: .res chnum
note_pitch_lo: .res chnum
note_pitch_hi: .res chnum
note_n: .res chnum
note_dest: .res chnum
finepitch: .res chnum
vibrato_param: .res chnum
vibrato_phase: .res chnum
cut_dur: .res chnum
new_dpcm_note: .res 1
volume_add: .res chnum
vol_tick: .res chnum
change_duty: .res chnum
dpcm_start_delta: .res 1
note_delay: .res chnum
delay_do: .res 1
note_play: .res chnum
note_dur: .res chnum
.if porta_once = 1
didporta: .res chnum
.endif
arpeff1: .res chnum
arpeff2: .res chnum
arpind: .res chnum

PPUCTRL = $2000
NT_2000 = $00
NT_2400 = $01
NT_2800 = $02
NT_2C00 = $03
VRAM_DOWN = $04
OBJ_0000 = $00
OBJ_1000 = $08
OBJ_8X16 = $20
BG_0000 = $00
BG_1000 = $10
VBLANK_NMI = $80

PPUMASK = $2001
LIGHTGRAY = $01
BG_OFF = $00
BG_CLIP = $08
BG_ON = $0A
OBJ_OFF = $00
OBJ_CLIP = $10
OBJ_ON = $14
TINT_R = $20
TINT_G = $40
TINT_B = $80

PPUSTATUS = $2002
OAMADDR = $2003
; Don't worry about $2004; let OAM_DMA do the work for you.
PPUSCROLL = $2005
PPUADDR = $2006
PPUDATA = $2007

OAM_DMA = $4014
SNDCHN = $4015
P1 = $4016
P2 = $4017

KEY_A      = %10000000
KEY_B      = %01000000
KEY_SELECT = %00100000
KEY_START  = %00010000
KEY_UP     = %00001000
KEY_DOWN   = %00000100
KEY_LEFT   = %00000010
KEY_RIGHT  = %00000001

.segment "CODE"
.org $e000
  inc init_count
  lda init_count
  cmp #2
  bcs :+
  rts
:

	sei
	lda #0
	sta $401D
	; setup IRQ pointer
	lda #<irq_handler
	sta $FFFE
	lda #>irq_handler
	sta $FFFF
	; set starting frequency
	lda #$100-$26 ;<(1789773/8400)
	sta $401B
	lda #0 ;>(1789773/8400)
	sta $401C
	; begin IRQ
	lda #1
	sta $401D
	cli
 
  jsr initaddr
loop:
  lda #0
  sta nmis
:
  cmp nmis
  beq :-
  jsr playaddr
  jmp loop

.proc nmi_handler
    pha
    lda #$ff
    sta nmis
    pla
    rts
.endproc

;.res 256-(*&$ff), 0
clamp:
.repeat 16, I
    .byte I
.endrepeat
.res 128-16, 15
.res 128, 0

.proc irq_handler
    sta $fd
    stx $fe

	lda $401D ; acknowledge IRQ

.repeat 2, I
  lda tfx_mode+I
  beq :++
  lda tfx_lo_phase+I
  clc
  adc tfx_lo_pitch+I
  sta tfx_lo_phase+I
  lda tfx_hi_phase+I
  adc tfx_hi_pitch+I
  sta tfx_hi_phase+I
  and #$40
  beq :+
  lda vol_pulse+I
  ora vol_or+I
  sta $4000+I*4
  jmp :++
:
  lda tfx_bound+I
  sec
  sbc #15
  clc 
  adc vol_pulse+I
  tax
  lda clamp, x
  ora vol_or+I
  sta $4000+I*4
:
.endrepeat

    lda $fd
    ldx $fe
    rti
.endproc

.macro incw address
  .local skipW
  inc address
  bne skipW
  inc address+1
skipW:
.endmacro

.proc initaddr
  lda #%00001111
  sta $4015
  lda #8
  sta $4001
  lda #8
  sta $4005

  lda #0
  sta tick_sel
  sta new_dpcm_note
  sta dpcm_start_delta

  lda ticks_init
  sta tick_speeds
  lda ticks_init+1
  sta tick_speeds+1

  lda #0
  sta patind
  sta delay_do

  lda #0
  sta tfx_mode
  sta tfx_mode+1
  sta tfx_off
  sta tfx_off+1
  sta tfx_bound
  sta tfx_bound+1
  sta tfx_lo_pitch
  sta tfx_lo_pitch+1
  sta tfx_hi_pitch
  sta tfx_hi_pitch+1
  sta tfx_lo_phase
  sta tfx_lo_phase+1
  sta tfx_hi_phase
  sta tfx_hi_phase+1
  lda #16
  sta tfx_arp
  sta tfx_arp+1

  ldx #chnum-1
:
  lda #$80
  sta finepitch, x
  lda #1
  sta dur, x
  lda #$ff
  sta cut_dur, x
  sta note_delay, x
  lda #0
.if porta_once = 1
  sta didporta, x
.endif
  sta arpind, x
  sta arpeff1, x
  sta arpeff2, x
  sta ins, x
  sta note_play, x
  sta change_duty, x
  sta note_dur, x
  sta arp, x
  sta volume_add, x
  sta slide_amt, x
  sta slide_amt_sign, x
  sta slide_buffer_lo, x
  sta slide_buffer_hi, x
  sta vibrato_phase, x
  sta note_dest, x
  sta mframeV, x
  sta mframeA, x
  sta mframeD, x
  sta doMacroA, x
  sta doMacroV, x
  sta doMacroD, x
  sta volout, x
  sta timerH, x
  lda #$ff
  sta isoff, x
  sta vol_tick, x
  lda #15
  sta vol, x
  sta volm, x
  dex
  bpl :-

  ldx #0
  jsr set_patseq_init
  lda #0
  sta tick
  rts
.endproc

.macro get_patzp
  .local skipW
  inc patzp
  bne skipW
	inc patzp+1
	lda patzp+1
	cmp #$a0
	bne skipW
	lda #$80
	sta patzp+1
	inc patzp+2
skipW:
	lda patzp+2
  asl
  sta $5ff8
  ora #1
  sta $5ff9
  lda (patzp), y
.endmacro

setduty:
  ldx ch
  get_patzp
  sta duty, x
  lda #$ff
  sta change_duty, x
  rts

.macro add_00xx
  .local skip
  cmp #$00
  bne skip
  ldx ch
  lda #0
  sta arpind, x
  lda effects_temp+1
  and #$0f
  sta arpeff2, x
  lda effects_temp+1
  lsr
  lsr
  lsr
  lsr
  sta arpeff1, x
  rts
skip:
.endmacro

.macro add_09xx
  .local skip
  cmp #$09
  bne skip
  lda effects_temp+1
  sta tick_speeds
  rts
skip:
.endmacro


.macro add_0Fxx
  .local skip
  cmp #$0F
  bne skip
  lda effects_temp+1
  sta tick_speeds+1
  rts
skip:
.endmacro

.macro add_01xx
  .local skip
  cmp #$01
  bne skip
  ldx ch
.if porta_once = 1
  lda #$00
  sta didporta, x
.endif
  lda effects_temp+1
  sta slide_amt, x
  lda #$ff
  sta slide_amt_sign, x
  ldy #0
  get_patzp
  ldx ch
  sta note_dest, x
  rts
skip:
.endmacro

.macro add_02xx
  .local skip
  cmp #$02
  bne skip
  ldx ch
.if porta_once = 1
  lda #$00
  sta didporta, x
.endif
  lda effects_temp+1
  sta slide_amt, x
  lda #$00
  sta slide_amt_sign, x
  ldy #0
  get_patzp
  ldx ch
  sta note_dest, x
  rts
skip:
.endmacro

.macro add_03xx
  .local skip
  cmp #$03
  bne skip
  ldx ch
  lda effects_temp+1
  sta slide_amt, x
  ldy #0
  get_patzp
  ldx ch
  sta note_dest, x
.if porta_once = 1
  lda #$ff
  sta didporta, x
.endif

  lda note_n, x
  cmp note_dest, x
  bne :+
  lda #0
  sta slide_amt, x
  sta slide_amt_sign, x
  rts
:
  bcc :+
  lda #0
  sta slide_amt_sign, x
  rts
:
  lda #$ff
  sta slide_amt_sign, x
  rts
skip:
.endmacro

.macro add_04xx
  .local skip, retskip
  cmp #$04
  bne skip
  ldx ch
  lda effects_temp+1
  sta vibrato_param, x
  cmp #0
  beq retskip
  sta vibrato_phase, x
retskip:
  rts
skip:
.endmacro

.macro add_11xx
  .local skip, retskip
  cmp #$11
  bne skip
  lda effects_temp+1
  sta $4011
  sta dpcm_start_delta
skip:
.endmacro


.macro add_E1xx
  .local skip
  cmp #$E1
  bne skip
  ldx ch
.if porta_once = 1
  lda #$ff
  sta didporta, x
.endif
  lda effects_temp+1
  asl
  asl
  sta slide_amt, x
  lda #$ff
  sta slide_amt_sign, x
  ldy #0
  get_patzp
  ldx ch
  ora #$80
  sta note_dest, x
  rts
skip:
.endmacro

.macro add_E2xx
  .local skip
  cmp #$E2
  bne skip
  ldx ch
.if porta_once = 1
  lda #$ff
  sta didporta, x
.endif
  lda effects_temp+1
  asl
  asl
  sta slide_amt, x
  lda #$00
  sta slide_amt_sign, x
  ldy #0
  get_patzp
  ldx ch
  ora #$80
  sta note_dest, x
  rts
skip:
.endmacro


.macro add_E5xx
  .local skip
  cmp #$E5
  bne skip
  ldx ch
  lda effects_temp+1
  sta finepitch, x
  rts
skip:
.endmacro

.macro add_ECxx
  .local skip
  cmp #$EC
  bne skip
  ldx ch
  lda effects_temp+1
  sta cut_dur, x
  rts
skip:
.endmacro


.macro add_0Axx
  .local skip, skip2
  cmp #$0A
  bne skip
  ldx ch
  lda effects_temp+1
  sta volume_add, x
  cmp #0
  bne skip2
  lda vol_tick, x
  ora #$80
  sta vol_tick, x
  rts
skip2:
  lda vol_tick, x
  and #3
  sta vol_tick, x
  rts
skip:
.endmacro


other_effects:
  lda effects_temp
  add_00xx
  add_09xx
  add_0Fxx
  add_01xx
  add_02xx
  add_03xx
  add_04xx
  add_0Axx
  add_11xx
  add_E1xx
  add_E2xx
  add_E5xx
  add_ECxx
  rts

.macro add_advance_routine
advance:
  .local skipD, noIns, noVol, end, beg, blank, blank2, blank3, begnote
  .local skip_delay, skip_delay2
beg:
  lda ch
  asl
  tax
  lda patseq, x
  sta patzp
  lda patseq+1, x
  sta patzp+1
  ldx ch
  lda patbank, x
  sta patzp+2

  dec dur, x
  lda dur, x
  cmp #0
  beq begnote
  jmp end
begnote:

  ldy #0
  get_patzp
  sta temp
  cmp #$ff
  bne :+
  jmp blank2
:
  cmp #$fe
  bne :+
  jsr setduty
  jmp begnote
:
  cmp #$fd
  bne :+
  get_patzp
  sta effects_temp
  get_patzp
  sta effects_temp+1
  jsr other_effects

  ; add EDxx effect
  cmp #$ED
  bne skip_delay
  ldx ch
  lda delay_do
  beq skip_delay2
  jmp begnote
skip_delay2:
  lda effects_temp+1
  sta note_delay, x
  lda #1
  sta dur, x
  rts
skip_delay:
  jmp begnote
:
  get_patzp
  sta temp+1
  cmp #$82
  bne :+
  lda ins, x
  tay
  lda insVrel, y
  sta mframeV, x
  lda insArel, y
  sta mframeA, x
  lda insDrel, y
  sta mframeD, x
  ldy #0
  jmp blank2
:
  cmp #$80
  beq blank2
  cmp #$81
  bne :+
  lda #$ff
  ldx ch
  sta isoff, x
  jmp blank2
:
  lda temp
  cmp #$ff
  beq blank3
  lda #0
  ldx ch
  sta isoff, x
  jmp blank3
blank3:
  lda temp+1
  ldx ch
  sta note_n, x
  lda #0
  sta mframeV, x
  sta mframeA, x
  sta mframeD, x
  sta slide_buffer_lo, x
  sta slide_buffer_hi, x
.if porta_once = 1
  cmp didporta, x
  beq :+
  sta didporta, x
  sta slide_amt, x
  sta slide_amt_sign, x
:
.endif
  lda #$ff
  sta doMacroA, x
  sta doMacroV, x
  sta doMacroD, x
  lda note_dur, x
  cmp #1
  bcc :+
  lda #$ff
  sta note_play, x
  lda #0
  sta note_dur, x
:
  cpx #4
  bne blank2
  lda #$ff
  sta new_dpcm_note
blank2:
  lda temp
  cmp #$ff
  bne skipD

  lda #$ff
  sta nextpat

  jmp end
skipD:
  lda temp
  and #1
  cmp #0
  beq noIns
  ldy #0
  get_patzp
  ldx ch
  sta ins, x
noIns:

  lda temp
  and #2
  cmp #0
  beq noVol
  ldy #0
  get_patzp
  ldx ch
  sta vol, x
noVol:

  ldy #0
  get_patzp
  ldx ch
  sta dur, x
end:
  lda ch
  asl
  tax
  lda patzp
  sta patseq, x
  lda patzp+1
  sta patseq+1, x
  ldx ch
  lda patzp+2
  sta patbank, x
  rts
.endmacro

add_advance_routine

.macro insarp ch
  .local end, skip1, beg, skip2
beg:
  lda doMacroA+ch
  cmp #0
  beq end

  ldx ins+ch
  lda insAL, x
  sta macroIns
  lda insAH, x
  sta macroIns+1
  ldy mframeA+ch
  lda (macroIns), y
  cmp #$fe
  beq skip2
  cmp #$ff
  bne skip1
  iny
  lda (macroIns), y
  cmp #$ff
  beq :+
  sta mframeA+ch
  jmp beg
:
  lda #0
  sta doMacroA+ch
  jmp end
skip1:
  sec
  sbc #128
  sta arp+ch
  lda #0
  sta absarp+ch
  inc mframeA+ch
  jmp end
skip2:
  iny
  lda (macroIns), y
  sta arp+ch
  lda #$ff
  sta absarp+ch
  inc mframeA+ch
  inc mframeA+ch
end:
.endmacro

.macro insvol ch
  .local end, skip1, beg, skip2
beg:
  lda doMacroV+ch
  cmp #0
  beq end

  ldx ins+ch
  lda insVL, x
  sta macroIns
  lda insVH, x
  sta macroIns+1
  ldy mframeV+ch
  lda (macroIns), y
  cmp #$ff
  bne skip1
  iny
  lda (macroIns), y
  cmp #$ff
  beq :+
  sta mframeV+ch
  jmp beg
:
  lda #0
  sta doMacroV+ch
  jmp end
skip1:
  sta volm+ch
  inc mframeV+ch
end:
.endmacro

.macro insduty ch
  .local end, skip1, beg, skip2, skip3
beg:
  lda doMacroD+ch
  cmp #0
  beq end

  ldx ins+ch
  lda insDL, x
  sta macroIns
  lda insDH, x
  sta macroIns+1
  ldy mframeD+ch
  lda (macroIns), y
  cmp #$fe
  bne skip3
  jmp skip1
skip3:
  cmp #$ff
  bne skip1
  iny
  lda (macroIns), y
  cmp #$ff
  beq :+
  sta mframeD+ch
  jmp beg
:
  lda #0
  sta doMacroD+ch
  jmp end
skip1:
  sta duty+ch
  lda #$ff
  sta change_duty+ch
  inc mframeD+ch
end:
.endmacro

.macro cmp16 val1, val2
    lda val1
    sec
    sbc val2
    php
    lda val1+1
    sbc val2+1
    php
    pla
    sta macroIns
    pla
    and #%00000010
    ora #%11111101
    and macroIns
    pha
    plp
.endmacro

.macro cmp16a val1, val2
    lda val1
    sec
    sbc #<val2
    php
    lda val1+1
    sbc #>val2
    php
    pla
    sta macroIns
    pla
    and #%00000010
    ora #%11111101
    and macroIns
    pha
    plp
.endmacro

doFinepitch:

  lda vibrato_param, x
  and #$0f
  tay
  lda tri_vibrato_lo, y
  sta patzp
  lda tri_vibrato_hi, y
  sta patzp+1

  lda vibrato_param, x
  lsr
  lsr
  lsr
  lsr
  clc
  adc vibrato_phase, x
  and #63
  sta vibrato_phase, x
  tay
  lda triangle_lookup, y
  tay

  clc
  lda note_pitch_lo, x
  adc #($80+$1f)
  sta temp
  lda note_pitch_hi, x
  adc #0
  sta temp+1

  sec
  lda temp
  sbc finepitch, x
  sta temp
  lda temp+1
  sbc #0
  sta temp+1
  bcs skip_pitch
  lda #0
  sta temp
  sta temp+1
skip_pitch:

  sec
  lda temp
  sbc (patzp), y
  sta temp
  lda temp+1
  sbc #0
  sta temp+1

  rts

do_dpcm:
  lda new_dpcm_note
  bne :+
  rts
:
  ldx ins+4

  lda #0
  sta new_dpcm_note

  lda #$0f
  sta $4015

  lda insDPCMIL, x
  sta patzp
  lda insDPCMIH, x
  sta patzp+1

  ldy note_n+4
  lda (patzp), y
  sta macroIns

  lda insDPCMPL, x
  sta patzp
  lda insDPCMPH, x
  sta patzp+1

  ldy #0
  lda (patzp), y
  cmp #$ff
  bne dpcm_map_skip
  lda note_n+4
  sta macroIns+1
  lda insDPCMIL, x
  sta patzp
  lda insDPCMIH, x
  sta patzp+1
  ldy #0
  lda (patzp), y
  sta macroIns
  jmp dpcm_map_skip_end
dpcm_map_skip:
  ldy note_n+4
  lda (patzp), y
  sta macroIns+1
dpcm_map_skip_end:

  ldy macroIns

  lda #<sampleB
  sta patzp
  lda #>sampleB
  sta patzp+1
  lda (patzp), y

  asl
  sta $5ffc
  ora #1
  sta $5ffd

  lda #<sampleA
  sta patzp
  lda #>sampleA
  sta patzp+1
  lda (patzp), y
  sta $4012

  lda #<sampleC
  sta patzp
  lda #>sampleC
  sta patzp+1
  lda (patzp), y
  sta $4013

  lda macroIns+1
  and #$0f
  sta $4010

  ;lda #$40
  lda dpcm_start_delta
  sta $4011

  lda #$1f
  sta $4015
  rts

.proc playaddr
  ldx tick_sel
  inc tick
  lda tick
  cmp tick_speeds, x
  bcc skipseq
  lda #0
  sta tick

advance_tick:
  lda tick_sel
  eor #1
  sta tick_sel
  lda #0
  sta delay_do
  .repeat chnum, I
    lda #I
    sta ch
    jsr advance
  .endrepeat

skipseq:

  lda #$ff
  sta delay_do

  ldx #chnum-1
note_delay_loop:
  txa
  pha

  lda note_delay, x
  cmp #$ff
  beq note_delay_loop_end
  dec note_delay, x
  lda note_delay, x
  cmp #$ff
  beq :+
  jmp note_delay_loop_end
:
  stx ch
  jsr advance
note_delay_loop_end:
  pla
  tax
  dex
  bpl note_delay_loop



  jsr do_dpcm

  lda nextpat
  beq skipnextpat
  lda #0
  sta nextpat
  inc patind
  lda patind
  cmp #order0len
  bne :+
  lda #loop_pattern_num
  sta patind
:
  jsr set_patseq
  ldx #chnum-1
  lda #1
durloop:
    sta dur, x
    dex
    bpl durloop
  jmp advance_tick
skipnextpat:


.repeat chnum, I
    .if I <> 4
        insarp I
        insvol I
        insduty I
    .endif
    inc note_dur+I
    lda note_dur+I
    cmp #96
    bne :+
    lda #0
    sta note_dur+I
:
.endrepeat


  ldx #3
vol_add_loop:
  lda vol_tick, x
  and #$80
  bne vol_add_loop_end
  lda vol_tick, x
  and #3
  sta vol_tick, x
  inc vol_tick, x
  ora volume_add, x
  tay
  lda vol_slide_lookup, y
  clc
  adc vol, x
  sta vol, x
  and #$80
  cmp #$80
  bne :+
  lda #0
  sta vol, x
  jmp vol_add_loop_end
:
  lda vol, x
  cmp #$0f
  bcc vol_add_loop_end
  lda #$0f
  sta vol, x
vol_add_loop_end:
  dex
  bpl vol_add_loop

  ldx #chnum-1
note_cut_loop:
  lda cut_dur, x
  cmp #$ff
  beq note_cut_loop_end
  dec cut_dur, x
  lda cut_dur, x
  cmp #0
  beq :+
  jmp note_cut_loop_end
:
  lda #$ff
  sta cut_dur, x
  sta isoff, x
note_cut_loop_end:
  dex
  bpl note_cut_loop

  ldx #chnum-1
relslide_loop:
  lda note_dest ,x
  and #$80
  beq slide_skip
  eor note_dest ,x
  sta macroIns
  lda slide_amt_sign, x
  beq positive_slide2
  lda note_n, x
  clc
  adc macroIns
  sta note_dest, x
  jmp slide_skip
positive_slide2:
  lda note_n, x
  sec
  sbc macroIns
  sta note_dest, x
slide_skip:
  dex
  bpl relslide_loop

  ldx #4
note_loop:
  jsr conv_freq
  dex
  bpl note_loop

  ldx #3
slide_loop:
  lda slide_amt, x
  cmp #0
  bne :+
  jmp slide_loop2
:
  lda slide_amt_sign, x
  beq positive_slide
  sec
  lda slide_buffer_lo, x
  sbc slide_amt, x
  sta slide_buffer_lo, x
  lda slide_buffer_hi, x
  sbc #0
  sta slide_buffer_hi, x
;  bvc :+ ; i've never used this instruction before lmao
;  jmp finish_slide
;:
  lda note_dest, x
  tay
  lda note_table_lo, y
  sta patzp
  lda note_table_hi, y
  sta patzp+1
  lda note_pitch_lo, x
  sta patzp+2
  lda note_pitch_hi, x
  sta patzp+3
  cmp16 patzp, patzp+2
  bcc slide_loop2
  jmp finish_slide
positive_slide:
  clc
  lda slide_buffer_lo, x
  adc slide_amt, x
  sta slide_buffer_lo, x
  lda slide_buffer_hi, x
  adc #0
  sta slide_buffer_hi, x
;  bcc :+
;:
  lda note_dest, x
  tay
  lda note_table_lo, y
  sta patzp
  lda note_table_hi, y
  sta patzp+1
  lda note_pitch_lo, x
  sta patzp+2
  lda note_pitch_hi, x
  sta patzp+3
  cmp16 patzp+2, patzp
  bcc slide_loop2
finish_slide:
  lda note_dest, x
  sta note_n, x
  lda #0
  sta slide_buffer_lo, x
  sta slide_buffer_hi, x
  sta slide_amt, x
  sta slide_amt_sign, x
  jsr conv_freq
slide_loop2:
  dex
  bmi slide_loopt
  jmp slide_loop
slide_loopt:

  ldx #chnum-1
:
  lda vol, x
  asl
  asl
  asl
  asl
  ora volm, x
  tay
  lda volmul, y
  sta volout, x
  dex
  bpl :-

  ldx #0
  jsr doFinepitch

 
  lda duty
  and #3
  tax
  lda #$30
  ora dutytbl, x
  sta vol_or
  lda volout
  ldx isoff
  beq :+
  lda #0
:
  sta vol_pulse
  lda temp
  sta periods
  sta $4002
  lda temp+1
  pha
  and #7
  sta periods+1
  pla
  ora #%11111000
  cmp timerH
  beq :+
  sta $4003
  sta timerH
:

  ldx #1
  jsr doFinepitch

  lda duty+1
  and #3
  tax
  lda #$30
  ora dutytbl, x
  sta vol_or+1
  lda volout+1
  ldx isoff+1
  beq :+
  lda #0
:
  sta vol_pulse+1
  lda temp
  sta periods+2
  sta $4006
  lda temp+1
  pha
  and #7
  sta periods+3
  pla
  ora #%11111000
  cmp timerH+1
  beq :+
  sta $4007
  sta timerH+1
:


  ldx #2
  jsr doFinepitch

  lda temp
  sta $400A
  lda temp+1
  ora #%11111000
  sta $400B
  lda #$0
  cmp volout+2
  beq :+
  ldx isoff+2
  bne :+
  lda #$81
:
  sta $4008


  lda #$30
  ldx isoff+3
  bne :+
  ora volout+3
:
  sta $400C
  lda #$f8
  sta $400F
  lda absarp+3
  beq NOISrel
  lda arp+3
  jmp NOISout
NOISrel:
  lda note_n+3
  clc
  adc arp+3
NOISout:
  clc
  jsr add_arpeff
  clc
  adc slide_buffer_lo+3
  and #15
  eor #15
  pha
  lda duty+3
  and #1
  tax
  pla
  ora dutytbl_noise, x
  sta $400E


  lda #0
  sta $5ffa

.repeat 2, I
  ldx ins+I
  lda tfxEn, x 
  cmp #$ff
  beq :+
  sta tfx_mode+I
:
  lda tfxOff, x 
  cmp #$ff
  beq :+
  sta tfx_off+I
:
  lda tfxArp, x 
  cmp #$ff
  beq :+
  sta tfx_arp+I
:
  lda tfxBnd, x 
  cmp #$ff
  beq :+
:
  sta tfx_bound+I

  lda periods+I*2+0
  sta NUM1
  lda periods+I*2+1
  sta NUM1+1

  ldx tfx_arp+I
  lda timer_arp_mul, x
  sta NUM2
  lda timer_arp_mul+32, x
  sta NUM2+1

  jsr mul16

  lda RESULT+1
  sta periods+I*2+0
  lda RESULT+2
  sta periods+I*2+1

  lda periods+I*2+0
  clc
  adc tfx_off+I
  sta periods+I*2+0
  bcc :+
  inc periods+I*2+1
:

  lda periods+I*2+0
  sec
  sbc #64
  sta periods+I*2+0
  lda periods+I*2+1
  sbc #0
  sta periods+I*2+1

  lda #0
  sta temp_index
  ldy periods+I*2+0
  lda periods+I*2+1
  ora #$a0
  sta temp_index+1
  lda (temp_index), y
  sta tfx_lo_pitch+I

  lda periods+I*2+1
  ora #$a8
  sta temp_index+1
  lda (temp_index), y
  sta tfx_hi_pitch+I

  lda tfx_mode+I
  bne :+
  lda vol_or+I
  ora vol_pulse+I
  sta $4000+I*4
:
.endrepeat
  rts
.endproc

mul16:
        LDA #0       ;Initialize RESULT to 0
        STA RESULT+2
        LDX #16      ;There are 16 bits in NUM2
L1:     LSR NUM2+1   ;Get low bit of NUM2
        ROR NUM2
        BCC L2       ;0 or 1?
        TAY          ;If 1, add NUM1 (hi byte of RESULT is in A)
        CLC
        LDA NUM1
        ADC RESULT+2
        STA RESULT+2
        TYA
        ADC NUM1+1
L2:     ROR A        ;"Stairstep" shift
        ROR RESULT+2
        ROR RESULT+1
        ROR RESULT
        DEX
        BNE L1
        STA RESULT+3
    rts

set_patseq:
  stx temp+2

  ldx patind

  .repeat chnum, I
    lda .ident(.concat ("order", .sprintf("%d",I), "L")), x
    sta patseq+0+2*I
    lda .ident(.concat ("order", .sprintf("%d",I), "H")), x
    clc
    adc #$80
    sta patseq+1+2*I
    lda .ident(.concat ("order", .sprintf("%d",I), "B")), x
    sta patbank+I
  .endrepeat

  ldx temp+2
  rts

set_patseq_init:
  .repeat chnum, I
    ldx patind
    lda .ident(.concat ("order", .sprintf("%d",I), "L")), x
    sta patseq+0+2*I
    lda .ident(.concat ("order", .sprintf("%d",I), "H")), x
    clc
    adc #$80
    sta patseq+1+2*I
    lda .ident(.concat ("order", .sprintf("%d",I), "B")), x
    sta patbank+I
  .endrepeat
  rts


add_arpeff:
  pha
  inc arpind, x
  lda arpind, x
  tay
  lda arp_mod, y
  sta arpind, x
  tay
  pla
  cpy #1
  beq arp1
  cpy #2
  beq arp2
  rts

arp1:
  clc
  adc arpeff1, x
  rts

arp2:
  clc
  adc arpeff2, x
  rts

conv_freq:
  lda absarp, x
  beq nrel
  lda arp, x
  and #127
  jmp nout
nrel:
  lda note_n, x
  clc
  adc arp, x
nout:
  clc
  jsr add_arpeff
  tay
  clc
  lda note_table_lo, y
  adc slide_buffer_lo, x
  sta note_pitch_lo, x
  lda note_table_hi, y
  adc slide_buffer_hi, x
  sta note_pitch_hi, x
  rts

arp_mod:
.byte 0,1,2,0

patLL:
  .repeat chnum, I
    .lobytes .ident(.concat ("order", .sprintf("%d",I), "L"))
  .endrepeat
patLH:
  .repeat chnum, I
    .hibytes .ident(.concat ("order", .sprintf("%d",I), "L"))
  .endrepeat
patHL:
  .repeat chnum, I
    .lobytes .ident(.concat ("order", .sprintf("%d",I), "H"))
  .endrepeat
patHH:
  .repeat chnum, I
    .hibytes .ident(.concat ("order", .sprintf("%d",I), "H"))
  .endrepeat
patBL:
  .repeat chnum, I
    .lobytes .ident(.concat ("order", .sprintf("%d",I), "B"))
  .endrepeat
patBH:
  .repeat chnum, I
    .hibytes .ident(.concat ("order", .sprintf("%d",I), "B"))
  .endrepeat

.include "note_tables.asm"

dutytbl:
  .byte %00000000, %01000000, %10000000, %11000000

dutytbl_noise:
  .byte %00000000, %10000000

;.res 256-(*&$ff), 0

volmul: ; directly taken from Dn-FT's sound driver :/
.repeat 16, xx
	.repeat 16, yy
		.if xx = 0 || yy = 0
			.byte 0
		.elseif xx * yy < 15
			.byte 1
		.else
			.byte xx * yy / 15
		.endif
	.endrep
.endrep

triangle_lookup:
  .repeat 64, I
    .if (I+0)&32
      .byte 32-(((I+0)&63)-32)-1
    .else
      .byte (I+0)&63
    .endif
  .endrepeat

tri_vibrato_lookup:
  .repeat 16, I
    .repeat 32, J
      .byte ((I*(J-15)*2)/15)+$1f
    .endrepeat
  .endrepeat

tri_vibrato_lo:
  .repeat 16, I
    .lobytes tri_vibrato_lookup+I*32
  .endrepeat

tri_vibrato_hi:
  .repeat 16, I
    .hibytes tri_vibrato_lookup+I*32
  .endrepeat

vol_slide_lookup:
  .byte 0,0,0,0
  .byte 1,0,0,0
  .byte 1,0,1,0
  .byte 1,1,1,0
  .byte 1,1,1,1
  .byte 2,1,1,1
  .byte 2,1,2,1
  .byte 2,2,2,1
  .byte 2,2,2,2
  .byte 3,2,2,2
  .byte 3,2,3,2
  .byte 3,3,3,2
  .byte 3,3,3,3
  .byte 4,3,3,3
  .byte 4,3,4,3
  .byte 4,4,4,4

  .byte 000,000,000,000
  .byte 255,000,000,000
  .byte 255,000,255,000
  .byte 255,255,255,000
  .byte 255,255,255,255
  .byte 254,255,255,255
  .byte 254,255,254,255
  .byte 254,254,254,255
  .byte 254,254,254,254
  .byte 253,254,254,254
  .byte 253,254,253,254
  .byte 253,253,253,254
  .byte 253,253,253,253
  .byte 252,253,253,253
  .byte 252,253,252,253
  .byte 252,252,252,252

timer_arp_mul:
.incbin "arpmul_tabl.bin"

.include "song.asm"

.segment "CHR"

.segment "DATA1"
.incbin "lotabl.bin"
.incbin "hitabl.bin"

