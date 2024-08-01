;;; cl65 -d -vm -l nsf.lst -g -t nes -C nsf.cfg -m nsf.map -Ln nsf.lbl -o driar.nsf nsf.asm

.define FDS_SUPPORT 0
.define VRC7_SUPPORT 0
.define loop_pattern_num 0
.define porta_once 1

.define chnum 5

.if FDS_SUPPORT = 1
  .define FDS_CHANNEL 5
.endif

.if VRC7_SUPPORT = 1
  .define VRC7_CHANNEL 5
.endif

.segment "HEADER"
	.import __ROM0_START__
	.macro pad32 str
	 .if (.strlen(str) > 31)
	  .error "pad32 given too long input"
	 .endif
	 .byte str,0
	 .res 31-.strlen(str)
	.endmacro

	.byte "NESM",$1a
	.byte 1     ; version
	.byte 1    ; number of songs
	.byte 1     ; starting song
	.word __ROM0_START__ ;;; This is where I was wrong. loadaddr should be the start of the ROM0 segment
	.word $8000
	.word $8003
	;; ....0123456789a123456789b123456789c1
	pad32 "FurNES Test"
	pad32 "AArt1256"
	pad32 "2024 AArt1256"
	.word 16639 ; Real NTSC rate
	.byte 0,1,2,0,0,0,0,0 ; disable bankswitching
	.word 19997	; Real PAL rate
	.byte 0	; Prefer PAL, compatible with both
	.byte FDS_SUPPORT<<2|VRC7_SUPPORT<<1	; no expansion audio
	.byte 0,0,0,0

;tick_speed = 3

.ZEROPAGE
.org $10
patzp: .res 3
macroIns: .res 2

.segment "BSS"
.org $200
patseq: .res chnum*2
patbank: .res chnum
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
.if VRC7_SUPPORT = 1
timerL: .res chnum
.endif
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
.if VRC7_SUPPORT = 1
custom_patch: .res 8
write_patch: .res 1
.endif
arpeff1: .res chnum
arpeff2: .res chnum
arpind: .res chnum

.segment "CODE"
.org $8000
jmp initaddr
jmp playaddr
.byte "furNES driver by AArt1256", 0

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
  .if FDS_SUPPORT = 1
    lda #$00
    sta $4023
    lda #$83
    sta $4023
    lda #$FF
    sta $408A
  .endif
  .if VRC7_SUPPORT = 1
     lda #0
     sta write_patch
     tax
:
     stx $9010
     jsr wait_9010
     sta $9030
     jsr wait_9030
     inx
     cpx #$3f
     bne :-
  .endif
  lda #8
  sta $4001
  lda #8
  sta $4005

.if VRC7_SUPPORT = 1
  ldx #7
  lda #0
:
  sta custom_patch, x
  dex
  bpl :-
.endif

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
.if VRC7_SUPPORT = 1
  sta timerL, x
.endif
  sta timerH, x
  lda #$ff
  sta isoff, x
  sta vol_tick, x
  lda #15
  sta vol, x
  sta volm, x
  dex
  bpl :-

  .if FDS_SUPPORT = 1
    lda #$20
    sta vol+FDS_CHANNEL
    sta volm+FDS_CHANNEL
  .endif

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
	cmp #$c0
	bne skipW
	lda #$b0
	sta patzp+1
	inc patzp+2
skipW:
	lda patzp+2
  clc
  adc #2
	sta $5ffb
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
  .if VRC7_SUPPORT = 1
    ldx #0
    :
        iny
        lda (macroIns), y
        sta custom_patch, x
        inx
        cpx #8
        bne :-
    lda mframeD+ch
    clc
    adc #9
    sta mframeD+ch
    lda #$ff
    sta write_patch
    jmp end
  .else
    jmp skip1
  .endif
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
  .if (VRC7_SUPPORT = 1)
    .if (ch >= VRC7_CHANNEL) && (ch < (VRC7_CHANNEL+6))
        ldy change_duty+ch
        cpy #0
        beq :+
        inc mframeD+ch
        lda #0
        sta change_duty+ch
        jmp end
    :
        sta duty+ch
    .else
        sta duty+ch
        lda #$ff
        sta change_duty+ch
    .endif
  .else
    sta duty+ch
    lda #$ff
    sta change_duty+ch
  .endif
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

.if VRC7_SUPPORT = 1
doFinepitchVRC7:
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
  adc #($80+($1f*4))
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
  bcs skip_pitchVRC7
  lda #0
  sta temp
  sta temp+1
skip_pitchVRC7:

  lda (patzp), y
  asl
  asl
  sta patzp

  sec
  lda temp
  sbc patzp
  sta temp
  lda temp+1
  sbc #0
  sta temp+1

  rts
.endif

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
  clc
  adc #3
	sta $5ffc
  clc
  adc #1
  sta $5ffd
  clc
  adc #1
  sta $5ffe

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

  dex
note_loop2:
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
  jmp slide_loop2
slide_loop2:
  dex
  bmi slide_loopt
  jmp slide_loop
slide_loopt:

.if FDS_SUPPORT = 1
  lda vol_tick+FDS_CHANNEL
  cmp #$ff
  beq vol_add_loop_end_fds
  lda vol_tick+FDS_CHANNEL
  and #3
  sta vol_tick+FDS_CHANNEL
  inc vol_tick+FDS_CHANNEL
  ora volume_add+FDS_CHANNEL
  tay
  lda vol_slide_lookup, y
  clc
  adc vol+FDS_CHANNEL
  sta vol+FDS_CHANNEL
  and #$80
  cmp #$80
  bne :+
  lda #0
  sta vol+FDS_CHANNEL
  jmp vol_add_loop_end_fds
:
  lda vol+FDS_CHANNEL
  cmp #$20
  bcc vol_add_loop_end_fds
  lda #$20
  sta vol+FDS_CHANNEL
vol_add_loop_end_fds:

  lda absarp+FDS_CHANNEL
  beq nrel_fds
  lda arp+FDS_CHANNEL
  and #127
  jmp nout_fds
nrel_fds:
  lda note_n+FDS_CHANNEL
  clc
  adc arp+FDS_CHANNEL
nout_fds:
  clc
  jsr add_arpeff
  tay
  clc
  lda fds_pitch_lo, y
  adc slide_buffer_lo+FDS_CHANNEL
  sta note_pitch_lo+FDS_CHANNEL
  lda fds_pitch_hi, y
  adc slide_buffer_hi+FDS_CHANNEL
  sta note_pitch_hi+FDS_CHANNEL

  clc
  lda slide_amt+FDS_CHANNEL
  bne :+
  jmp slide_loop_fds
:
  lda slide_amt_sign+FDS_CHANNEL
  bne positive_slide_fds
  sec
  lda slide_buffer_lo+FDS_CHANNEL
  sbc slide_amt+FDS_CHANNEL
  sta slide_buffer_lo+FDS_CHANNEL
  lda slide_buffer_hi+FDS_CHANNEL
  sbc #0
  sta slide_buffer_hi+FDS_CHANNEL

  ldy note_dest+FDS_CHANNEL
  lda fds_pitch_lo, y
  sta patzp
  lda fds_pitch_hi, y
  sta patzp+1
  lda note_pitch_lo+FDS_CHANNEL
  sta patzp+2
  lda note_pitch_hi+FDS_CHANNEL
  sta patzp+3
  cmp16 patzp, patzp+2
  bcc slide_loop_fds
  jmp finish_slide_fds
positive_slide_fds:
  clc
  lda slide_buffer_lo+FDS_CHANNEL
  adc slide_amt+FDS_CHANNEL
  sta slide_buffer_lo+FDS_CHANNEL
  lda slide_buffer_hi+FDS_CHANNEL
  adc #0
  sta slide_buffer_hi+FDS_CHANNEL

  ldy note_dest+FDS_CHANNEL
  lda fds_pitch_lo, y
  sta patzp
  lda fds_pitch_hi, y
  sta patzp+1
  lda note_pitch_lo+FDS_CHANNEL
  sta patzp+2
  lda note_pitch_hi+FDS_CHANNEL
  sta patzp+3
  cmp16 patzp+2, patzp
  bcc slide_loop_fds
  jmp finish_slide_fds

finish_slide_fds:
  lda note_dest+FDS_CHANNEL
  sta note_n+FDS_CHANNEL
  lda #0
  sta slide_buffer_lo+FDS_CHANNEL
  sta slide_buffer_hi+FDS_CHANNEL
  sta slide_amt+FDS_CHANNEL
  sta slide_amt_sign+FDS_CHANNEL
  jmp slide_loop_fds
slide_loop_fds:
.endif


.if VRC7_SUPPORT = 1
  ldx #5
VRC7_LOOP:
  lda vol_tick+VRC7_CHANNEL, x
  cmp #$ff
  beq vol_add_loop_end_vrc7
  lda vol_tick+VRC7_CHANNEL, x
  and #3
  sta vol_tick+VRC7_CHANNEL, x
  inc vol_tick+VRC7_CHANNEL, x
  ora volume_add+VRC7_CHANNEL, x
  tay
  lda vol_slide_lookup, y
  clc
  adc vol+VRC7_CHANNEL, x
  sta vol+VRC7_CHANNEL, x
  and #$80
  cmp #$80
  bne :+
  lda #0
  sta vol+VRC7_CHANNEL, x
  jmp vol_add_loop_end_vrc7
:
  lda vol+VRC7_CHANNEL, x
  cmp #$10
  bcc vol_add_loop_end_vrc7
  lda #$10
  sta vol+VRC7_CHANNEL, x
vol_add_loop_end_vrc7:

  lda absarp+VRC7_CHANNEL, x
  beq nrel_vrc7
  lda arp+VRC7_CHANNEL, x
  and #127
  jmp nout_vrc7
nrel_vrc7:
  lda note_n+VRC7_CHANNEL, x
  clc
  adc arp+VRC7_CHANNEL, x
nout_vrc7:
  clc
  jsr add_arpeffVRC7
  tay
  clc
  lda vrc7_pitch_lo, y
  adc slide_buffer_lo+VRC7_CHANNEL, x
  sta note_pitch_lo+VRC7_CHANNEL, x
  lda vrc7_pitch_hi, y
  adc slide_buffer_hi+VRC7_CHANNEL, x
  sta note_pitch_hi+VRC7_CHANNEL, x

  clc
  lda slide_amt+VRC7_CHANNEL, x
  bne :+
  jmp slide_loop_vrc7
:
  lda slide_amt_sign+VRC7_CHANNEL, x
  bne positive_slide_vrc7
  sec
  lda slide_buffer_lo+VRC7_CHANNEL, x
  sbc slide_amt+VRC7_CHANNEL, x
  sta slide_buffer_lo+VRC7_CHANNEL, x
  lda slide_buffer_hi+VRC7_CHANNEL, x
  sbc #0
  sta slide_buffer_hi+VRC7_CHANNEL, x

  ldy note_dest+VRC7_CHANNEL, x
  lda vrc7_pitch_lo, y
  sta patzp
  lda vrc7_pitch_hi, y
  sta patzp+1
  lda note_pitch_lo+VRC7_CHANNEL, x
  sta patzp+2
  lda note_pitch_hi+VRC7_CHANNEL, x
  sta patzp+3
  cmp16 patzp, patzp+2
  bcc slide_loop_vrc7
  jmp finish_slide_vrc7
positive_slide_vrc7:
  clc
  lda slide_buffer_lo+VRC7_CHANNEL, x
  adc slide_amt+VRC7_CHANNEL, x
  sta slide_buffer_lo+VRC7_CHANNEL, x
  lda slide_buffer_hi+VRC7_CHANNEL, x
  adc #0
  sta slide_buffer_hi+VRC7_CHANNEL, x

  ldy note_dest+VRC7_CHANNEL, x
  lda vrc7_pitch_lo, y
  sta patzp
  lda vrc7_pitch_hi, y
  sta patzp+1
  lda note_pitch_lo+VRC7_CHANNEL, x
  sta patzp+2
  lda note_pitch_hi+VRC7_CHANNEL, x
  sta patzp+3
  cmp16 patzp+2, patzp
  bcc slide_loop_vrc7
  jmp finish_slide_vrc7

finish_slide_vrc7:
  lda note_dest+VRC7_CHANNEL, x
  sta note_n+VRC7_CHANNEL, x
  lda #0
  sta slide_buffer_lo+VRC7_CHANNEL, x
  sta slide_buffer_hi+VRC7_CHANNEL, x
  sta slide_amt+VRC7_CHANNEL, x
  sta slide_amt_sign+VRC7_CHANNEL, x
  jmp slide_loop_vrc7
slide_loop_vrc7:
  dex
  bmi :+
  jmp VRC7_LOOP
:
.endif

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
  ldx isoff
  bne :+
  ora volout
:
  sta $4000
  lda temp
  sta $4002
  lda temp+1
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
  ldx isoff+1
  bne :+
  ora volout+1
:
  sta $4004
  lda temp
  sta $4006
  lda temp+1
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

  .if FDS_SUPPORT = 1
    lda change_duty+FDS_CHANNEL
    beq skip_fds_waveform

    lda #0
    sta change_duty+FDS_CHANNEL

    ldx duty+FDS_CHANNEL
    lda wavL, x
    sta patzp
    lda wavH, x
    sta patzp+1

    lda #$80
    sta $4089
    ldy #$3f
:
    lda (patzp), y
    sta $4040, y
    dey
    bpl :-
    lda #$00
    sta $4089
skip_fds_waveform:
    ldx vol+FDS_CHANNEL
    lda fds_vol_table, x
    ldx volm+FDS_CHANNEL
    ora fds_vol_table2, x
    tay
    lda #0
    ldx isoff+FDS_CHANNEL
    bne :+
    lda volmul, y
    asl
:
    ora #%10000000
    sta $4080

    ldx #FDS_CHANNEL
    jsr doFinepitch

    lda temp
    sta $4082
    lda temp+1
    and #%00001111
    cmp timerH+FDS_CHANNEL
    beq :+
    sta $4083
    sta timerH+FDS_CHANNEL
  :
  .endif

  .if VRC7_SUPPORT = 1
     .repeat 6, I
       lda write_patch
       beq :++
       ldx #0
       stx write_patch
:
       stx $9010
       jsr wait_9010
       lda custom_patch, x
       sta $9030
       jsr wait_9030
       inx
       cpx #8
       bne :-
:
       ldx #VRC7_CHANNEL+I
       jsr doFinepitchVRC7


       lda temp+1
       lsr
       lsr
       and #%1110
;       lsr
;       and #7
;       asl
       sta patzp

       .repeat 2
          clc
          lsr temp+1
          ror temp
       .endrepeat

       lda temp+1
       and #1
       ora patzp
       sta patzp

       ldx #VRC7_CHANNEL+I

       lda #$30|I
       sta $9010
       jsr wait_9010
       lda duty, x
       asl
       asl
       asl
       asl
       ldy volout, x
       ora vol_reverse_4bit, y
       sta $9030
       jsr wait_9030


       lda note_play, x
       beq :+
       lda #$20|I
       sta $9010
       jsr wait_9010
       lda patzp
       sta $9030
       jsr wait_9030
:

       lda isoff, x
       and #%00010000
       eor #%00010000
       ora patzp
       sta patzp

       lda patzp
       sta timerH, x
       lda temp
       sta timerL, x
       lda #$20|I
       sta $9010
       jsr wait_9010
       lda timerH, x
       sta $9030
       jsr wait_9030

       lda #$10|I
       sta $9010
       jsr wait_9010
       lda temp
       sta $9030
       jsr wait_9030


       lda #0
       sta note_play, x
     .endrepeat
  .endif

  rts
.endproc

.if VRC7_SUPPORT = 1
wait_9030:          ; JSR to this label immediately after writing to $9030
    tya
    pha
   ldy #$08
@wait_loop:
    dey
    bne @wait_loop
    pla
    tay
wait_9010:          ; JSR to this label immediately after writing to $9010
    rts
.endif

set_patseq:
  stx temp+2

  ldx patind

  .repeat chnum, I
    lda .ident(.concat ("order", .sprintf("%d",I), "L")), x
    sta patseq+0+2*I
    lda .ident(.concat ("order", .sprintf("%d",I), "H")), x
    clc
    adc #$b0
    sta patseq+1+2*I
    lda .ident(.concat ("order", .sprintf("%d",I), "B")), x
    clc
    adc #1
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
    adc #$b0
    sta patseq+1+2*I
    lda .ident(.concat ("order", .sprintf("%d",I), "B")), x
    clc
    adc #1
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

.if VRC7_SUPPORT = 1
add_arpeffVRC7:
  pha
  inc arpind+VRC7_CHANNEL, x
  lda arpind+VRC7_CHANNEL, x
  tay
  lda arp_mod+VRC7_CHANNEL, y
  sta arpind+VRC7_CHANNEL, x
  tay
  pla
  cpy #1
  beq arp1VRC7
  cpy #2
  beq arp2VRC7
  rts

arp1VRC7:
  clc
  adc arpeff1+VRC7_CHANNEL, x
  rts

arp2VRC7:
  clc
  adc arpeff2+VRC7_CHANNEL, x
  rts
.endif

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

note_table_lo:
  .res 9, $f1
  .byt $f1,$7f,$13,$ad,$4d,$f3,$9d,$4c,$00,$b8,$74,$34
  .byt $f8,$bf,$89,$56,$26,$f9,$ce,$a6,$80,$5c,$3a,$1a
  .byt $fb,$df,$c4,$ab,$93,$7c,$67,$52,$3f,$2d,$1c,$0c
  .byt $fd,$ef,$e1,$d5,$c9,$bd,$b3,$a9,$9f,$96,$8e,$86
  .byt $7e,$77,$70,$6a,$64,$5e,$59,$54,$4f,$4b,$46,$42
  .byt $3f,$3b,$38,$34,$31,$2f,$2c,$29,$27,$25,$23,$21
  .byt $1f,$1d,$1b,$1a,$18,$17,$15,$14
note_table_hi:
  .res 9, $07
  .byt $07,$07,$07,$06,$06,$05,$05,$05,$05,$04,$04,$04
  .byt $03,$03,$03,$03,$03,$02,$02,$02,$02,$02,$02,$02
  .byt $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
  .byt $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  .byt $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  .byt $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  .byt $00,$00,$00,$00,$00,$00,$00,$00

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

.if FDS_SUPPORT = 1
fds_vol_table:
  .repeat 33, I
    .if I = 32
      .byte 15
    .else
      .byte I>>1
    .endif
  .endrepeat
fds_vol_table2:
  .repeat 33, I
    .if I = 32
      .byte 15<<4
    .else
      .byte (I>>1)<<4
    .endif
  .endrepeat
fds_pitch_lo:
	.lobytes	$004D, $0051, $0056, $005B, $0061, $0066, $006C, $0073, $007A, $0081, $0089, $0091
	.lobytes	$0099, $00A2, $00AC, $00B6, $00C1, $00CD, $00D9, $00E6, $00F3, $0102, $0111, $0121
	.lobytes	$0133, $0145, $0158, $016D, $0182, $0199, $01B2, $01CB, $01E7, $0204, $0222, $0243
	.lobytes	$0265, $028A, $02B0, $02D9, $0304, $0332, $0363, $0397, $03CD, $0407, $0444, $0485
	.lobytes	$04CA, $0513, $0560, $05B2, $0609, $0665, $06C6, $072D, $079B, $080E, $0889, $090B
	.lobytes	$0994, $0A26, $0AC1, $0B64, $0C12, $0CCA, $0D8C
fds_pitch_hi:
	.hibytes	$004D, $0051, $0056, $005B, $0061, $0066, $006C, $0073, $007A, $0081, $0089, $0091
	.hibytes	$0099, $00A2, $00AC, $00B6, $00C1, $00CD, $00D9, $00E6, $00F3, $0102, $0111, $0121
	.hibytes	$0133, $0145, $0158, $016D, $0182, $0199, $01B2, $01CB, $01E7, $0204, $0222, $0243
	.hibytes	$0265, $028A, $02B0, $02D9, $0304, $0332, $0363, $0397, $03CD, $0407, $0444, $0485
	.hibytes	$04CA, $0513, $0560, $05B2, $0609, $0665, $06C6, $072D, $079B, $080E, $0889, $090B
	.hibytes	$0994, $0A26, $0AC1, $0B64, $0C12, $0CCA, $0D8C
.endif

.if VRC7_SUPPORT = 1
; Fnum table, multiplied by 4 for higher resolution (taken from the DnFT driver)

vrc7_pitch_lo:
    .repeat 8, I
	    .lobytes $02B0+(I*2048), $02DC+(I*2048), $0308+(I*2048), $0334+(I*2048), $0364+(I*2048), $0398+(I*2048), $03D0+(I*2048), $0408+(I*2048), $0448+(I*2048), $0488+(I*2048), $04CC+(I*2048), $0518+(I*2048)
    .endrepeat
vrc7_pitch_hi:
    .repeat 8, I
	    .hibytes $02B0+(I*2048), $02DC+(I*2048), $0308+(I*2048), $0334+(I*2048), $0364+(I*2048), $0398+(I*2048), $03D0+(I*2048), $0408+(I*2048), $0448+(I*2048), $0488+(I*2048), $04CC+(I*2048), $0518+(I*2048)
    .endrepeat
vol_reverse_4bit:
    .repeat 16, I
        .byte 15-I
    .endrepeat
.endif

.include "song.asm"
