
;;; Message for Hannah...

    org $fffc
    word reset
    word irq

    org $8000

g_ticks = $A0

GAP = 170
QUICK = 40

    include via.s
    include ticks.s
    include sound.s
    include lcd.s

irq:
    bit via.T1CL ; acknowledge interrupt
    inc g_ticks
    rti

reset:
    jsr via.init
    jsr init_ticks
    jsr sound.init ; silence
    jsr lcd.init

    ldx #(message1 & $ff)       ;lo
    ldy #(message1 >> 8)        ;hi
    jsr print_message
    lda #GAP
    jsr pause

    ldx #(message2 & $ff)       ;lo
    ldy #(message2 >> 8)        ;hi
    jsr print_message
    lda #GAP
    jsr pause
    lda #GAP
    jsr pause

    ldx #(message3 & $ff)       ;lo
    ldy #(message3 >> 8)        ;hi
    jsr print_message
    lda #GAP
    jsr pause
    lda #GAP
    jsr pause

    ldx #(message4 & $ff)       ;lo
    ldy #(message4 >> 8)        ;hi
    jsr print_message
    lda #GAP
    jsr pause

    ldx #(message5 & $ff)       ;lo
    ldy #(message5 >> 8)        ;hi
    jsr print_message
    lda #GAP
    jsr pause
    lda #GAP
    jsr pause

    ldx #(message6 & $ff)       ;lo
    ldy #(message6 >> 8)        ;hi
    jsr print_message
    lda #GAP
    jsr pause

    ldx #(message7 & $ff)       ;lo
    ldy #(message7 >> 8)        ;hi
    jsr print_message
    lda #GAP
    jsr pause

    ldx #(message8 & $ff)       ;lo
    ldy #(message8 >> 8)        ;hi
    jsr print_message
    lda #GAP
    jsr pause
    lda #GAP
    jsr pause

    ldx #(message9 & $ff)       ;lo
    ldy #(message9 >> 8)        ;hi
    jsr print_message
    lda #GAP
    jsr pause
    ldx #(message10 & $ff)       ;lo
    ldy #(message10 >> 8)        ;hi
    jsr print_message
    lda #QUICK
    jsr pause
    ldx #(message11 & $ff)       ;lo
    ldy #(message11 >> 8)        ;hi
    jsr print_message
    lda #QUICK
    jsr pause
    ldx #(message12 & $ff)       ;lo
    ldy #(message12 >> 8)        ;hi
    jsr print_message
    lda #QUICK
    jsr pause
    ldx #(message13 & $ff)       ;lo
    ldy #(message13 >> 8)        ;hi
    jsr print_message
    lda #QUICK
    jsr pause
    ldx #(message14 & $ff)       ;lo
    ldy #(message14 >> 8)        ;hi
    jsr print_message
    lda #GAP
    jsr pause
    lda #GAP
    jsr pause

    ldx #(message15 & $ff)       ;lo
    ldy #(message15 >> 8)        ;hi
    jsr print_message
    lda #GAP
    jsr pause
    lda #GAP
    jsr pause

    ldx #(message16 & $ff)       ;lo
    ldy #(message16 >> 8)        ;hi
    jsr print_message
    lda #GAP
    jsr pause
    lda #GAP
    jsr pause

    ldx #(message17 & $ff)       ;lo
    ldy #(message17 >> 8)        ;hi
    jsr print_message
    lda #GAP
    jsr pause

    ldx #(message18 & $ff)       ;lo
    ldy #(message18 >> 8)        ;hi
    jsr print_message
    lda #GAP
    jsr pause

spin:
    jmp spin

;;               ..1111111111111111........................2222222222222222...
message1:  asciiz " Hello Hannah!  "
message2:  asciiz " This is my new                          6502 computer.  "
message3:  asciiz " Top row is CPU                           RAM and ROM.   "

message4:  asciiz "   Below that                           is the I/O chip: "
message5:  asciiz "for controlling                         the mini screen. "

message6:  asciiz "The Silver cube                           bottom left... "
message7:  asciiz "  is the clock                          for keeping time:"
message8:  asciiz "1,000,000 ticks                           each second.   "

message9:  asciiz "      So..       "
message10: asciiz "       we       "
message11: asciiz "      can       "
message12: asciiz "       do       "
message13: asciiz "     things     "
message14: asciiz "      quick!    "

message15: asciiz "    ...or...    "
message16: asciiz "  ...s.l.o.w... "

message17: asciiz " Hope that was                            interesting!   "
message18: asciiz "  Lots of Love                              Dad xxx      "


MPTR = $AA

PTIME = $BB
last_message_ticks = $A2

pause:
    sta PTIME
    lda g_ticks
    sta last_message_ticks
keep_waiting:
    sec
    lda g_ticks
    sbc last_message_ticks
    cmp $BB
    bcc keep_waiting
    rts

print_message:
    stx MPTR                    ;lo
    sty MPTR + 1                ;hi
    ldy #0
    jsr lcd.clear_display
print_message_loop:
    lda (MPTR),y
    beq print_message_done
    jsr lcd.putchar
    iny
    jmp print_message_loop
print_message_done:
    rts
