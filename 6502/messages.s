
;;; Modification of hello program:
;;; use loops more: for sending message chars & the pause
;;; allow the pause to be easily switched between being suitable for 4KHz and 1MHz
;;; ready to explore using the 1MHz clock
;;; which will require we code a wait on the the LCD busy flag
;;; --DONE
;;; have message strings generated on fly from a number

    .org $fffc
    .word reset
    .word 0

;;; VIA
PORTB = $6000                   ; LCD data
PORTA = $6001                   ; LCD control; using 3 bits
DDRB = $6002
DDRA = $6003

    .org $8000

    include lcd.s

reset:
    jsr init_display
    jsr clear_display
    ldx #0
messages_loop:
    lda messages,x
    beq spin
    sta MPTR
    inx
    lda messages,x
    sta MPTR + 1
    inx
    jsr pause
    jsr clear_display
    jsr send_message
    jmp messages_loop
spin:
    jmp spin

MPTR = $AA
send_message:
    ldy #0
send_message_loop:
    lda (MPTR),y
    beq send_message_done
    jsr print_char
    iny
    jmp send_message_loop
send_message_done:
    rts

messages:
    .word message1
    .word message2
    .word message3
    .word message4
    .word message5
    .word message6
    .word message7
    .word message8
    .word message9
    .byte 0

message1: .asciiz "Hello, world!"
message2: .asciiz "This is fun!"
message3: .asciiz "Third message."
message4: .asciiz "4th message."
message5: .asciiz "* This message *                        * over 2 lines *"
message6: .asciiz "ABCDE"
message7: .asciiz " BCD"
message8: .asciiz "  C"
message9: .asciiz "**last message**"

pause:
    ldy #100                    ; 1 second
pause_loop:
    jsr pause_10000             ; for fast clock
    dey
    bne pause_loop
    rts

pause_10000:                    ; .01sec with the 1MHz clock
    pha
    txa
    pha
    ldx #250
pause_10000_loop:
    jsr pause_40
    dex
    bne pause_10000_loop
    pla
    tax
    pla
    rts

pause_40:                       ; 40 clocks .01sec with the 4KHz clock
    nop                         ; #clocks: nop:2, jsr:6, rts:6
    nop                         ; so we need 14 nops: 14*2 + 6 + 6 = 40
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    rts
