
;;; Modification of hello program:
;;; use loops more: for sending message chars & the pause
;;; allow the pause to be easily switched between being suitable for 4KHz and 1MHz
;;; send more than 2 messages; have message strings generated on fly from a number
;;; ready to explore using the 1MHz clock
;;; which will require we code a wait on the the LCD busy flag

    .org $fffc
    .word reset
    .word 0

;;; VIA
PORTB = $6000                   ; LCD data
PORTA = $6001                   ; LCD control; using 3 bits
DDRB = $6002
DDRA = $6003

;;; LCD control bits on port A
E  = %10000000
RW = %01000000                  ; 1-read
RS = %00100000                  ; 1-data

    .org $8000

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
    jsr send_lcd_data
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
message5: .asciiz "Let's make this message much longer that will fit on the screen."
message6: .asciiz "ABCDE"
message7: .asciiz " BCD"
message8: .asciiz "  C"
message9: .asciiz "**last message**"

init_display:
    lda #$ff
    sta DDRB
    sta DDRA
    lda #%00111000              ; function set: 8 bit; 2 lines, 5x8
    jsr send_lcd_command
    lda #%00001100              ; turn on display; no cursor; no blinking
    jsr send_lcd_command
    lda #%00000110              ; entry mode: increment; no shift
    jsr send_lcd_command
    rts

clear_display:
    lda #%00000001
    jsr send_lcd_command
    rts

send_lcd_command:
    sta PORTB
    lda #0
    sta PORTA
    lda #(E)
    sta PORTA
    lda #0
    sta PORTA
    rts

send_lcd_data:
    sta PORTB
    lda #(RS)
    sta PORTA
    lda #(RS | E)
    sta PORTA
    lda #(RS)
    sta PORTA
    rts

pause:
    ldy #25                     ;1/4 second with slow clock
pause_loop:
    jsr pause_40
    dey
    bne pause_loop
    rts

pause_40:                       ; 40 clocks is about .01sec with the 4KHz clock
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
