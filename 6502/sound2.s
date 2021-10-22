
    .org $fffc
    .word main_reset
    .word irq

    .org $8000

;;; VIA
PORTB = $6000               ; 7 MSBs for lcd; LSB is 76489 control bit
PORTA = $6001               ; 76489 data
DDRB = $6002
DDRA = $6003

;;; variables in various random slots in page-0 :)

ticks = $71                    ; maintained by irq; +1 every 10ms
    include ticks.s
    include lcd.s
    include 76489.s

bptr = $72 ;2 bytes
goal_ticks = $74
repeat = $75

main_reset:
    lda #%11111111
    sta DDRA
    lda #%11111111
    sta DDRB

    lda #3
    sta repeat

    jsr init_display
    jsr init_timer
    jsr init_sound

restart:
    lda #(data & $ff)       ;lo
    sta bptr
    lda #(data >> 8)        ;hi
    sta bptr+1
    ldy #0
    lda (bptr),y
    clc
    adc ticks
    sta goal_ticks
top_loop:
    jsr send_if_time_to_send
    jmp top_loop

send_if_time_to_send:
    sec
    lda goal_ticks
    sbc ticks
    beq send_bytes              ;hmm?
    rts
send_bytes:
    ldy #1
    lda (bptr),y                 ; read CHAR for message
    jsr message

    ;; bptr points to the delay we just did, at +1 we have #bytes to send
    ldy #2
    lda (bptr),y                 ; read N, the #bytes to send
    beq finish                     ; stop if 0
    tax
continue_send_bytes:
    iny                         ; y=3 (skipping DEL,CHAR,N)
    jsr dot
    ;; x: N, N-1 ... 1
    ;; y: 3, 4 ... N+3
    lda (bptr),y                 ; read data byte to send
    jsr send_sound_data
    dex
    bne continue_send_bytes
    iny                     ; y is now N+3
    ;; read next DEL and increment goal_ticks
    lda (bptr),y
    clc
    adc goal_ticks
    sta goal_ticks
    ;; shift buffer pointer by y (N+3)
    tya
    clc
    adc bptr
    sta bptr
    bcc shift_btr_no_carry
    inc bptr + 1
shift_btr_no_carry:
    rts

message:
    pha
    jsr clear_display
    pla
    jsr print_char
    rts

dot:
    lda #'.'
    jsr print_char
    rts

finish:
    dec repeat
    beq spin
    jmp restart                 ; not stack safe!

spin:
    jmp spin

;;; buffer format: repeats of: DEL, N, b1,b2..bN,
;;; DEL in 1/100s, N is number of following bytes

H = 50 ;1/4s

data:
    .byte (H*0), '-', 4, $f9, $fd, $fb, $ff ;silence c#0, c#1, c#2, c#3
    .byte (H*3), '+', 2, $09, $0d           ;volume-on c#0, c#1
    .byte (H*0), 'c', 2, $e1, $e0           ;c
    .byte (H*0), 'C', 2, $f5, $70           ;low c on c#1
    .byte (H*1), 'd', 2, $51, $60           ;d
    .byte (H*1), 'e', 2, $f1, $a0           ;e
    .byte (H*1), 'f', 2, $91, $a0           ;f
    .byte (H*1), 'g', 2, $01, $a0           ;g
    .byte (H*0), 'G', 2, $f5, $90           ;low g on c#1
    .byte (H*3), '-', 1, $f9                ;silence c#0
    .byte (H*1), '+', 1, $09                ;volume-on c#0
    .byte (H*0), 'g', 2, $01, $a0           ;g
    .byte (H*1), 'f', 2, $91, $a0           ;f
    .byte (H*1), 'e', 2, $f1, $a0           ;e
    .byte (H*1), 'd', 2, $51, $60           ;d
    .byte (H*1), 'c', 2, $e1, $e0           ;c
    .byte (H*0), 'C', 2, $f5, $70           ;low c on c#1
    .byte (H*5), '-', 2, $f9, $fd           ;silence c#0, c#1
    .byte (H*0), '!', 0                     ;FINISH
