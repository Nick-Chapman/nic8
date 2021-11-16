
    .org $fffc
    .word main_reset
    .word ticks_irq

    .org $8000

;;; VIA
PORTB = $6000 ; 7 MSBs for lcd; LSB is 76489 control bit
PORTA = $6001 ; 76489 data
DDRB = $6002
DDRA = $6003

;;; variables in various random slots in page-0 :)

g_ticks = $71 ; maintained by irq; +1 every 10ms
    include ticks.s
    include lcd.s
    include sound.s

bptr = $72 ;2 bytes
g_sleep_ticks = $74
repeat = $75

main_reset:
    lda #%11111111
    sta DDRA
    lda #%11111111
    sta DDRB

    lda #3
    sta repeat

    jsr init_lcd
    jsr init_ticks
    jsr init_sound

restart:
    lda #(data & $ff)       ;lo
    sta bptr
    lda #(data >> 8)        ;hi
    sta bptr+1
    ldy #0
    lda (bptr),y
    clc
    adc g_ticks
    sta g_sleep_ticks
top_loop:
    jsr send_if_time_to_send
    jmp top_loop

send_if_time_to_send:
    sec
    lda g_sleep_ticks
    sbc g_ticks
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
    jsr sound_send_data
    dex
    bne continue_send_bytes
    iny                     ; y is now N+3
    ;; read next DEL and increment g_sleep_ticks
    lda (bptr),y
    clc
    adc g_sleep_ticks
    sta g_sleep_ticks
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
    jsr lcd_clear_display
    pla
    jsr lcd_putchar
    rts

dot:
    lda #'.'
    jsr lcd_putchar
    rts

finish:
    dec repeat
    beq spin
    jmp restart                 ; not stack safe!

spin:
    jmp spin

;;; buffer format: repeats of: DEL, N, b1,b2..bN,
;;; DEL in 1/100s, N is number of following bytes

H = 25 ;1/4s

data:
    .byte (H*0), '-', 4, $9f, $bf, $df, $ff ;silence c#0, c#1, c#2, c#3
    .byte (H*3), '+', 2, $90, $b0           ;volume-on c#0, c#1
    .byte (H*0), 'c', 2, $87, $07           ;c
    .byte (H*0), 'C', 2, $af, $0e           ;low c on c#1
    .byte (H*1), 'd', 2, $8a, $06           ;d
    .byte (H*1), 'e', 2, $8f, $05           ;e
    .byte (H*1), 'f', 2, $89, $05           ;f
    .byte (H*1), 'g', 2, $80, $05           ;g
    .byte (H*0), 'G', 2, $af, $09           ;low g on c#1
    .byte (H*3), '-', 1, $9f                ;silence c#0
    .byte (H*1), '+', 1, $90                ;volume-on c#0
    .byte (H*0), 'g', 2, $80, $05           ;g
    .byte (H*1), 'f', 2, $89, $05           ;f
    .byte (H*1), 'e', 2, $8f, $05           ;e
    .byte (H*1), 'd', 2, $8a, $06           ;d
    .byte (H*1), 'c', 2, $87, $07           ;c
    .byte (H*0), 'C', 2, $af, $0e           ;low c on c#1
    .byte (H*5), '-', 2, $9f, $bf           ;silence c#0, c#1
    .byte (H*0), '!', 0                     ;FINISH
