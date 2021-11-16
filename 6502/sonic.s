;;; Play Sonic music found at bitshifters

    .org $fffc
    .word main_reset
    .word ticks_irq

    .org $8000

;;; VIA
PORTB = $6000
PORTA = $6001
DDRB = $6002
DDRA = $6003

g_ticks = $71
    include ticks.s
    include sound.s
    include lcd.s

ptr = $72 ;2 bytes
g_sleep_ticks = $74
song_count = $77

main_reset:
    lda #%11111111
    sta DDRA
    lda #%11111111
    sta DDRB

    jsr init_ticks
    jsr init_sound
    jsr silence

    jsr init_lcd
    jsr lcd_clear_display

    sec
    lda #num_songs_minus_1
    cmp song_count            ; on power up may contain any value
    bcs after_reset_song_0    ; dont reset if song_count in range
    lda #0
    sta song_count            ; cycle/reset to song 0
after_reset_song_0:

    ;; select current song data
    lda song_count
    inc song_count              ; next song on reset
    asl
    tay
    lda song_table,y
    sta ptr
    iny
    lda song_table,y
    sta ptr+1
    iny

    ;; show song number(+1) so it matches file name
    lda song_count
    jsr print_hex_number
    lda #100                    ;2secs pause
    jsr sleep

    ;; skip over header
    clc
    ldy #0
    lda (ptr),y
    adc #1
    tay
    ;; print title
    jsr lcd_clear_display
print_title:
    iny
    lda (ptr),y
    beq author
    jsr lcd_putchar
    jmp print_title
author:
    iny
    tya
    ;; skip over author
    clc
    adc (ptr),y
    adc #1
    jsr bump_ptr

play_music_loop:

    lda #2 ;; music packet every 1/50s (but we tick every 1/100)
    jsr sleep
    jsr send_packet
    jmp play_music_loop

sleep1:
    lda #50
    jmp sleep ;tail

;;; sleep for N (in accumulator) 1/50s ticks (so max time 2.5secs)
sleep:
    clc
    adc g_ticks
    sta g_sleep_ticks
sleep_wait:
    sec
    lda g_sleep_ticks
    sbc g_ticks
    bne sleep_wait
    rts

silence:
    lda #(silence_data & $ff)
    sta ptr
    lda #(silence_data >> 8)
    sta ptr+1
    jsr send_packet
    rts
silence_data:
    .byte 4, $9f, $bf, $df, $ff


send_packet:
    ;; ptr points to the #bytes to send
    ldy #0
    lda (ptr),y                 ; read N, the #bytes to send
    cmp #$ff
    beq finish                  ; stop if eof
    tax
    beq send_bytes_done
continue_send_bytes:
    iny                         ; y=1 (skipping N)
    ;; x: N, N-1 ... 1
    ;; y: 1, 2 ... N+1
    lda (ptr),y                 ; read data byte to send
    jsr sound_send_data
    dex
    bne continue_send_bytes
send_bytes_done:
    iny                         ; y is now N+1
    tya
    jmp bump_ptr ;tail

;;; bump the ptr by the value in the accumulator
bump_ptr:
    adc ptr
    sta ptr
    bcc bump_ptr_no_carry
    inc ptr + 1
bump_ptr_no_carry:
    rts


finish:
    jsr silence
spin:
    jmp spin

;;; print byte passed in accumulator as 2 digit hex number
print_hex_number:
    pha
    jsr lcd_clear_display
    pla
    pha
    lsr
    lsr
    lsr
    lsr
    tax
    lda digits,x
    jsr lcd_putchar
    pla
    and #%1111
    tax
    lda digits,x
    jsr lcd_putchar
    rts

digits: .ascii "0123456789abcdef"

num_songs_minus_1 = (((song_table_end - song_table) >> 1) - 1)

song_table:
    .word sonic1
    .word sonic2
    .word sonic3
    .word sonic4
    .word sonic5
    .word sonic6
    .word sonic7
song_table_end

sonic1:
    .incbin sonic-data/sonic1.raw
sonic2:
    .incbin sonic-data/sonic2.raw
sonic3:
    .incbin sonic-data/sonic3.raw
sonic4:
    .incbin sonic-data/sonic4.raw
sonic5:
    .incbin sonic-data/sonic5.raw
sonic6:
    .incbin sonic-data/sonic6.raw
sonic7:
    .incbin sonic-data/sonic7.raw
;;; upto 16!
