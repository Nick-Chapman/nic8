;;; Play Sonic music found at bitshifters

    org $fffc
    word main_reset
    word deprecated_ticks_irq

    org $8000

;;; bytes
g_ticks = $50
g_song_count = $52

;;; words
g_ptr = $70

    include via.s
    include ticks.s
    include sound.s
    include lcd.s
    include sleep.s

main_reset:

    jsr init_via
    jsr init_ticks
    jsr init_sound
    jsr init_lcd
    jsr lcd_clear_display

    sec
    lda #num_songs_minus_1
    cmp g_song_count            ; on power up may contain any value
    bcs after_reset_song_0      ; dont reset if g_song_count in range
    lda #0
    sta g_song_count            ; cycle/reset to song 0
after_reset_song_0:

    ;; select current song data
    lda g_song_count
    inc g_song_count            ; next song on reset
    asl
    tay
    lda song_table,y
    sta g_ptr
    iny
    lda song_table,y
    sta g_ptr+1
    iny

    ;; show song number(+1) so it matches file name
    lda g_song_count
    jsr print_hex_number
    lda #100 ; 1sec pause
    jsr sleep_blocking

    ;; skip over header
    clc
    ldy #0
    lda (g_ptr),y
    adc #1
    tay
    ;; print title
    jsr lcd_clear_display
print_title:
    iny
    lda (g_ptr),y
    beq author
    jsr lcd_putchar
    jmp print_title
author:
    iny
    tya
    ;; skip over author
    clc
    adc (g_ptr),y
    adc #1
    jsr bump_ptr

play_music_loop:

    lda #2 ;; music packet every 1/50s (but we tick every 1/100)
    jsr sleep_blocking
    jsr send_packet
    jmp play_music_loop


send_packet:
    ;; g_ptr points to the #bytes to send
    ldy #0
    lda (g_ptr),y                 ; read N, the #bytes to send
    cmp #$ff
    beq finish                  ; stop if eof
    tax
    beq send_bytes_done
continue_send_bytes:
    iny                         ; y=1 (skipping N)
    ;; x: N, N-1 ... 1
    ;; y: 1, 2 ... N+1
    lda (g_ptr),y                 ; read data byte to send
    jsr sound_send_data
    dex
    bne continue_send_bytes
send_bytes_done:
    iny                         ; y is now N+1
    tya
    jmp bump_ptr ;tail

;;; bump the ptr by the value in the accumulator
bump_ptr:
    adc g_ptr
    sta g_ptr
    bcc bump_ptr_no_carry
    inc g_ptr + 1
bump_ptr_no_carry:
    rts


finish:
    jsr sound_silence
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

digits: ascii "0123456789abcdef" ; TODO: move to more a general location (print?)

num_songs_minus_1 = (((song_table_end - song_table) >> 1) - 1)

song_table:
    word sonic1
    word sonic2
    word sonic3
    word sonic4
    word sonic5
    word sonic6
    word sonic7
song_table_end

sonic1: incbin sonic1.raw
sonic2: incbin sonic2.raw
sonic3: incbin sonic3.raw
sonic4: incbin sonic4.raw
sonic5: incbin sonic5.raw
sonic6: incbin sonic6.raw
sonic7: incbin sonic7.raw
