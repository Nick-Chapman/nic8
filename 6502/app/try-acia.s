    org $fffa
    word nmi
    word main
    word deprecated_ticks_irq
    org $8000

    include via.s
    include ticks.s
    include lcd.s
    include screen.s
    include print.s
    include sleep.s
    include debug.s

g_ticks = $32
g_selected_screen = $34

g_nmi_count = $35
g_next_screen_flush = $37

g_mptr = $58 ; print.s / acia_put_string

NUM_SCREENS = 2
g_screen_pointers = $80
g_screens = $200

nmi:
    rti

acia_print_string: macro S
    jmp .skip\@
.embedded\@:
    string \S
.skip\@:
    pha
      lda #>.embedded\@
      pha
      lda #<.embedded\@
      pha
      jsr acia_put_string
      pla
      pla
    pla
endmacro

main:
    ldx #$ff
    txs
    jsr init_via
    jsr init_ticks
    jsr init_lcd
    jsr lcd_clear_display
    jsr init_screen
    jsr init_acia
again:
    debug 'a'
    acia_print_string "Hello from nic502. Let's make this message quite a bit longer! Alphabet: abcdefghijklmnopqrstuvwxyz\n"
    lda #50
    jsr sleep_blocking
    jmp again

acia_put_string:
    phx
    tsx
    lda $104,x ; string-pointer-word (under return-address-word, and saved x)
    sta g_mptr
    lda $105,x
    sta g_mptr + 1
    ldy #0
.loop:
    lda (g_mptr),y
    beq .done
    phy
      jsr acia_putchar
    ply
    iny
    jmp .loop
.done:
    plx
    rts

acia:
.data = $5000
.status = $5001
.command = $5002
.control = $5003

init_acia:
    sta acia.status ; program reset
    lda #%00001011 ; no parity, no echo, no interrupt
    sta acia.command
    lda #%00011111 ; 1 stop bit, word length: 8 bits, baud rate: 19200
    sta acia.control
    rts

acia_putchar:
    pha
    jsr acia_blocking_wait_send
    pla
    sta acia.data
    rts

acia_blocking_wait_send:
.ready_to_send_bit = %00010000
.loop:
    lda acia.status
    and #.ready_to_send_bit
    beq .loop ; 0 means not-empty(not-ready)
    rts
