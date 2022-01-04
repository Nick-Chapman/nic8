;;; Use interrupts for ACIA RX

    org $fffa
    word my_nmi
    word main
    word my_irq
    org $8000

;MHz = 1000000

cpu_clks_per_sec = 4 * MHz

    include via.s
    include arith16.s
    include acia.s
    include ticks1.s
    ;include nmi_irq.s
    include lcd.s
    include screen.s
    ;; include decimal.s
    include print.s
    ;include sleep.s
    ;include debug.s

g_acia_buffer_write_ptr = $30
g_acia_buffer_read_ptr = $31

g_ticks = $32
g_selected_screen = $34

g_nmi_count = $35
g_nmi_blocked = $36
g_next_screen_flush = $37

;; g_divisor = $54 ; decimal.s
;; g_mod10 = $56 ; decimal.s
g_mptr = $58 ; print.s / acia_put_string

NUM_SCREENS = 2
g_screen_pointers = $80
g_screens = $200


;;; acia read buffer
g_acia_buffer = $300


;;; copied from nmi_irq.s
my_nmi:
    pha
    lda g_nmi_blocked
    bne .done
    lda #25 ; debounce time
    sta g_nmi_blocked
    inc g_nmi_count
.done:
    pla
    rti

my_irq:
check_acia:
    bit acia.status ; test/ack
    bpl .nope
    jsr handle_acia_interrupt
.nope:
check_T1:
    bit IFR ; test
    bpl .nope
    bit T1CL ; ack
    inc g_ticks

    ;; debound NMI
    pha
    lda g_nmi_blocked
    beq .after
    dec g_nmi_blocked
.after:
    pla

.nope:
    rti

main:
    ldx #$ff
    txs
    cli
    jsr init_via
    jsr init_ticks
    jsr init_nmi_irq
    jsr init_lcd
    jsr lcd_clear_display
    jsr init_screen
    jsr init_acia_buffer
    jmp example


;;; copied from nmi_irq.s
init_nmi_irq: ; TODO: combine with (ticks)init_ticks
    stz g_nmi_blocked
    stz g_nmi_count
    rts

example:
    print_char '>'
    jsr init_acia_using_rx_interrupts
    jsr process_acia_rx.init
.loop:
    jsr screen_flush_when_time
    jsr process_acia_rx.check
    jsr show_acia_buffer_status
    jmp .loop


show_acia_buffer_status
    lda #1
    sta g_selected_screen
    jsr screen_return_home
    print_hex_byte g_acia_buffer_read_ptr
    print_char ' '
    print_hex_byte g_acia_buffer_write_ptr
    rts


process_acia_rx:
.j = 1
.init:
    lda g_ticks
    adc #10
    sta .j ;, x
    rts
.check:
    lda g_ticks
    cmp .j ;, x
    bmi .done
.go:
    jsr is_char_in_acia_buffer
    beq .init
    ;; get char
    ldy g_acia_buffer_read_ptr
    lda g_acia_buffer,y
    inc g_acia_buffer_read_ptr
    ;; send to lcd and acia
    pha
    stz g_selected_screen
    jsr screen_putchar
    pla
    jsr acia_putchar
    jmp .init
.done:
    rts


is_char_in_acia_buffer: ; TODO: inline
    lda g_acia_buffer_read_ptr
    cmp g_acia_buffer_write_ptr
    beq .empty
    lda #1
    rts
.empty:
    lda #0
    rts

init_acia_using_rx_interrupts:
    sta acia.status ; program reset
    ;lda #%00001011 ; no parity, no echo, no interrupts, ready
    lda #%00001001 ; no parity, no echo, no TX interrupt, RX interrupt, ready
    sta acia.command
    lda #%00011111 ; 1 stop bit, word length: 8 bits, baud rate: 19200
    sta acia.control
    ;; Using VIA timer2 to control/limit acia write
    ;; It is already initialize in the mode I need.
    ;; But must start the timer during init, so (after expiration)
    ;; the wait_timer called by the first acia_putchar will complete
    jsr start_timer
    rts

init_acia_buffer:
    stz g_acia_buffer_read_ptr
    stz g_acia_buffer_write_ptr
    rts

handle_acia_interrupt: ; calc number of cycles.. are we quick enough?
    pha
    phy
    lda acia.data
    ldy g_acia_buffer_write_ptr
    sta g_acia_buffer,y
    inc g_acia_buffer_write_ptr
    ;; TODO: deal with write pts catching up read pts?
    ply
    pla
    rts
