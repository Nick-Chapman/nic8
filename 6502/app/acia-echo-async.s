;;; Use interrupts for ACIA RX

    org $fffa
    word nmi
    word main
    word my_irq
    org $8000

    include via.s
    include interrupts.s
    include arith16.s
    include acia.s
    include lcd.s
    include screen.s
    include print.s

g_acia_buffer_write_ptr = $30
g_acia_buffer_read_ptr = $31

g_ticks = $32
g_selected_screen = $34

g_nmi_count = $35
g_nmi_blocked = $36
g_next_screen_flush = $37

;; g_divisor = $54 ; decimal16.s
;; g_mod10 = $56 ; decimal16.s
g_mptr = $58 ; print.s / acia.put_string

NUM_SCREENS = 2
g_screen_pointers = $80
g_screens = $200

;;; acia read buffer
g_acia_buffer = $300

my_irq:
check_acia:
    bit acia.status ; test/ack
    bpl .nope
    jsr handle_acia_interrupt
.nope:
check_T1:
    bit via.IFR ; test
    bpl .nope
    bit via.T1CL ; ack
    inc g_ticks

    ;; debounce NMI
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
    jsr via.init
    jsr init_ticks
    jsr init_nmi_irq
    jsr lcd.init
    jsr lcd.clear_display
    jsr screen.init
    jsr acia_init_buffer
    jmp example


example:
    print_char '>'
    jsr acia_init_using_rx_interrupts
    jsr process_acia_rx.init
.loop:
    jsr screen.flush_when_time
    jsr process_acia_rx.check
    jsr show_acia_buffer_status
    jmp .loop


show_acia_buffer_status
    lda #1
    sta g_selected_screen
    jsr screen.return_home
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
    jsr screen.putchar
    pla
    jsr acia.putchar
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

acia_init_using_rx_interrupts:
    sta acia.status ; program reset
    ;lda #%00001011 ; no parity, no echo, no interrupts, ready
    lda #%00001001 ; no parity, no echo, no TX interrupt, RX interrupt, ready
    sta acia.command
    lda #%00011111 ; 1 stop bit, word length: 8 bits, baud rate: 19200
    sta acia.control
    ;; Using VIA timer2 to control/limit acia write
    ;; It is already initialize in the mode I need.
    ;; But must start the timer during init, so (after expiration)
    ;; the wait_timer called by the first acia.putchar will complete
    jsr acia.start_timer
    rts

acia_init_buffer:
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
