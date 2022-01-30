
;;; App to try out readline task...

    org $fffa
    word nmi
    word reset_main
    word irq_checking_acia ; TODO: make the default!
    org $8000

    include via.s
    include interrupts.s
    include arith16.s
    include acia.s
    include lcd.s
    include screen.s
    include macs.s
    include decimal16.s
    include decimal24.s
    include print.s
    include panic.s
    include macs.s
    include arith16.s
    include heap.s
    include sound.s
    include tasking.s
    include speed.s
    include readline.s
    include churn.s
    include shell.s

;;; bytes
heap_end_page = $30
g_ticks = $32
gc_screen = $33
g_selected_screen = $34
g_nmi_count = $35
g_nmi_blocked = $36
g_next_screen_flush = $37

g_acia_buffer_write_ptr = $38
g_acia_buffer_read_ptr = $39

;;; words
g_heap_pointer = $40
heap_start = $4e
gc_count = $50
space_switcher = $52
g_divisor = $54 ; decimal16.s
g_mod10 = $56 ; decimal16.s
g_mptr = $58 ; print.s
g_putchar = $5a ; decimal16.s

g_divisor24 = $60 ; decimal24.s
g_modulus24 = $63 ; decimal24.s

g_first_task = $68 ; byte
g_task = $70 ; word

g_current_line_rev_chars = $72 ; list of chars for current line in reverse order
g_last_line = $74 ; last line as a string
g_new_command = $76 ; byte (bool)

NUM_SCREENS = 4
g_screen_pointers = $80
g_screens = $200

;;; acia read buffer
g_acia_buffer = $300


find_roots:
    gc_root_at g_current_line_rev_chars
    gc_root_at g_last_line
    jsr tasking_find_roots
    rts

reset_main:
    ldx #$ff
    txs
    jsr via.init
    jsr init_ticks
    jsr init_nmi_irq
    jsr sound.init

    jsr acia_init_using_rx_interrupts
    jsr acia_init_buffer

    jsr lcd.init
    jsr lcd.clear_display
    jsr screen.init
    jsr tasking.init
    acia_print_string "\n\nRESET...\n"
    init_heap 1; gc_screen

    jsr init_readline ; globals

    spawn_readline 0 ; screen
    spawn_churn
    spawn_shell

    lda #speed.size_locals+1
    jsr tasking.create
    store8i_x 2, speed.screen
    jsr speed.begin ; TODO: make a spawn macro for this

    jmp tasking.start
