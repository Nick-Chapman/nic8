
    org $fffa
    word nmi
    word reset_main
    word irq

    org $8000

    include via.s
    include ticks.s
    include nmi_irq.s
    include lcd.s
    include screen.s
    include decimal.s
    include print.s
    include panic.s
    include macs.s

NEXT: macro A
    jsr screen_flush_when_time
    store16i \A, continue_code
    jmp (switcher)
endmacro

    include uptime.s
    include speed-watch.s

;;; bytes
g_ticks = $32
g_selected_screen = $34
g_nmi_count = $35
g_nmi_blocked = $36
g_next_screen_flush = $37

;;; words
g_divisor = $54 ; decimal.s
g_mod10 = $56 ; decimal.s
g_mptr = $58 ; print.s

;;; First stab to round-robin between two tasks
;;; Local vars must not conflict - have temp hack (base = 10/20)
task1 = $60 ; contains address to continue task1
task2 = $62 ; ditto task2
switcher = $64 ; switches task, either: 1->2 or 2->1
continue_code = $66 ; set by NEXT macro; will be written to task1/task2 vars

NUM_SCREENS = 2
g_screen_pointers = $80
g_screens = $200

task1_screen = 0
task2_screen = 1

reset_main:
    ldx #$ff
    txs
    jsr init_via
    jsr init_ticks
    jsr init_nmi_irq
    jsr init_lcd
    jsr lcd_clear_display
    jsr init_screen
    jsr screen_flush_now ; sets the next(first) time to flush ; TODO: rather not flush now, just set time

    store16i uptime.begin, task1
    store16i speed_watch.begin, task2
    store16i one2two, switcher
    store8i task1_screen, g_selected_screen
    jmp (task1)

one2two:
    copy16 continue_code, task1
    store8i task2_screen, g_selected_screen
    store16i two2one, switcher
    jmp (task2)

two2one:
    copy16 continue_code, task2
    store8i task1_screen, g_selected_screen
    store16i one2two, switcher
    jmp (task1)
