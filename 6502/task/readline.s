;;; readline support via acia for serial input


;;;----------------------------------------------------------------------
;;; heap strings

; allocated size: 2(code pointer) + 1(size byte) + N(string data) + 1(null)
heap_string:
.size_byte = 2 ; total size of allocated space (4 + string-length)
.data = 3
    word roots_impossible, .evac, .scav
.marker:
.evac:
    loadA ev,.size_byte
    jmp heap.evacuateN
.scav:
    loadA lw,.size_byte
    jmp heap.scavenge_done


initial_message_string:
    word .marker
    byte 0 ; size byte not examined for a static string
    string "readline:"
    byte 0 ; null
    word roots_impossible, evac_static, scav_impossible
.marker:

;;; ----------------------------------------------------------------------
;;; can be shared between different closures..

roots_impossible:
    panic "roots"

roots_none:
    rts

evac_static:
    rts

scav_impossible:
    panic "scav"


;;; ----------------------------------------------------------------------
;;; cons8/nil8 -- list of bytes

nil8:
.static_closure:
    word .tag
    word roots_impossible, evac_static, scav_impossible
.tag:
    byte 0


cons8:
.head = 2
.tail = 3
.size = 5
    word roots_impossible, .evac, .scav
.tag:
    byte 1
.evac:
    evacuate .size ; [ code16, char8, tail-closure-16 ]
.scav
    scavenge_cell_at .tail
    scavenge_done .size


length_char_list:
.list = 0
.tag = 2
.next = 4
    phx
    ldx #0
.loop:
    load16 .list,0, .tag
    lda (.tag)
    beq .done
    inx
    load16 .list,3, .next
    copy16 .next, .list
    jmp .loop
.done
    txa
    plx
    rts


safe_tail_char_list:
.list = 0 ; and return in 0
.tag = 2
.next = 4
    load16 .list,0, .tag
    lda (.tag)
    beq .done
    load16 .list,3, .next
    copy16 .next, .list
.done
    rts


fill_string_from_rev_char_list:
.list = 0
.tag = 2
.next = 4
    ;; size == length+4 is passed in acc
    saveA clo,2 ; fill in size byte
    dec
    tay
    lda #0 ; fill in null at length+3
    sta (clo),y
.loop:
    dey
    ;; y loops down from length+2
    phy
      load16 .list,0, .tag
      lda (.tag)
      beq .done
      loadA .list,cons8.head
    ply
    phy
      sta (clo),y ; fill-in char
      load16 .list,cons8.tail, .next
      copy16 .next, .list
    ply
    jmp .loop
.done:
    ply
    rts

rev_print_char_list:
.list = 0
.tag = 2
.next = 4
    phx
    ldx #16
    lda #0
    pha ; marker for end of chars to print
.loop:
    load16 .list,0, .tag
    lda (.tag)
    beq .done
    dex
    beq .done
    loadA .list,cons8.head ; char to print
    pha ; push char to print
    load16 .list,cons8.tail, .next
    copy16 .next, .list
    jmp .loop
.done:
    cpx #0
    bne .no_elipsis
    print_char '.'
    jmp .after_prompt
.no_elipsis:
    print_char '>'
.after_prompt:
.loop2: ; loop back in reverse over the chars to be printed
    pla
    beq .done2
    jsr screen.putchar
    jmp .loop2
.done2:
    plx
    rts

;;; ----------------------------------------------------------------------
;;; readline...

init_readline: ; global vars
    store16i initial_message_string, g_last_line
    store16i nil8.static_closure, g_current_line_rev_chars ; TODO: better a task var!
    rts

spawn_readline: macro screen
    lda #readline.size_locals+1
    jsr tasking.create
    store8i_x \screen, readline.screen
    store16i_x readline.static_closure, readline.fp
    jsr readline.show
endmacro

readline:
.fp = 0
.screen = 2
.size_locals = 3

.static_closure:
    word .code
    word roots_none, evac_static, scav_impossible
.code:
    ;; is there a new char?
    jsr is_char_in_acia_buffer
    bne .char_arrived
    ;; if not then just yield immediately
    yield

.char_arrived:
    ;; get char
    ldy g_acia_buffer_read_ptr
    lda g_acia_buffer,y
    inc g_acia_buffer_read_ptr
    pha
    jsr acia.putchar ; echo back immediately
    pla
    cmp #13
    beq .newline
    cmp #127
    bne .not_backspace
    jmp .backspace
.not_backspace:
    pha
    ;; for any other char: push it into the holding rev-list
    heap_alloc 5
    save16i_0 cons8.tag, clo
    pla
    saveA clo, cons8.head
    save16 g_current_line_rev_chars, clo, cons8.tail
    copy16 clo, g_current_line_rev_chars
    jsr .show
    yield

.newline:
    copy16 g_current_line_rev_chars, length_char_list.list
    jsr length_char_list
    ;; length now in acc
    ;; we are limited to strings of length 251
    clc
    adc #4 ; +2 for closure ptr; +1 for size byte, +1 for null
    pha ; push size+4
    jsr heap.allocate_may_gc ; allocate space for size+4 bytes
    ;; clo now points to new heap space
    save16i_0 heap_string.marker, clo
    copy16 g_current_line_rev_chars, fill_string_from_rev_char_list.list
    pla ; pass size+4 in acc
    jsr fill_string_from_rev_char_list ; fill in clo
    ;; TODO: append to list of collected lines not yet processed
    copy16 clo, g_last_line ; for now, previous line is just lost here (and will be GCed)
    jsr .echo_last_line ; to acia
    ;; set current line chars back to empty list
    store16i nil8.static_closure, g_current_line_rev_chars
    jsr .show
    yield

.backspace:
    acia_print_string "^H"
    copy16 g_current_line_rev_chars, safe_tail_char_list.list
    jsr safe_tail_char_list
    copy16 safe_tail_char_list.list, g_current_line_rev_chars
    jsr .show
    yield

.show:
    copyFrom8_x .screen, g_selected_screen
    newline
    jsr .print_last_line ; TODO: remove when have command interpreter task
    newline
    copy16 g_current_line_rev_chars, rev_print_char_list.list
    jsr rev_print_char_list
    rts

.print_last_line:
    copy16 g_last_line, temp
    ;; very inefficient way of incrementing the pointer by 3
    increment16 temp
    increment16 temp
    increment16 temp
    print_string_variable temp
    rts


.echo_last_line:
    acia_print_string "run: "
    copy16 g_last_line, temp
    ;; very inefficient way of incrementing the pointer by 3
    increment16 temp
    increment16 temp
    increment16 temp
    acia_print_string_variable temp
    acia_print_char '\n'
    rts

;;; ----------------------------------------------------------------------
;;; copied... for ../app/acia-echo-async.s

irq_checking_acia:
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

is_char_in_acia_buffer:
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
    ply
    pla
    rts
