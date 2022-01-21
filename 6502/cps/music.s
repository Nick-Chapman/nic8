
music:
.fp = 0
.ptr = 2
.jiffy = 4
.screen = 5
.size_locals = 6
.begin:
    store16i_x .start_closure, .fp
    rts
.data:
    incbin sonic3.raw ; TODO: green hill is #3 - play 1 then other with pauses in between
.roots:
    rts ; no roots
.evac:
    rts ; static
.scav:
    impossible_scavenge_because_static
.start_closure:
    word .start
    word .roots, .evac, .scav
.start:
    lda g_ticks
    sta .jiffy, x
    store16i_x .data, .ptr
    ;; header section
    lda (.ptr, x)
    inc a
    jsr .bump_ptr
    ;; title section
    lda (.ptr, x)
    pha
      lda #1
      jsr .bump_ptr
      copyFrom8_x .screen, g_selected_screen
      newline
      newline
      jsr screen.return_home
      print_string_variable_x .ptr
    pla
    jsr .bump_ptr
    ;; author section
    lda (.ptr, x)
    inc a
    jsr .bump_ptr
    store16i_x .play_closure, .fp
    jmp .send

.play_closure:
    word .play
    word .roots, .evac, .scav
.play:
    lda g_ticks
    sec
    sbc .jiffy, x ; sbc (rather than cmp) so we can see by how many jiffy we missed
    bpl .send
    enter_fp
.send:
    beq .no_print ; if >0 we missed, so print debug to acia to show by how many jiffies we missed
    phx
      tax
      lda screen.digits,x ; TODO: avoid odd nonlocal reference
    plx
    jsr acia.putchar
.no_print:
    lda (.ptr, x) ; #bytes to send
    cmp #$ff
    beq .finish
    tay
    iny
.send_loop:
    lda #1
    jsr .bump_ptr
    dey
    beq .send_done
    lda (.ptr, x)
    jsr sound_send_data
    jmp .send_loop
.send_done:
    lda #2 ; music packet every 1/50s (but we tick every 1/100)
    jsr .set_wait
    enter_fp

.finish:
    jsr sound_silence
    copyFrom8_x .screen, g_selected_screen
    print_string "\nDONE"
    store16i_x .gap_closure, .fp
    lda #100 ; pause one second between replaying the music again
    jsr .set_wait
    enter_fp

.gap_closure:
    word .gap
    word .roots, .evac, .scav
.gap:
    lda g_ticks
    cmp .jiffy, x
    bpl .gap_over
    enter_fp
.gap_over:
    jmp .start

;;; set a non-blocking wait for a number of jiffyspassed in the accumulator
.set_wait:
    clc
    adc .jiffy, x
    sta .jiffy, x
    rts

;;; bump the ptr by the value in the accumulator
.bump_ptr:
    clc
    adc .ptr, x
    sta .ptr, x
    bcc .bump_ptr_no_carry
    inc .ptr + 1, x
.bump_ptr_no_carry:
    rts
