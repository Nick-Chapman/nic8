
True = 1
False = 0

;;; ----------------------------------------------------------------------
;;; lists of 16-bit numbers: cons/nil
;;; (we need to know the element's size & that it's not a heap value)

cons_cell_i16:
    word .roots, .evac, .scav
.tag:
    byte 1
.roots:
    impossible_roots
.evac:
    evacuate 6
.scav
    scavenge_cell_at 4 ; tail
    scavenge_done 6
    byte 'C'

nil_cell_i16:
    word .roots, .evac, .scav
.tag:
    byte 0
.roots:
    impossible_roots
.evac:
    rts
.scav:
    impossible_scavenge_because_static
.static_closure:
    word .tag
    byte 'N'

;;; ----------------------------------------------------------------------
;;; primes :: IO ()
;;; primes = search 2 nil
;;;
;;; search :: Int -> List Int -> IO ()
;;; search i ps = do
;;;   candidate i ps $ \b ->
;;;     case b of
;;;       True -> do
;;;         let ps' = cons (i,ps)
;;;         print i
;;;         search (i+1) ps'
;;;       False ->
;;;         search (i+1) ps

primes:
.i = 2
.ps = 4
.begin:
    lda #2
    sta .i, x
    stz .i + 1, x
    store16i_x nil_cell_i16.static_closure, .ps
    store16i search.static_closure, fp
    rts

;;; fp    23 45
;;; [..] (i  ps)
search:
.i = 2
.ps = 4
.jiffy = 6
.k = 6 ; called
    byte 'A'
.roots:
    gc_root_at_x .ps
    rts
.evac:
    rts
.scav:
    impossible_scavenge_because_static
.begin: ; will have jump to here; which call enter_fp
    clc
    lda g_ticks
    adc #1 ; wait a jiffy
    sta .jiffy, x
    store16i search.static_closure, fp
    enter_fp
.static_closure:
    word .code
    word .roots, .evac, .scav
.code
    lda g_ticks
    sec
    sbc .jiffy, x
    bpl .go
    enter_fp ; self
.go
    heap_alloc 6
    save16i_0 search_continue.code, clo
    save16_x .i, clo,2
    save16_x .ps, clo,4
    ;; setup args
    ;; i already in 23; ps already in 45
    copyTo16_x clo, .k
    store16i candidate.static_closure, fp
    enter_fp

;;; fp            2
;;;    .23 .45
;;; [.. i   ps ] (b) --> if b then (print i; search (i+1, ps)) else search (i+1, cons (i,ps))
search_continue:
.i = 2  ; called
.ps = 4 ; called
.roots:
    rts
.evac:
    evacuate 6
.scav:
    scavenge_cell_at 4 ; ps
    scavenge_done 6
    byte 'T'
    word .roots, .evac, .scav
.code:
    lda .i, x
    bne .skip_print_no
    load16_x fp,2, .i
    load16_x fp,4, .ps
    jmp .do_inc
.skip_print_no:
    load16_x fp,2, .i
    ;; (1) print to screen, and..
    print_char ' '
    print_decimal_word_x .i
    ;; (2) print to serial link
    acia_print_char ' '
    acia_print_decimal_word_x .i
    ;; alloc cons cell
    heap_alloc 6
    save16i_0 cons_cell_i16.tag, clo
    load16_x fp,2, .i
    load16_x fp,4, .ps
    save16_x .i, clo,2
    save16_x .ps, clo,4
    copyTo16_x clo, .ps ; cons (i,ps)
.do_inc:
    inc .i, x
    bne .skip_inc_byte2
    inc .i + 1, x
.skip_inc_byte2:
    jmp search.begin


;;; ----------------------------------------------------------------------
;;; candidate :: Int -> List Int -> (Bool -> r) -> r
;;; candidate i ps k = do
;;;   let nil = \() -> k True
;;;   let cons = \p ps ->
;;;     let k1 = make_candidate_cont (i,ps,k)
;;;     divides i p k1
;;;   match ps nil cons
;;;
;;; fp    23 45 67
;;; [..] (i  ps k)
candidate:
.i = 2
.p = 4 ; called
.ps = 4
.k = 6
.roots:
    gc_root_at_x .ps
    gc_root_at_x .k
    rts
.evac:
    rts
.scav:
    impossible_scavenge_because_static
.static_closure:
    word .code
    byte 'A'
    word .roots, .evac, .scav
.code:
    copyFrom16_x .ps, temp
    load16 temp,0, cp
    lda (cp)
    beq .nil
    ;; cons case
    heap_alloc 8
    copyFrom16_x .ps, temp
    save16i_0 candidate_cont.code, clo
    save16_x .i, clo,2
    transfer16 temp,4, clo, 4
    save16_x .k, clo,6
    ;; setup args
    ;; i already setup
    load16_x temp,2, .p
    copyTo16_x clo, .k
    store16i divides.static_closure, fp
    enter_fp ; divides i p k1
.nil:
     copyFrom16_x .k, fp
     lda #True
     sta .i, x
     enter_fp ; k True

;;; make_candidate_cont :: (Int,List Int,(Bool -> r)) -> Bool -> r
;;; make_candidate_cont (i,ps,k) = \b -> if b then k False else candidate i ps k
;;;
;;; fp                2
;;;    .23 .45 .67
;;; [.. i   ps  k  ] (b)
candidate_cont:
.b = 2
.i = 2 ; called
.ps = 4 ; called
.k = 6  ; called
.temp = 3
.roots:
    rts ; no roots
.evac:
    evacuate 8
.scav:
    scavenge_cell_at 4 ; ps
    scavenge_cell_at 6 ; k
    scavenge_done 8
    byte 'B'
    word .roots, .evac, .scav
.code:
    lda .b, x
    bne .bTrue
    ;; candidate i ps k
    load16_x fp,.i, .i
    load16_x fp,.ps, .ps
    load16_x fp,.k, .k
    store16i candidate.static_closure, fp
    enter_fp
.bTrue:
    load16_x fp,.k, .temp ; k -> fp (using 3 as a temp)
    copyFrom16_x .temp, fp
    lda #False
    sta .i, x
    enter_fp ; k False

;;; divides :: Int -> Int -> (Bool -> r) -> r
;;; divides i p k =
;;;   if i == 0 then k True else let i' = i-p in if i' < 0 then k False else divides i' p k
;;;
;;; fp    23 45 67
;;; [..] (i  p  k)
divides:
.i = 2
.p = 4
.k = 6
.roots:
    gc_root_at_x .k
    rts
.evac:
    rts
.scav:
    impossible_scavenge_because_static
.static_closure:
    word .code
    byte 'D'
    word .roots, .evac, .scav
.code:
    lda .i, x
    ora .i + 1, x
    beq .baseT ;i==0
    sec
    lda .i, x
    sbc .p, x
    sta .i, x ; (ok to save even if we branch to baseF)
    lda .i + 1, x
    sbc .p + 1, x
    bmi .baseF
    sta .i + 1, x
    enter_fp ; divides i' p k
.baseT:
    copyFrom16_x .k, fp
    lda #True
    sta .i, x
    enter_fp ; k True
.baseF:
    copyFrom16_x .k, fp
    lda #False
    sta .i, x
    enter_fp ; k False
