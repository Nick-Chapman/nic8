
True = 1
False = 0

panic_if_not_in_rom_sub:
    cmp #$80
    bcc .bad
    rts
.bad:
    panic 'OOR'

panic_if_not_in_rom: macro V
    pha
    lda \V + 1
    jsr panic_if_not_in_rom_sub
    pla
endmacro

indirect_NEXT: macro V
    ;panic_if_not_in_rom \V
    NEXT (\V)
endmacro

enter_fp: macro
    load16_0 fp, cp
    indirect_NEXT cp
endmacro

;;; ----------------------------------------------------------------------
;;; push/pull macros

push_word_immediate: macro LIT
    lda #>\LIT
    pha
    lda #<\LIT
    pha
endmacro

;;; ----------------------------------------------------------------------
;;; cons-cell of 16-bit numbers
;;; (we need to know the element's size & that it's not a heap value)
;;;
;;; fp
;;;     .23 .45
;;; [..  i   tail]
cons_cell_i16:
.roots:
    impossible_roots
.evac:
    evacuate 6
.scav
    scavenge_cell_at 4 ; tail
    scavenge_done 6
    byte 'C'
    word .roots, .evac, .scav
.code:
    ;; \n c -> c i tail
    pla ; cL
    sta temp
    pla ; cH
    sta temp + 1
    pla ; nL
    pla ; nH
    ;; setup head/tail
    loadA data_pointer, 3
    pha ; iH
    loadA data_pointer, 2
    pha ; iL
    loadA data_pointer, 5
    pha ; tailH
    loadA data_pointer, 4
    pha ; tailL
    jmp (temp)

nil_cell_i16:
.roots:
    impossible_roots
.evac:
    rts
.scav:
    impossible_scavenge_because_static
.static_closure:
    word .code
    byte 'N'
    word .roots, .evac, .scav
.code:
    pla ; cL
    pla ; cH
    pla ; nL
    sta temp
    pla ; nH
    sta temp + 1
    jmp (temp)

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
.code:
    lda #2
    sta arg2
    stz arg3
    store16i nil_cell_i16.static_closure, arg4
    store16i search.static_closure, fp
    enter_fp

;;; fp    23 45
;;; [..] (i  ps)
search:
    byte 'A'
.roots:
    gc_root_at arg4 ; ps
    rts
.evac:
    rts
.scav:
    impossible_scavenge_because_static
.static_closure:
    word .code
    word .roots, .evac, .scav
.code:
    clc
    lda g_ticks
    adc #1 ; wait a jiffy
    sta arg6
.wait
    lda g_ticks
    sec
    sbc arg6 ; temp in arg6 - the time to continue
    bpl .go
    NEXT .wait
.go
    heap_alloc 6
    save16i_0 search_continue.code, clo
    save16 arg2, clo,2 ; i
    save16 arg4, clo,4 ; ps
    ;; setup args
    ;; i already in 23; ps already in 45
    copy16 clo, arg6 ; k
    store16i candidate.static_closure, fp
    enter_fp

;;; fp            2
;;;    .23 .45
;;; [.. i   ps ] (b) --> if b then (print i; search (i+1, ps)) else search (i+1, cons (i,ps))
search_continue:
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
    lda arg2
    beq .skip_print
;;     bne .skip_print_no
;;     jmp .skip_print
;; .skip_print_no:
    load16 fp,2, arg2 ; i
    print_char ' '
    print_decimal_word arg2 ; i
    ;; alloc cons cell
    heap_alloc 6
    save16i_0 cons_cell_i16.code, clo
    load16 fp,2, arg2 ; i
    load16 fp,4, arg4 ; ps
    save16 arg2, clo,2 ; i
    save16 arg4, clo,4 ; ps
    ;; set arg4-ps to be the newly allocated cons cell
    copy16 clo, arg4 ; cons (i,ps)
    jmp .do_inc
.skip_print:
    load16 fp,2, arg2 ; i
    load16 fp,4, arg4 ; ps
.do_inc:
    inc arg2
    bne .skip_inc_byte2
    inc arg2 + 1
.skip_inc_byte2:
    store16i search.static_closure, fp
    enter_fp


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
.roots:
    gc_root_at arg4 ; ps
    gc_root_at arg6 ; k
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
    ;; match ps nil cons
    push_word_immediate .nil
    push_word_immediate .cons
    copy16 arg4,data_pointer ;ps
    load16_0 data_pointer, cp
    indirect_NEXT cp
.nil:
    copy16 arg6,fp ;k
    lda #True
    sta arg2
    enter_fp ; k True
.cons:
    ;; \p ps' -> let k1 = make_candidate_cont (i,ps,k) ...

    ;; before we alloc, pull ps' off the stack, and put it in arg4
    ;; which is treated as a root here
    pla ; ps'L
    sta arg4
    pla ; ps'H
    sta arg5

    heap_alloc 8
    save16i_0 candidate_cont.code, clo
    save16 arg2, clo,2 ; i
    save16 arg4, clo,4 ; ps'
    save16 arg6, clo,6 ; k
    ;; setup args
    ;; iL,iH already in arg2,arg3
    pla ; pL
    sta arg4
    pla ; pH
    sta arg5
    copy16 clo, arg6
    store16i divides.static_closure, fp
    NEXT divides.code ; divides i p k1

;;; make_candidate_cont :: (Int,List Int,(Bool -> r)) -> Bool -> r
;;; make_candidate_cont (i,ps,k) = \b -> if b then k False else candidate i ps k
;;;
;;; fp                2
;;;    .23 .45 .67
;;; [.. i   ps  k  ] (b)
candidate_cont:
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
    lda arg2 ; b
    bne .bTrue
    ;; candidate i ps k
    load16 fp,2, arg2 ; i
    load16 fp,4, arg4 ; ps
    load16 fp,6, arg6 ; k
    store16i candidate.static_closure, fp
    NEXT candidate.code
.bTrue:
    load16 fp,6, arg3 ; k -> fp (using 3 as a temp)
    copy16 arg3, fp
    lda #False
    sta arg2
    enter_fp ; k False

;;; divides :: Int -> Int -> (Bool -> r) -> r
;;; divides i p k =
;;;   if i == 0 then k True else let i' = i-p in if i' < 0 then k False else divides i' p k
;;;
;;; fp    23 45 67
;;; [..] (i  p  k)
divides:
.roots:
    gc_root_at arg6 ; k
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
    lda arg2 ;iL
    ora arg3 ;iH
    beq .baseT ;i==0
    sec
    lda arg2 ;iL
    sbc arg4 ;pL
    sta arg2 ;i'L (ok to save even if we branch to baseF)
    lda arg3 ;iL
    sbc arg5 ;pL
    bmi .baseF
    sta arg3 ; i'H
    NEXT .code ; divides i' p k
.baseT:
    copy16 arg6,fp ;k
    lda #True
    sta arg2
    enter_fp ; k True
.baseF:
    copy16 arg6,fp ;k
    lda #False
    sta arg2
    enter_fp ; k False
