

debug: macro C
    print_char \C
    screen_flush_selected
endmacro

debug_hex_word: macro L
    print_hex_word \L
    screen_flush_selected
endmacro

debug_hex_byte: macro L
    print_hex_byte \L
    screen_flush_selected
endmacro

debug_decimal_word: macro L
    print_decimal_word \L
    screen_flush_selected
endmacro

;;; ----------------------------------------------------------------------
;;; extra GC macros

no_evacuate_because_static: macro
    copy_word ev, clo ;identity because static
    rts
endmacro

impossible_scavenge_because_static: macro
    panic 'S'
endmacro

impossible_roots: macro
    panic 'R'
endmacro

;;; ----------------------------------------------------------------------
;;; general array access
copy_word_from_array: macro A, N, L ; A[N] -> L
    ldy #\N
    lda (\A),y
    sta \L
    ldy #(\N + 1)
    lda (\A),y
    sta \L + 1
endmacro

copy_word_from_array0: macro A, L ; A[0] -> L
    lda (\A)
    sta \L
    ldy #1
    lda (\A),y
    sta \L + 1
endmacro

load_byte_from_array: macro A, N
    ldy #\N
    lda (\A),y
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
;;; [. . iL iH tailL tailH]
cons_cell_i16:
    word .roots, .evac, .scav
.code:
    ;debug 'C'
    ;; \n c -> c i tail
    pla ; cL
    sta temp
    pla ; cH
    sta temp + 1
    pla ; nL
    pla ; nH
    ;; setup head/tail
    load_byte_from_array fp, 3
    pha ; iH
    load_byte_from_array fp, 2
    pha ; iL
    load_byte_from_array fp, 5
    pha ; tailH
    load_byte_from_array fp, 4
    pha ; tailL
    jmp (temp)
.roots:
    impossible_roots ; because a cons-cell is not a proper closure; we dont 'enter' it
.evac:
    evacuate 6 ; dispatch code; head-i16; tail
.scav
    scavenge_cell_at 4 ; tail
    scavenge_done 6


nil_cell_i16:
    word .roots, .evac, .scav
.code:
    ;debug 'N'
    pla ; cL
    pla ; cH
    pla ; nL
    sta temp
    pla ; nH
    sta temp + 1
    jmp (temp)
.roots:
    impossible_roots
.evac:
    no_evacuate_because_static
.scav:
    impossible_scavenge_because_static
.static_closure:
    word .code


;;; ----------------------------------------------------------------------
;;; Number args/locals from 1 (because of plan to have fp as 0)


JUMP: macro DEST
    ;; Maybe switch task here
    jmp \DEST
endmacro

enter_fp: macro
    copy_word_from_frame0 cp
    JUMP (cp)
endmacro

True = 1
False = 0

;; primes:
;; .code:
;; .evac:
;; .scav:
;; .static_closure:

;; primes :: IO ()
;; primes = search 2 nil

;; search :: Int -> List Int -> IO ()
;; search i ps = do
;;   candidate i ps $ \b ->
;;     case b of
;;       True -> do
;;         let ps' = cons (i,ps)
;;         print i
;;         search (i+1) ps'
;;       False ->
;;         search (i+1) ps

;;; TODO: search

;;; candidate :: Int -> List Int -> (Bool -> r) -> r
;;; candidate i ps k = do
;;;   let nil = \() -> k True
;;;   let cons = \p ps ->
;;;     let k1 = make_candidate_cont (i,ps,k)
;;;     divides i p k1
;;;   match ps nil cons
;;;
;;; [] iL iH psL psH kL kH
candidate:
    word .roots, .evac, .scav
.code:
    ;debug 'A'

    ;; match ps nil cons
    push_word_immediate .nil
    push_word_immediate .cons

    ;; copy_word_from_array 3, 0, cp
    ;; jmp (cp)

    ;; TODO: need to set fp even for data closures !
    copy_word 3,fp ;ps
    enter_fp ; k True

.nil:
    ;debug 'n'
    copy_word 5,fp ;k
    lda #True
    sta $1
    enter_fp ; k True
.cons:
    ;debug 'c'
    ;; \p ps' -> let k1 = make_candidate_cont (i,ps,k) ...
    ;; allocate continuation
    lda #8
    jsr alloc
    ;; fill in closure
    copy_code_pointer_to_heap0 candidate_cont.code
    copy_word_local_to_heap 1, 2 ; i
    pla ; ps'L
    store_heap 4
    pla ; ps'H
    store_heap 5
    copy_word_local_to_heap 5, 6 ; k
    ;; setup args
    ;; iL,iH already in 1,2
    pla ; pL
    sta 3
    pla ; pH
    sta 4
    copy_word clo, 5
    copy_code_pointer_to_local divides.static_closure, fp
    JUMP divides.code ; divides i p k1
.roots:
    gc_root_at 2 ; ps
    gc_root_at 4 ; k
    rts
.evac:
    no_evacuate_because_static
.scav:
    impossible_scavenge_because_static
.static_closure:
    word .code

;;; make_candidate_cont :: (Int,List Int,(Bool -> r)) -> Bool -> r
;;; make_candidate_cont (i,ps,k) = \b -> if b then k False else candidate i ps k
;;;
;;; [. . iL iH psL psH kL kH] b
candidate_cont:
    word .roots, .evac, .scav
.code:
    ;debug 'B'
    ;debug_hex_byte 1
    lda 1 ; b
    bne .bTrue
    ;; candidate i ps k
    ;debug 'f'
    copy_word_from_frame 2, 1 ; i
    copy_word_from_frame 4, 3 ; ps
    copy_word_from_frame 6, 5 ; k
    copy_code_pointer_to_local candidate.static_closure, fp
    JUMP candidate.code
.bTrue:
    ;debug 't'
    copy_word_from_frame 6, 2 ; k -> fp (using 2 as a temp)
    copy_word 2, fp
    lda #False
    sta $1
    enter_fp ; k False
.roots:
    rts ; no roots
.evac:
    evacuate 8
.scav:
    scavenge_cell_at 4 ; ps
    scavenge_cell_at 6 ; k
    scavenge_done 8


;;; divides :: Int -> Int -> (Bool -> r) -> r
;;; divides i p k =
;;;   if i == 0 then k True else let i' = i-p in if i' < 0 then k False else divides i' p k
;;;
;;; [] iL iH pL pH kL kH
divides:
    word .roots, .evac, .scav
.code:
    ;debug 'D'
    ;debug_decimal_word 1
    ;debug '/'
    ;debug_decimal_word 3
    ;panic '*'

    lda $1 ;iL
    ora $2 ;iH
    beq .baseT ;i==0
    sec
    lda $1 ;iL
    sbc $3 ;pL
    sta $1 ;i'L (ok to save even if we branch to baseF)
    lda $2 ;iL
    sbc $4 ;pL
    bmi .baseF
    sta $2 ; i'H
    JUMP .code ; divides i' p k
.baseT:
    ;debug 't'
    copy_word 5,fp ;k
    lda #True
    sta $1
    enter_fp ; k True
.baseF:
    ;debug 'f'
    copy_word 5,fp ;k
    lda #False
    sta $1
    enter_fp ; k False
.roots:
    gc_root_at 4 ; k
    rts
.evac:
    no_evacuate_because_static
.scav:
    impossible_scavenge_because_static
.static_closure:
    word .code
