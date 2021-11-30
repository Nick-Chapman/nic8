
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

debug: macro C
    print_char \C
    screen_flush_selected
endmacro

debug_hex_word: macro L
    print_hex_word \L
    screen_flush_selected
endmacro


no_evacuate_because_static: macro
    copy_word ev, clo ;identity because static
    rts
endmacro

impossible_scavenge_because_static: macro
    panic 'S'
endmacro

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

;; candidate :: Int -> List Int -> (Bool -> r) -> r
;; candidate i ps k = do
;;   let kNil = make_kNil (k)
;;   let kCons = make_kCons (i,k)
;;   match ps kNil kCons

;; make_kNil :: (Bool -> r) -> (() -> r)
;; make_kNil (k) = \() -> k True

;; make_kCons ::(Int,Bool -> r) -> Int -> List Int -> r
;; make_kCons (i,k) = \p ps -> do
;;   let k1 = make_candidate_cont (i,ps,k)
;;   divides i p k1

;; make_candidate_cont :: (Int,List Int,(Bool -> r)) -> Bool -> r
;; make_candidate_cont (i,ps,k) = \b -> if b then k False else candidate i ps k

;;; divides :: Int -> Int -> (Bool -> r) -> r
;;; divides i p k =
;;;   if i == 0 then k True else let i' = i-p in if i' < 0 then k False else divides i' p k

;;; [] il ih pl ph kl kh
divides:
    word .roots, .evac, .scav
.code:
    lda $1 ;il
    ora $2 ;ih
    beq .baseT ;i==0
    sec
    lda $1 ;il
    sbc $3 ;pl
    sta $1 ; (ok to save even if we branch to baseF)
    lda $2 ;il
    sbc $4 ;pl
    bmi .baseF
    sta $2
    JUMP .code ; divides i' p k
.baseT:
    copy_word 5,fp ;k
    lda #True
    sta $1
    enter_fp ; k True
.baseF:
    copy_word 5,fp ;k
    lda #False
    sta $1
    enter_fp ; k False
.roots:
    rts ; no roots
.evac:
    no_evacuate_because_static
.scav:
    impossible_scavenge_because_static
.static_closure:
    word .code
