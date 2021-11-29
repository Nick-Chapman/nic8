
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
;;;   if i < 0 then k False else if i == 0 then k True else divides (i-p) p k

;;;   if i == 0 then k True else let i' = i-p in if i' < 0 then k False else divides i' p k


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

;;; Number args/locals from 1 (because of plan to have fp as 0)
;;; [] i p kl kh
divides:
    word .roots, .evac, .scav
.code:
    lda $1 ;i --TODO: 16 bits
    beq .baseT
    sec
    sbc $2 ;p
    bmi .baseF
    sta $1
    JUMP .code ; divides i' p k
.baseT:
    copy_word 3,fp ;k
    lda #True
    sta $1
    enter_fp ; k True
.baseF:
    copy_word 3,fp ;k
    lda #False
    sta $1
    enter_fp ; k False
.roots:
    rts ; no roots
.evac:
    copy_word ev, clo ;identity because static
    rts
.scav:
    panic 'S' ;because static
.static_closure:
    word .code
