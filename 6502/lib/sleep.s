;;; REQUIRES: g_ticks
;;; PROVIDES: sleep_blocking

;;; blocking sleep for N (in accumulator) 1/100s ticks
;;; so max time 2.5secs

sleep_blocking:
    cmp #0
    beq _done$
    clc
    adc g_ticks
    sec
_wait$:
    cmp g_ticks
    bne _wait$
_done$:
    rts
