;;; REQUIRES: g_ticks, g_sleep_ticks
;;; PROVIDES: sleep_blocking

;;; blocking sleep for N (in accumulator) 1/100s ticks
;;; so max time 2.5secs

sleep_blocking:
    clc
    adc g_ticks
    sta g_sleep_ticks
sleep_wait: ; TODO: how to use local labels?
    sec
    lda g_sleep_ticks
    sbc g_ticks
    bne sleep_wait
    rts
