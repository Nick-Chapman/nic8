
;;; cyclic executive, allowing task switching

;;; TODO: count the switches, display in a debug screen!

NEXT: macro A ; what is the best name for this? SWITCH?
    jsr screen_flush_when_time
    jmp \A
endmacro
