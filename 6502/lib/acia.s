
acia_print_char: macro CHAR
    pha
    lda #\CHAR
    jsr acia_putchar
    pla
endmacro

acia_print_string: macro S
    jmp .skip\@
.embedded\@:
    string \S
.skip\@:
    pha
      lda #>.embedded\@
      pha
      lda #<.embedded\@
      pha
      jsr acia_put_string
      pla
      pla
    pla
endmacro

acia_put_string:
    phx
    tsx
    lda $104,x ; string-ptr-word (under return-address, and saved x)
    sta g_mptr
    lda $105,x
    sta g_mptr + 1
.loop:
    lda (g_mptr)
    beq .done
    jsr acia_putchar
    increment16 g_mptr
    jmp .loop
.done:
    plx
    rts

acia:
.data = $5000
.status = $5001
.command = $5002
.control = $5003

init_acia:
    sta acia.status ; program reset
    lda #%00001011 ; no parity, no echo, no interrupt
    sta acia.command
    lda #%00011111 ; 1 stop bit, word length: 8 bits, baud rate: 19200
    sta acia.control
    ;; Using VIA timer2 to control/limit acia write
    ;; It is already initialize in the mode I need.
    ;; But must start the timer during init, so (after expiration)
    ;; the wait_timer called by the first acia_putchar will complete
    jsr start_timer
    rts

acia_putchar:
    jsr wait_timer
    ;;jsr acia_blocking_wait_send
    sta acia.data
    jsr start_timer
    rts


;; acia_blocking_wait_send:
;;     pha
;; .ready_to_send_bit = %00010000 ; bit4
;; .loop:
;;     lda #.ready_to_send_bit
;;     and acia.status
;;     and acia.status
;;     beq .loop ; 0 means not-empty (not-ready)
;;     pla
;;     rts


;;; Use VIA timer to get the delay
;;; try delay of 1ms.. (measured wall of text: just over 2s)
;;; try 1/2 that time.. get wall in just over 1s
;;; 700us... good
;;; 600us... good
;;; 521 good
;;; 519 good
;;; 518 good - switched to this when 517 was seen to be bad

;;; 517 bad - did think this was good for ages, then saw occasional lost char when debuuging
;;; 516 bad - bits corruption
;;; 515 bad - bits corruption
;;; 510 bad - bytes lost
;;; 500 bad - bytes lost
;;; 500 bad - bytes lost
clks_to_wait = (cpu_clks_per_sec / 1000000) * 518

T2CL = $6008
T2CH = $6009
IFR = $600D

timer2_bit_mask = %00100000

start_timer:
    lda #<clks_to_wait
    sta T2CL
    lda #>clks_to_wait
    sta T2CH
    rts

wait_timer:
    pha
    lda #timer2_bit_mask
.loop:
    bit IFR
    beq .loop
    pla
    rts

acia_read_one_byte:
.ready_to_receive_bit = %00001000 ; bit3
.loop:
    lda acia.status
    and #.ready_to_receive_bit
    beq .loop ; 0 means not-full (no byte has arrived)
    lda acia.data
    rts


acia_print_string: macro S
    jmp .skip\@
.embedded\@:
    string \S
.skip\@:
    pha
      lda #>.embedded\@
      pha
      lda #<.embedded\@
      pha
      jsr acia_put_string
      pla
      pla
    pla
endmacro

acia_newline:
    acia_print_string "\n"
    rts

acia_put_hex_byte:
    phx
    pha
    lsr
    lsr
    lsr
    lsr
    tax
    lda digits,x
    jsr acia_putchar
    pla
    and #%1111
    tax
    lda digits,x
    jsr acia_putchar
    plx
    rts

acia_print_hex_word: macro L
    lda #'['
    jsr acia_putchar
    lda \L + 1
    jsr acia_put_hex_byte
    lda \L
    jsr acia_put_hex_byte
    lda #']'
    jsr acia_putchar
endmac

acia_print_hex_byte: macro L
    lda #'['
    jsr acia_putchar
    lda \L
    jsr acia_put_hex_byte
    lda #']'
    jsr acia_putchar
endmac
