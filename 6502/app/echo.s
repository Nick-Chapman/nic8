
;;; Prep for porting wozmon.
;;; Get serial port read & flow-control write working.

    org $fffa
    word 0
    word main
    word 0

    org $8000

;;; Use VIA timer for flow control when writing to acia

via_T2CL  = $6008
via_T2CH  = $6009
via_IFR   = $600D

MHz = 1000000

cpu_clks_per_sec = 4 * MHz

clks_to_wait = (cpu_clks_per_sec / 1000000) * 518 ; determined by experimentation

start_timer:
    lda #<clks_to_wait
    sta via_T2CL
    lda #>clks_to_wait
    sta via_T2CH
    rts

via_timer2_bit_mask = %00100000

wait_timer:
    pha
    lda #via_timer2_bit_mask
.wait_loop:
    bit via_IFR
    beq .wait_loop
    pla
    rts

acia_data    = $5000
acia_status  = $5001
acia_command = $5002
acia_control = $5003

acial_ready_to_receive_mask = %00001000 ; bit3

main:
    ;; acia init
    sta acia_status ; program reset
    lda #%00001011 ; no parity, no echo, no interrupt
    sta acia_command
    lda #%00011111 ; 1 stop bit, word length: 8 bits, baud rate: 19200
    sta acia_control
    jsr start_timer

    lda #'\\'
    jsr echo
main_loop:

read_wait:
    ;; read a char over the serial connection
    lda acia_status
    and #acial_ready_to_receive_mask
    beq read_wait ; 0 means not-full (no byte has arrived)
    lda acia_data

    ;; echo back what we just read (prefixed by a '.' to demonstate write flow control)
    pha
    lda #'.'
    jsr echo
    pla
    jsr echo

    jmp main_loop

echo:
    jsr wait_timer
    sta acia_data
    jsr start_timer
    rts
