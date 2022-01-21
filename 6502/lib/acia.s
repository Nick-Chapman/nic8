
acia:

;;; registers
.data = $5000
.status = $5001
.command = $5002
.control = $5003

.ready_to_receive_mask = %00001000 ; bit3

.put_string: ; TODO:move down file
    phx
    tsx
    lda $104,x ; string-ptr-word (under return-address, and saved x)
    sta g_mptr
    lda $105,x
    sta g_mptr + 1
.put_loop:
    lda (g_mptr)
    beq .put_done
    jsr .putchar
    increment16 g_mptr
    jmp .put_loop
.put_done:
    plx
    rts

.init: ; TODO: first sub in file (changes .bins!)
    sta .status ; program reset
    lda #%00001011 ; no parity, no echo, no interrupt
    sta .command
    lda #%00011111 ; 1 stop bit, word length: 8 bits, baud rate: 19200
    sta .control
    ;; Using VIA timer2 to control/limit acia write
    ;; It is already initialize in the mode I need.
    ;; But must start the timer during init, so (after expiration)
    ;; the wait_timer called by the first acia.putchar will complete
    jsr .start_timer
    rts

.putchar:
    jsr .wait_timer
    sta .data
    jsr .start_timer
    rts

.clks_to_wait = (cpu_clks_per_sec / 1000000) * 518 ; determined by experimentation

.start_timer:
    lda #<.clks_to_wait
    sta T2CL
    lda #>.clks_to_wait
    sta T2CH
    rts

.wait_timer:
    pha
    lda #timer2_bit_mask
.wait_loop:
    bit IFR
    beq .wait_loop
    pla
    rts

.read_blocking:
    lda .status
    and #.ready_to_receive_mask
    beq .read_blocking ; 0 means not-full (no byte has arrived)
    lda .data
    rts

;;; TODO: remove unused macros and subs
acia_print_char: macro CHAR
    pha
    lda #\CHAR
    jsr acia.putchar
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
      jsr acia.put_string
      pla
      pla
    pla
endmacro

acia_print_string_variable: macro V
    pha
      lda \V + 1
      pha
      lda \V
      pha
      jsr acia.put_string
      pla
      pla
    pla
endmacro

acia_print_hex_word: macro L ; TODO: dedup with similar (wrapping different putchar)
    lda #'['
    jsr acia.putchar
    lda \L + 1
    jsr acia_put_hex_byte
    lda \L
    jsr acia_put_hex_byte
    lda #']'
    jsr acia.putchar
endmac

acia_print_hex_byte: macro L
    lda #'['
    jsr acia.putchar
    lda \L
    jsr acia_put_hex_byte
    lda #']'
    jsr acia.putchar
endmac

acia_newline: ; TODO: move above macros
    acia_print_string "\n" ; TODO: use .putchar (not macro print_char)
    rts

acia_put_hex_byte: ; TODO: dedup with similar (wrapping different putchar)
    phx
    pha
    lsr
    lsr
    lsr
    lsr
    tax
    lda digits,x
    jsr acia.putchar
    pla
    and #%1111
    tax
    lda digits,x
    jsr acia.putchar
    plx
    rts
