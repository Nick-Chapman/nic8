
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
    lda $104,x ; string-pointer-word (under return-address-word, and saved x)
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
    rts

acia_putchar:
    pha
    jsr acia_blocking_wait_send
    pla
    sta acia.data
    rts

acia_blocking_wait_send:
.ready_to_send_bit = %00010000 ; bit4
.loop:
    lda acia.status
    and #.ready_to_send_bit
    beq .loop ; 0 means not-empty (not-ready)
    rts


acia_read_one_byte:
.ready_to_receive_bit = %00001000 ; bit3
.loop:
    lda acia.status
    and #.ready_to_receive_bit
    beq .loop ; 0 means not-full (no byte has arrived)
    lda acia.data
    rts
