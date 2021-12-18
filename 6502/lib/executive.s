
panic_if_not_in_rom_sub:
    cmp #$80
    bcc .bad
    rts
.bad:
    panic 'OOR'

panic_if_not_in_rom: macro V
    pha
    lda \V + 1
    jsr panic_if_not_in_rom_sub
    pla
endmacro

enter_fp: macro
    load16_0 fp, cp
    panic_if_not_in_rom cp
    jsr screen_flush_when_time
    jmp (cp)
endmacro
