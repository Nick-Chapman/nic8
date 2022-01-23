
;;; Multi tasking support

yield: macro
    jmp tasking.yield
endmacro

find_roots:
    phx
    ldx g_first_task
.loop:
    beq .done
    copyFrom16_x 0, g_task
    jsr .dispatch_roots
    gc_root_at_x 0
    txa
    clc
    adc $ff,x
    tax
    jmp .loop
.done:
    plx
    rts
.dispatch_roots:
    jump_indirect_offset g_task, 6

tasking:

.init:
    stz g_first_task
    ldx #0
    rts

.create:
    sta temp
    lda g_first_task
    sec
    sbc temp
    sta g_first_task
    tax
    lda temp
    sta $ff,x
    rts

.start:
    ldx g_first_task
    beq .no_tasks
    jmp .next

.no_tasks:
    panic 'No Tasks'

.yield:
    jsr screen.flush_when_time
    txa
    clc
    adc $ff,x
    tax
    beq .start
.next:
    copyFrom16_x 0, temp
    load16_0 temp, cp
    cmp #$80
    bcc .not_in_rom
    jmp (cp)

.not_in_rom:
    panic 'OOR'
