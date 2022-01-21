
;;; task switching... between exactly 5 tasks ; TODO: allow N tasks!

task1 = $a0
task2 = $b0
task3 = $c0
task4 = $d0
task5 = $e0

yield: macro
    jsr screen.flush_when_time ; TODO: should this be a task like any other?
    jmp (switcher)
endmacro


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

switch_to_1:
    store16i switch_to_2, switcher
    ldx #task1
    load16_0 task1, cp
    panic_if_not_in_rom cp
    jmp (cp)

switch_to_2:
    store16i switch_to_3, switcher
    ldx #task2
    load16_0 task2, cp
    panic_if_not_in_rom cp
    jmp (cp)

switch_to_3:
    store16i switch_to_4, switcher
    ldx #task3
    load16_0 task3, cp
    panic_if_not_in_rom cp
    jmp (cp)

switch_to_4:
    store16i switch_to_5, switcher
    ldx #task4
    load16_0 task4, cp
    panic_if_not_in_rom cp
    jmp (cp)

switch_to_5:
    store16i switch_to_1, switcher
    ldx #task5
    load16_0 task5, cp
    panic_if_not_in_rom cp
    jmp (cp)

find_roots:
    phx
    ldx #task1
    find_roots_from task1
    ldx #task2
    find_roots_from task2
    ldx #task3
    find_roots_from task3
    ldx #task4
    find_roots_from task4
    ldx #task5
    find_roots_from task5

    plx
    rts
