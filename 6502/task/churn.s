;;; task which does nothing but churn the heap. (for debug)

spawn_churn: macro
    lda #churn.size_locals+1
    jsr tasking.create
    store16i_x churn.static_closure, churn.fp
endmacro

churn:
.fp = 0
.size_locals = 2
.static_closure:
    word .code
    word roots_none, evac_static, scav_impossible
.code:
    ;acia_print_char '.'
    heap_alloc 2
    save16i_0 churned.marker, clo
    yield

churned:
    word roots_impossible, .evac, scav_impossible
.marker:
.evac:
    panic "churned" ; never expect this data to be live
