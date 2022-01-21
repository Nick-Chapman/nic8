
;;; null task: do nothing, then yield

null:
.fp = 0
.size_locals = 2
.roots:
    rts ; no roots
.evac:
    rts ; static
.scav:
    impossible_scavenge_because_static
.begin:
    store16i_x .closure, .fp
    rts
.closure:
    word .code
    word .roots, .evac, .scav
.code:
    yield
