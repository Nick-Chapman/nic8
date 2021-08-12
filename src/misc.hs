

allOps :: [Op]
allOps =
  [ FETCH, LIA, LIB, LIX, LXA, LXB, LXX, SXA
  , JIU, JIZ, JXU, JXZ, JAU
  , ADD, ADDM, SUB, ADX, SUX
  , OUT, TAX, HLT ]

decodeOp :: Byte -> Op
decodeOp =
  \b -> maybe (IMM b) id (Map.lookup b m)
  where
    m = Map.fromList [(encodeOp op, op) | op <- allOps ]
