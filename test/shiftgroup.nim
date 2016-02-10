template genShiftA(T: typedesc, imm, r: string) =
  for reg in T.low..top[T](ctx):
    ctx.lst.write "$1 $2, $3\n" % [inst, $reg, toUpper(imm)]

  ctx.nim.write "for reg in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  ctx.$1(TReg(($2.int shl 8) or reg.int), $3)\n" % [niminst, r, imm]

proc testShift(ctx: TestContext, inst, niminst: string) =
  genShiftA(reg8, "1", "REG8")
  genShiftA(reg16, "1", "REG16")
  genShiftA(reg32, "1", "REG32")
  if ctx.bits == BITS64: genShiftA(reg64, "1", "REG64")
  genShiftA(reg8, "7", "REG8")
  genShiftA(reg16, "7", "REG16")
  genShiftA(reg32, "7", "REG32")
  if ctx.bits == BITS64: genShiftA(reg64, "7", "REG64")
  genShiftA(reg8, "cl", "REG8")
  genShiftA(reg16, "cl", "REG16")
  genShiftA(reg32, "cl", "REG32")
  if ctx.bits == BITS64: genShiftA(reg64, "cl", "REG64")

template genShiftB(T: typedesc, imm, r: string) =
  for reg in T.low..top[T](ctx):
    ctx.lst.write "$1 $2 [$3], $4\n" % [inst, size, $reg, toUpper(imm)]

  ctx.nim.write "for reg in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  ctx.$1(nimjit.$3, TReg(($2.int shl 8) or reg.int), 0, $4)\n" % [niminst, r, size, imm]

template genShiftBB(T: typedesc, imm, r: string) =
  for base in T.low..top[T](ctx):
    for index in T.low..top[T](ctx):
      for scale in {1, 2, 4, 8}:
        when T is reg32:
          if index == ESP: continue
        else:
          if index == RSP: continue
        ctx.lst.write "$1 $2 [$3 + $4 * $5], $6\n" % [inst, size, $base, $index, $scale, toUpper(imm)]

  ctx.nim.write "for base in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  for index in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "    for scale in {1, 2, 4, 8}:\n"
  ctx.nim.write "      if index.int == ESP.int: continue\n"
  ctx.nim.write "      ctx.$1(nimjit.$2, reg(base.int, $3), reg(index.int, $3), scale, 0, $4)\n" % [niminst, size, r, imm]

proc testShift(ctx: TestContext, inst: string, niminst, size: string) =
  genShiftB(reg32, "1", "REG32")
  if ctx.bits == BITS64: genShiftB(reg64, "1", "REG64")
  genShiftBB(reg32, "1", "REG32")
  if ctx.bits == BITS64: genShiftBB(reg64, "1", "REG64")
  genShiftB(reg32, "7", "REG32")
  if ctx.bits == BITS64: genShiftB(reg64, "7", "REG64")
  genShiftBB(reg32, "7", "REG32")
  if ctx.bits == BITS64: genShiftBB(reg64, "7", "REG64")
  genShiftB(reg32, "cl", "REG32")
  if ctx.bits == BITS64: genShiftB(reg64, "cl", "REG64")
  genShiftBB(reg32, "cl", "REG32")
  if ctx.bits == BITS64: genShiftBB(reg64, "cl", "REG64")

template genShiftC(T: typedesc, imm, r: string) =
  for reg in T.low..top[T](ctx):
    ctx.lst.write "$1 $2 [$3 + $4], $5\n" % [inst, size, $reg, $disp, toUpper(imm)]

  ctx.nim.write "for reg in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  ctx.$1(nimjit.$3, TReg(($2.int shl 8) or reg.int), $4, $5)\n" % [niminst, r, size, $disp, imm]

template genShiftCC(T: typedesc, imm, r: string) =
  for base in T.low..top[T](ctx):
    for index in T.low..top[T](ctx):
      for scale in {1, 2, 4, 8}:
        when T is reg32:
          if index == ESP: continue
        else:
          if index == RSP: continue
        ctx.lst.write "$1 $2 [$3 + $4 * $5 + $6], $7\n" % [inst, size, $base, $index, $scale, $disp, toUpper(imm)]

  ctx.nim.write "for base in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  for index in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "    for scale in {1, 2, 4, 8}:\n"
  ctx.nim.write "      if index.int == ESP.int: continue\n"
  ctx.nim.write "      ctx.$1(nimjit.$2, reg(base.int, $3), reg(index.int, $3), scale, $4, $5)\n" % [niminst, size, r, $disp, imm]

proc testShift(ctx: TestContext, inst: string, niminst, size: string, disp: int) =
  genShiftC(reg32, "1", "REG32")
  if ctx.bits == BITS64: genShiftC(reg64, "1", "REG64")
  genShiftCC(reg32, "1", "REG32")
  if ctx.bits == BITS64: genShiftCC(reg64, "1", "REG64")
  genShiftC(reg32, "7", "REG32")
  if ctx.bits == BITS64: genShiftC(reg64, "7", "REG64")
  genShiftCC(reg32, "7", "REG32")
  if ctx.bits == BITS64: genShiftCC(reg64, "7", "REG64")
  genShiftC(reg32, "cl", "REG32")
  if ctx.bits == BITS64: genShiftC(reg64, "cl", "REG64")
  genShiftCC(reg32, "cl", "REG32")
  if ctx.bits == BITS64: genShiftCC(reg64, "cl", "REG64")

proc testShiftA(ctx: TestContext, inst, niminst: string) =
  ctx.testShift(inst, niminst)
  ctx.testShift(inst, niminst, "byte")
  ctx.testShift(inst, niminst, "word")
  ctx.testShift(inst, niminst, "dword")
  if ctx.bits == BITS64: ctx.testShift(inst, niminst, "qword")

  ctx.testShift(inst, niminst, "byte", 10)
  ctx.testShift(inst, niminst, "word", 10)
  ctx.testShift(inst, niminst, "dword", 10)
  if ctx.bits == BITS64: ctx.testShift(inst, niminst, "qword", 10)

proc testShiftB(ctx: TestContext, inst, niminst: string) =
  ctx.testShift(inst, niminst, "byte", 1000)
  ctx.testShift(inst, niminst, "word", 1000)
  ctx.testShift(inst, niminst, "dword", 1000)
  if ctx.bits == BITS64: ctx.testShift(inst, niminst, "qword", 1000)

  ctx.testShift(inst, niminst, "byte", 300000)
  ctx.testShift(inst, niminst, "word", 300000)
  ctx.testShift(inst, niminst, "dword", 300000)
  if ctx.bits == BITS64: ctx.testShift(inst, niminst, "qword", 300000)

proc genShift(ctx: TestContext, inst, niminst: string) =
  beginTest(inst):
    ctx.testShiftA(inst, niminst)
    ctx.testShiftB(inst, niminst)