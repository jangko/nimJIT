proc genShiftA(ctx: TestContext, inst, imm, r: string) =
  ctx.nim.write "for reg in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  ctx.$1(TReg(($2.int shl 8) or reg.int), $3)\n" % [inst, r, imm]

proc testShift(ctx: TestContext, inst: string) =
  ctx.genShiftA(inst, "1", "REG8")
  ctx.genShiftA(inst, "1", "REG16")
  ctx.genShiftA(inst, "1", "REG32")
  ctx.genShiftA(inst, "7", "REG8")
  ctx.genShiftA(inst, "7", "REG16")
  ctx.genShiftA(inst, "7", "REG32")
  ctx.genShiftA(inst, "cl", "REG8")
  ctx.genShiftA(inst, "cl", "REG16")
  ctx.genShiftA(inst, "cl", "REG32")

  if ctx.bits == BITS64: ctx.genShiftA(inst, "1", "REG64")
  if ctx.bits == BITS64: ctx.genShiftA(inst, "7", "REG64")
  if ctx.bits == BITS64: ctx.genShiftA(inst, "cl", "REG64")

proc genShiftB(ctx: TestContext, inst, size, imm, r: string) =
  ctx.nim.write "for reg in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  ctx.$1(nimjit.$3, TReg(($2.int shl 8) or reg.int), 0, $4)\n" % [inst, r, size, imm]

proc genShiftBB(ctx: TestContext, inst, size, imm, r: string) =
  ctx.nim.write "for base in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  for index in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "    for scale in {1, 2, 4, 8}:\n"
  ctx.nim.write "      if index.int == ESP.int: continue\n"
  ctx.nim.write "      ctx.$1(nimjit.$2, reg(base.int, $3), reg(index.int, $3), scale, 0, $4)\n" % [inst, size, r, imm]

proc testShift(ctx: TestContext, inst: string, size: string) =
  if ctx.bits == BITS64:
    ctx.genShiftB(inst, size, "1", "REG64")
    ctx.genShiftBB(inst, size, "1", "REG64")
    ctx.genShiftB(inst, size, "7", "REG64")
    ctx.genShiftBB(inst, size, "7", "REG64")
    ctx.genShiftB(inst, size, "cl", "REG64")
    ctx.genShiftBB(inst, size, "cl", "REG64")

  ctx.genShiftB(inst, size, "1", "REG32")
  ctx.genShiftBB(inst, size, "1", "REG32")
  ctx.genShiftB(inst, size, "7", "REG32")
  ctx.genShiftBB(inst, size, "7", "REG32")
  ctx.genShiftB(inst, size, "cl", "REG32")
  ctx.genShiftBB(inst, size, "cl", "REG32")

proc genShiftC(ctx: TestContext, inst: string, size: string, disp: int, imm, r: string) =
  ctx.nim.write "for reg in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  ctx.$1(nimjit.$3, TReg(($2.int shl 8) or reg.int), $4, $5)\n" % [inst, r, size, $disp, imm]

proc genShiftCC(ctx: TestContext, inst: string, size: string, disp: int, imm, r: string) =
  ctx.nim.write "for base in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  for index in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "    for scale in {1, 2, 4, 8}:\n"
  ctx.nim.write "      if index.int == ESP.int: continue\n"
  ctx.nim.write "      ctx.$1(nimjit.$2, reg(base.int, $3), reg(index.int, $3), scale, $4, $5)\n" % [inst, size, r, $disp, imm]

proc testShift(ctx: TestContext, inst: string, size: string, disp: int) =
  if ctx.bits == BITS64:
    ctx.genShiftC(inst, size, disp, "1", "REG64")
    ctx.genShiftCC(inst, size, disp, "1", "REG64")
    ctx.genShiftC(inst, size, disp, "7", "REG64")
    ctx.genShiftCC(inst, size, disp, "7", "REG64")
    ctx.genShiftC(inst, size, disp, "cl", "REG64")
    ctx.genShiftCC(inst, size, disp, "cl", "REG64")

  ctx.genShiftC(inst, size, disp, "1", "REG32")
  ctx.genShiftCC(inst, size, disp, "1", "REG32")
  ctx.genShiftC(inst, size, disp, "7", "REG32")
  ctx.genShiftCC(inst, size, disp, "7", "REG32")
  ctx.genShiftC(inst, size, disp, "cl", "REG32")
  ctx.genShiftCC(inst, size, disp, "cl", "REG32")

proc testShiftA(ctx: TestContext, inst: string) =
  ctx.testShift(inst)
  ctx.testShift(inst, "byte")
  ctx.testShift(inst, "word")
  ctx.testShift(inst, "dword")
  if ctx.bits == BITS64: ctx.testShift(inst, "qword")

  ctx.testShift(inst, "byte", 10)
  ctx.testShift(inst, "word", 10)
  ctx.testShift(inst, "dword", 10)
  if ctx.bits == BITS64: ctx.testShift(inst, "qword", 10)

proc testShiftB(ctx: TestContext, inst: string) =
  ctx.testShift(inst, "byte", 1000)
  ctx.testShift(inst, "word", 1000)
  ctx.testShift(inst, "dword", 1000)
  if ctx.bits == BITS64: ctx.testShift(inst, "qword", 1000)

  ctx.testShift(inst, "byte", 300000)
  ctx.testShift(inst, "word", 300000)
  ctx.testShift(inst, "dword", 300000)
  if ctx.bits == BITS64: ctx.testShift(inst, "qword", 300000)

proc genShiftD(ctx: TestContext, inst: string, size: string, disp: int, imm, r: string) =
  ctx.nim.write "for index in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  for scale in {1, 2, 4, 8}:\n"
  ctx.nim.write "    if index.int == ESP.int: continue\n"
  ctx.nim.write "    ctx.$1(nimjit.$2, reg(index.int, $3), scale, $4, $5)\n" % [inst, size, r, $disp, imm]

proc testShiftD(ctx: TestContext, inst: string, size: string, disp: int) =
  if ctx.bits == BITS64:
    ctx.genShiftD(inst, size, disp, "1", "REG64")
    ctx.genShiftD(inst, size, disp, "7", "REG64")
    ctx.genShiftD(inst, size, disp, "cl", "REG64")

  ctx.genShiftD(inst, size, disp, "1", "REG32")
  ctx.genShiftD(inst, size, disp, "7", "REG32")
  ctx.genShiftD(inst, size, disp, "cl", "REG32")

proc testShiftD(ctx: TestContext, inst: string) =
  ctx.testShiftD(inst, "byte", 10)
  ctx.testShiftD(inst, "word", 10)
  ctx.testShiftD(inst, "dword", 10)
  if ctx.bits == BITS64: ctx.testShiftD(inst, "qword", 10)

  ctx.testShiftD(inst, "byte", 1000)
  ctx.testShiftD(inst, "word", 1000)
  ctx.testShiftD(inst, "dword", 1000)
  if ctx.bits == BITS64: ctx.testShiftD(inst, "qword", 1000)

  ctx.testShiftD(inst, "byte", 300000)
  ctx.testShiftD(inst, "word", 300000)
  ctx.testShiftD(inst, "dword", 300000)
  if ctx.bits == BITS64: ctx.testShiftD(inst, "qword", 300000)

proc genShift(ctx: TestContext, inst: string) =
  beginTest(inst):
    ctx.testShiftA(inst)
    ctx.testShiftB(inst)
    ctx.testShiftD(inst)