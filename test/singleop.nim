proc genSingleOperandA(ctx: TestContext, inst, r: string) =
  ctx.nim.write "for reg in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  ctx.$1(TReg(($2.int shl 8) or reg.int))\n" % [inst, r]

proc testSingleOperand(ctx: TestContext, inst: string) =
  ctx.genSingleOperandA(inst, "REG8")
  ctx.genSingleOperandA(inst, "REG16")
  ctx.genSingleOperandA(inst, "REG32")
  if ctx.bits == BITS64: ctx.genSingleOperandA(inst, "REG64")

proc genSingleOperandB(ctx: TestContext, inst, size, r: string) =
  ctx.nim.write "for reg in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  ctx.$1(nimjit.$3, TReg(($2.int shl 8) or reg.int), 0)\n" % [inst, r, size]

proc genSingleOperandBB(ctx: TestContext, inst, size,r: string) =
  ctx.nim.write "for base in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  for index in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "    for scale in {1, 2, 4, 8}:\n"
  ctx.nim.write "      if index.int == ESP.int: continue\n"
  ctx.nim.write "      ctx.$1(nimjit.$2, reg(base.int, $3), reg(index.int, $3), scale, 0)\n" % [inst, size, r]

proc testSingleOperand(ctx: TestContext, inst: string, size: string) =
  ctx.genSingleOperandB(inst, size, "REG32")
  if ctx.bits == BITS64: ctx.genSingleOperandB(inst, size, "REG64")
  ctx.genSingleOperandBB(inst, size, "REG32")
  if ctx.bits == BITS64: ctx.genSingleOperandBB(inst, size, "REG64")

proc genSingleOperandC(ctx: TestContext, inst: string, size: string, disp: int, r: string) =
  ctx.nim.write "for reg in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  ctx.$1(nimjit.$3, TReg(($2.int shl 8) or reg.int), $4)\n" % [inst, r, size, $disp]

proc genSingleOperandCC(ctx: TestContext, inst: string, size: string, disp: int, r: string) =
  ctx.nim.write "for base in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  for index in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "    for scale in {1, 2, 4, 8}:\n"
  ctx.nim.write "      if index.int == ESP.int: continue\n"
  ctx.nim.write "      ctx.$1(nimjit.$2, reg(base.int, $3), reg(index.int, $3), scale, $4)\n" % [inst, size, r, $disp]

proc testSingleOperand(ctx: TestContext, inst: string, size: string, disp: int) =
  ctx.genSingleOperandC(inst, size, disp, "REG32")
  if ctx.bits == BITS64: ctx.genSingleOperandC(inst, size, disp, "REG64")
  ctx.genSingleOperandCC(inst, size, disp, "REG32")
  if ctx.bits == BITS64: ctx.genSingleOperandCC(inst, size, disp, "REG64")

proc testSingleOpA(ctx: TestContext, inst: string) =
  ctx.testSingleOperand(inst)
  ctx.testSingleOperand(inst, "byte")
  ctx.testSingleOperand(inst, "word")
  ctx.testSingleOperand(inst, "dword")
  if ctx.bits == BITS64: ctx.testSingleOperand(inst, "qword")
  ctx.testSingleOperand(inst, "byte", 10)
  ctx.testSingleOperand(inst, "word", 10)
  ctx.testSingleOperand(inst, "dword", 10)
  if ctx.bits == BITS64: ctx.testSingleOperand(inst, "qword", 10)

proc testSingleOpB(ctx: TestContext, inst: string) =
  ctx.testSingleOperand(inst, "byte", 1000)
  ctx.testSingleOperand(inst, "word", 1000)
  ctx.testSingleOperand(inst, "dword", 1000)
  if ctx.bits == BITS64: ctx.testSingleOperand(inst, "qword", 1000)

  ctx.testSingleOperand(inst, "byte", 300000)
  ctx.testSingleOperand(inst, "word", 300000)
  ctx.testSingleOperand(inst, "dword", 300000)
  if ctx.bits == BITS64: ctx.testSingleOperand(inst, "qword", 300000)

proc genSingleOperandD(ctx: TestContext, inst: string, size: string, disp: int, r: string) =
  ctx.nim.write "for index in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  for scale in {1, 2, 4, 8}:\n"
  ctx.nim.write "    if index.int == ESP.int: continue\n"
  ctx.nim.write "    ctx.$1(nimjit.$2, reg(index.int, $3), scale, $4)\n" % [inst, size, r, $disp]

proc testSingleOperandD(ctx: TestContext, inst: string, size: string, disp: int) =
  ctx.genSingleOperandD(inst, size, disp, "REG32")
  if ctx.bits == BITS64: ctx.genSingleOperandD(inst, size, disp, "REG64")

proc testSingleOpD(ctx: TestContext, inst: string) =
  ctx.testSingleOperandD(inst, "byte", 10)
  ctx.testSingleOperandD(inst, "word", 10)
  ctx.testSingleOperandD(inst, "dword", 10)
  if ctx.bits == BITS64: ctx.testSingleOperandD(inst, "qword", 10)

  ctx.testSingleOperandD(inst, "byte", 300000)
  ctx.testSingleOperandD(inst, "word", 300000)
  ctx.testSingleOperandD(inst, "dword", 300000)
  if ctx.bits == BITS64: ctx.testSingleOperandD(inst, "qword", 300000)

proc genSingleOperand(ctx: TestContext, inst: string) =
  beginTest(inst):
    ctx.testSingleOpA(inst)
    ctx.testSingleOpB(inst)
    ctx.testSingleOpD(inst)