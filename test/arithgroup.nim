proc testArithA(ctx: TestContext, inst: string, imm: int, r, size: string) =
  ctx.nim.write "for reg in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  ctx.$1(TReg(($2.int shl 8) or reg.int), $3)\n" % [inst, r, $imm]

proc testArithB(ctx: TestContext, inst: string, imm: int, r: string) =
  ctx.nim.write "for reg in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  ctx.$1(TReg(($2.int shl 8) or reg.int), $3)\n" % [inst, r, $imm]

proc testArith(ctx: TestContext, inst: string, imm: int) =
  if imm <= 0xFF:
    ctx.testArithA(inst, imm, "REG8", "byte")
    ctx.testArithA(inst, imm, "REG16", "word")
    ctx.testArithA(inst, imm, "REG32", "dword")
    if ctx.bits == BITS64: ctx.testArithA(inst, imm, "REG64", "")
  elif imm > 0xFF and imm <= 0xFFFF:
    ctx.testArithB(inst, imm, "REG16")
    ctx.testArithB(inst, imm, "REG32")
    if ctx.bits == BITS64: ctx.testArithB(inst, imm, "REG64")
  elif imm > 0xFFFF:
    ctx.testArithB(inst, imm, "REG32")
    if ctx.bits == BITS64: ctx.testArithB(inst, imm, "REG64")

proc testArithC(ctx: TestContext, inst, size: string, disp, imm: int, r: string) =
  ctx.nim.write "for reg in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  ctx.$1(nimjit.$3, TReg(($2.int shl 8) or reg.int), $4, $5)\n" % [inst, r, size, $disp, $imm]

proc testArithCC(ctx: TestContext, inst, size: string, disp, imm: int, r: string) =
  ctx.nim.write "for base in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  for index in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "    for scale in {1, 2, 4, 8}:\n"
  ctx.nim.write "      if index.int == ESP.int: continue\n"
  ctx.nim.write "      ctx.$1(nimjit.$2, reg(base.int, $3), reg(index.int, $3), scale, $4, $5)\n" % [inst, size, r, $disp, $imm]

proc testArithD(ctx: TestContext, inst: string, disp: int, size, r, rr: string) =
  let d = $disp
  ctx.nim.write "for OP2 in $1.low..top[$1](ctx):\n" % [toLower(rr)]
  ctx.nim.write "  for reg in $1.low..top[$1](ctx):\n" % [toLower(r)]
  if rr == "REG8" and r == "REG64":
    ctx.nim.write "    if OP2 in {AH, CH, DH, BH} and reg >= R8: continue\n"
  elif rr == "REG8" and r == "REG32":
    ctx.nim.write "    if OP2 in {AH, CH, DH, BH} and reg >= R8D: continue\n"
  ctx.nim.write "    ctx.$1(nimjit.$2, reg(reg.int, $3), $5, reg(OP2.int, $4))\n" % [inst, size, r, rr, d]

proc testArithDD(ctx: TestContext, inst: string, disp: int, size, r, rr: string) =
  let d = $disp
  ctx.nim.write "for OP2 in $1.low..top[$1](ctx):\n" % [toLower(rr)]
  ctx.nim.write "  for base in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "    for index in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "      for scale in {1, 2, 4, 8}:\n"
  ctx.nim.write "        if index.int == ESP.int: continue\n"
  if rr == "REG8" and r == "REG64":
    ctx.nim.write "        if OP2 in {AH, CH, DH, BH} and index >= R8: continue\n"
    ctx.nim.write "        if OP2 in {AH, CH, DH, BH} and base >= R8: continue\n"
  elif rr == "REG8" and r == "REG32":
    ctx.nim.write "        if OP2 in {AH, CH, DH, BH} and index >= R8D: continue\n"
    ctx.nim.write "        if OP2 in {AH, CH, DH, BH} and base >= R8D: continue\n"
  ctx.nim.write "        ctx.$1(nimjit.$2, reg(base.int, $3), reg(index.int, $3), scale, $4, reg(OP2.int, $5))\n" % [inst, size, r, d, rr]

proc testArith(ctx: TestContext, inst, size: string, disp, imm: int) =
  ctx.testArithC(inst, size, disp, imm, "REG32")
  if ctx.bits == BITS64: ctx.testArithC(inst, size, disp, imm, "REG64")
  ctx.testArithCC(inst, size, disp, imm, "REG32")
  if ctx.bits == BITS64: ctx.testArithCC(inst, size, disp, imm, "REG64")

proc testArithMemRegA(ctx: TestContext, inst: string, disp: int) =
  ctx.testArithD(inst, disp, "byte", "REG32", "REG8")
  ctx.testArithD(inst, disp, "word", "REG32", "REG16")
  ctx.testArithD(inst, disp, "dword", "REG32", "REG32")
  if ctx.bits == BITS64: ctx.testArithD(inst, disp, "qword", "REG32", "REG64")

proc testArithMemRegB(ctx: TestContext, inst: string, disp: int) =
  ctx.testArithD(inst, disp, "byte", "REG64", "REG8")
  ctx.testArithD(inst, disp, "word", "REG64", "REG16")
  ctx.testArithD(inst, disp, "dword", "REG64", "REG32")
  ctx.testArithD(inst, disp, "qword", "REG64", "REG64")

proc testArithMemRegC(ctx: TestContext, inst: string, disp: int) =
  if ctx.bits == BITS64:
    beginTest(inst):
      ctx.testArithDD(inst, disp, "byte", "REG32", "REG8")
      ctx.testArithDD(inst, disp, "word", "REG32", "REG16")
      ctx.testArithDD(inst, disp, "dword", "REG32", "REG32")
      ctx.testArithDD(inst, disp, "qword", "REG32", "REG64")
  else:
    beginTest(inst):
      ctx.testArithDD(inst,disp, "byte", "REG32", "REG8")
      ctx.testArithDD(inst,disp, "word", "REG32", "REG16")
      ctx.testArithDD(inst,disp, "dword", "REG32", "REG32")

proc testArithMemRegD(ctx: TestContext, inst: string, disp: int) =
  beginTest(inst):
    ctx.testArithDD(inst, disp, "byte", "REG64", "REG8")
    ctx.testArithDD(inst, disp, "word", "REG64", "REG16")
    ctx.testArithDD(inst, disp, "dword", "REG64", "REG32")
    ctx.testArithDD(inst, disp, "qword", "REG64", "REG64")

proc testArithA(ctx: TestContext, inst: string) =
  ctx.testArith(inst, 10)
  ctx.testArith(inst, 3000)
  ctx.testArith(inst, 300000)

proc testArithB(ctx: TestContext, inst: string) =
  ctx.testArith(inst, "byte", 10, 10)
  ctx.testArith(inst, "word", 10, 3000)
  ctx.testArith(inst, "dword", 10, 300000)

proc testArithC(ctx: TestContext, inst: string) =
  ctx.testArith(inst, "byte", 400, 10)
  ctx.testArith(inst, "word", 400, 3000)
  ctx.testArith(inst, "dword", 400, 300000)

proc testArithD(ctx: TestContext, inst: string) =
  ctx.testArith(inst, "byte", 7000, 10)
  ctx.testArith(inst, "word", 7000, 3000)
  ctx.testArith(inst, "dword", 7000, 300000)

proc testArithEE(ctx: TestContext, inst: string, disp: int, size, r, rr: string) =
  let d = $disp
  ctx.nim.write "for OP2 in $1.low..top[$1](ctx):\n" % [toLower(rr)]
  ctx.nim.write "  for index in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "    for scale in {1, 2, 4, 8}:\n"
  ctx.nim.write "      if index.int == ESP.int: continue\n"
  if rr == "REG8" and r == "REG64":
    ctx.nim.write "      if OP2 in {AH, CH, DH, BH} and index >= R8: continue\n"
  elif rr == "REG8" and r == "REG32":
    ctx.nim.write "      if OP2 in {AH, CH, DH, BH} and index >= R8D: continue\n"
  ctx.nim.write "      ctx.$1(nimjit.$2, reg(index.int, $3), scale, $4, reg(OP2.int, $5))\n" % [inst, size, r, d, rr]

proc testArithMemRegE(ctx: TestContext, inst: string, disp: int) =
  if ctx.bits == BITS64:
    beginTest(inst):
      ctx.testArithEE(inst, disp, "byte", "REG32", "REG8")
      ctx.testArithEE(inst, disp, "word", "REG32", "REG16")
      ctx.testArithEE(inst, disp, "dword", "REG32", "REG32")
      ctx.testArithEE(inst, disp, "qword", "REG32", "REG64")
  else:
    beginTest(inst):
      ctx.testArithEE(inst,disp, "byte", "REG32", "REG8")
      ctx.testArithEE(inst,disp, "word", "REG32", "REG16")
      ctx.testArithEE(inst,disp, "dword", "REG32", "REG32")

proc testArithMemRegF(ctx: TestContext, inst: string, disp: int) =
  beginTest(inst):
    ctx.testArithEE(inst, disp, "byte", "REG64", "REG8")
    ctx.testArithEE(inst, disp, "word", "REG64", "REG16")
    ctx.testArithEE(inst, disp, "dword", "REG64", "REG32")
    ctx.testArithEE(inst, disp, "qword", "REG64", "REG64")

proc testArithRegReg(ctx: TestContext, inst, r: string) =
  ctx.nim.write "for op1 in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  for op2 in $1.low..top[$1](ctx):\n" % [toLower(r)]

  if r == "REG8":
    ctx.nim.write "    if op1 in {AH, CH, DH, BH} and op2 >= R8B: continue\n"
    ctx.nim.write "    if op2 in {AH, CH, DH, BH} and op1 >= R8B: continue\n"

  ctx.nim.write "    ctx.$1(reg(op1.int, $2), reg(op2.int, $2))\n" % [inst, r]

proc testArithRegReg(ctx: TestContext, inst: string) =
  ctx.testArithRegReg(inst, "REG8")
  ctx.testArithRegReg(inst, "REG16")
  ctx.testArithRegReg(inst, "REG32")
  if ctx.bits == BITS64: ctx.testArithRegReg(inst, "REG64")

proc genArith(ctx: TestContext, inst: string) =
  beginTest(inst):
    ctx.testArithRegReg(inst)
    ctx.testArithA(inst)
    ctx.testArithB(inst)
    ctx.testArithC(inst)
    ctx.testArithD(inst)

  beginTest(inst):
    ctx.testArithMemRegA(inst, 10)
    ctx.testArithMemRegA(inst, 7000)
    ctx.testArithMemRegA(inst, 300000)

  if ctx.bits == BITS64:
    beginTest(inst):
      ctx.testArithMemRegB(inst, 10)
      ctx.testArithMemRegB(inst, 7000)
      ctx.testArithMemRegB(inst, 300000)

  ctx.testArithMemRegC(inst, 10)
  ctx.testArithMemRegC(inst, 7000)
  ctx.testArithMemRegC(inst, 300000)

  if ctx.bits == BITS64:
    ctx.testArithMemRegD(inst, 10)
    ctx.testArithMemRegD(inst, 7000)
    ctx.testArithMemRegD(inst, 300000)

  ctx.testArithMemRegE(inst, 10)
  ctx.testArithMemRegE(inst, 7000)
  ctx.testArithMemRegE(inst, 300000)

  if ctx.bits == BITS64:
    ctx.testArithMemRegF(inst, 10)
    ctx.testArithMemRegF(inst, 7000)
    ctx.testArithMemRegF(inst, 300000)

