proc testMov(ctx: TestContext, inst: string, imm: BiggestInt) =
  if imm <= 0xFF:
    ctx.testArithA(inst, imm, "REG8", "byte")
    ctx.testArithA(inst, imm, "REG16", "word")
    ctx.testArithA(inst, imm, "REG32", "dword")
    if ctx.bits == BITS64: ctx.testArithA(inst, imm, "REG64", "")
  elif imm > 0xFF and imm <= 0xFFFF:
    ctx.testArithB(inst, imm, "REG16")
    ctx.testArithB(inst, imm, "REG32")
    if ctx.bits == BITS64: ctx.testArithB(inst, imm, "REG64")
  elif imm > 0xFFFF and imm <= 0xFFFFFFFF:
    ctx.testArithB(inst, imm, "REG32")
    if ctx.bits == BITS64: ctx.testArithB(inst, imm, "REG64")
  elif imm > 0xFFFFFFFF:
    if ctx.bits == BITS64: ctx.testArithB(inst, imm, "REG64")
    
proc testMovA(ctx: TestContext, inst: string) =
  ctx.testMov(inst, 10)
  ctx.testMov(inst, 3000)
  ctx.testMov(inst, 300000)
  ctx.testMov(inst, 0xFFFFFFFFFF)
  
proc testSReg(ctx: TestContext, inst, r, rr: string) =
  ctx.nim.write "for op1 in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  for op2 in $1.low..top[$1](ctx):\n" % [toLower(rr)]
  ctx.nim.write "    ctx.$1(reg(op1.int, $2), reg(op2.int, $3))\n" % [inst, r, rr]

proc testSReg(ctx: TestContext, inst: string) =
  ctx.testSReg(inst, "REG16", "REGSEG")
  ctx.testSReg(inst, "REG32", "REGSEG")
  if ctx.bits == BITS64: ctx.testSReg(inst, "REG64", "REGSEG")
  
  ctx.testSReg(inst, "REGSEG", "REG16")
  ctx.testSReg(inst, "REGSEG", "REG32")
  if ctx.bits == BITS64: ctx.testSReg(inst, "REGSEG", "REG64")
  
proc testSRegB(ctx: TestContext, inst, s, r, size: string, disp = 0) =
  ctx.nim.write "for op1 in $1.low..top[$1](ctx):\n" % [toLower(s)]
  ctx.nim.write "  for op2 in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "    ctx.$1(reg(op1.int, $2), reg(op2.int, $3), $4, $5)\n" % [inst, s, r, size, $disp]
  ctx.nim.write "    ctx.$1($2, reg(op2.int, $3), $4, reg(op1.int, $5))\n" % [inst, size, r, $disp, s]

proc testSRegB(ctx: TestContext, inst: string) =
  ctx.testSRegB(inst, "REGSEG", "REG32", "word")
  
  if ctx.bits == BITS64:
    ctx.testSRegB(inst, "REGSEG", "REG64", "word")
    ctx.testSRegB(inst, "REGSEG", "REG32", "qword")
    ctx.testSRegB(inst, "REGSEG", "REG64", "qword")
  
proc testSRegC(ctx: TestContext, inst: string, s, r, size: string, disp = 0) =
  let d = $disp
  ctx.nim.write "for OP2 in $1.low..top[$1](ctx):\n" % [toLower(s)]
  ctx.nim.write "  for base in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "    for index in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "      for scale in {1, 2, 4, 8}:\n"
  ctx.nim.write "        if index.int == ESP.int: continue\n"
  if s == "REG8" and r == "REG64":
    ctx.nim.write "        if OP2 in {AH, CH, DH, BH} and index >= R8: continue\n"
    ctx.nim.write "        if OP2 in {AH, CH, DH, BH} and base >= R8: continue\n"
  elif s == "REG8" and r == "REG32":
    ctx.nim.write "        if OP2 in {AH, CH, DH, BH} and index >= R8D: continue\n"
    ctx.nim.write "        if OP2 in {AH, CH, DH, BH} and base >= R8D: continue\n"
  ctx.nim.write "        ctx.$1(nimjit.$2, reg(base.int, $3), reg(index.int, $3), scale, $4, reg(OP2.int, $5))\n" % [inst, size, r, d, s]
  ctx.nim.write "        ctx.$1(reg(OP2.int, $5), nimjit.$2, reg(base.int, $3), reg(index.int, $3), scale, $4)\n" % [inst, size, r, d, s]
 
proc testSRegC(ctx: TestContext, inst: string, disp = 0) =
  ctx.testSRegC(inst, "REGSEG", "REG32", "word", disp)
  
  if ctx.bits == BITS64:
    ctx.testSRegC(inst, "REGSEG", "REG32", "qword", disp)
    ctx.testSRegC(inst, "REGSEG", "REG64", "word", disp)
    ctx.testSRegC(inst, "REGSEG", "REG64", "qword", disp)
    
proc genMov(ctx: TestContext, inst: string) =
  beginTest(inst):
    ctx.testSReg(inst)
    ctx.testSRegB(inst)
    ctx.testSRegC(inst)
    
  beginTest(inst):
    ctx.testArithRegReg(inst)
    ctx.testMovA(inst)
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