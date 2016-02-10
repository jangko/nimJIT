template testArithA(T: typedesc, r, size: string) =
  for reg in T.low..top[T](ctx):
    ctx.lst.write "$1 $2, $3 $4\n" % [inst, $reg, size, $imm]

  ctx.nim.write "for reg in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  ctx.$1(TReg(($2.int shl 8) or reg.int), $3)\n" % [niminst, r, $imm]
  ctx.nim.write "  ctx.add(\"$1 $$1, $2 $3\" % [$$reg])\n" % [inst, size, $imm]

template testArithB(T: typedesc, r: string) =
  for reg in T.low..top[T](ctx):
    ctx.lst.write "$1 $2, $3\n" % [inst, $reg, $imm]

  ctx.nim.write "for reg in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  ctx.$1(TReg(($2.int shl 8) or reg.int), $3)\n" % [niminst, r, $imm]
  ctx.nim.write "  ctx.add(\"$1 $$1, $2\" % [$$reg])\n" % [inst, $imm]

proc testArith(ctx: TestContext, inst, niminst: string, imm: int) =
  if imm <= 0xFF:
    testArithA(reg8, "REG8", "byte")
    testArithA(reg16, "REG16", "word")
    testArithA(reg32, "REG32", "dword")
    if ctx.bits == BITS64: testArithA(reg64, "REG64", "")
  elif imm > 0xFF and imm <= 0xFFFF:
    testArithB(reg16, "REG16")
    testArithB(reg32, "REG32")
    if ctx.bits == BITS64: testArithB(reg64, "REG64")
  elif imm > 0xFFFF:
    testArithB(reg32, "REG32")
    if ctx.bits == BITS64: testArithB(reg64, "REG64")

template testArithC(T: typedesc, r: string) =
  for reg in T.low..top[T](ctx):
    ctx.lst.write "$1 $2 [$3 + $4], $5\n" % [inst, size, $reg, $disp, $imm]

  ctx.nim.write "for reg in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  ctx.$1(nimjit.$3, TReg(($2.int shl 8) or reg.int), $4, $5)\n" % [niminst, r, size, $disp, $imm]
  ctx.nim.write "  ctx.add(\"$1 $2 [$$1 + $3], $4\" % [$$reg])\n" % [inst, size, $disp, $imm]

template testArithCC(T: typedesc, r: string) =
  for base in T.low..top[T](ctx):
    for index in T.low..top[T](ctx):
      for scale in {1, 2, 4, 8}:
        when T is reg32:
          if index == ESP: continue
        else:
          if index == RSP: continue
        ctx.lst.write "$1 $2 [$3 + $4 * $5 + $6], $7\n" % [inst, size, $base, $index, $scale, $disp, $imm]

  ctx.nim.write "for base in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  for index in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "    for scale in {1, 2, 4, 8}:\n"
  ctx.nim.write "      if index.int == ESP.int: continue\n"
  ctx.nim.write "      ctx.$1(nimjit.$2, reg(base.int, $3), reg(index.int, $3), scale, $4, $5)\n" % [niminst, size, r, $disp, $imm]
  ctx.nim.write "      ctx.add(\"$1 $2 [$$1 + $$2 * $$3 + $3], $4\" % [$$base, $$index, $$scale])\n" % [inst, size, $disp, $imm]

template testArithD(T, TT: typedesc, size, r, rr: string) =
  let d = $disp
  for OP2 in TT.low..top[TT](ctx):
    for reg in T.low..top[T](ctx):
      when TT is reg8 and T is reg64:
        if OP2 in {AH, CH, DH, BH} and reg >= R8: continue
      elif TT is reg8 and T is reg32:
        if OP2 in {AH, CH, DH, BH} and reg >= R8D: continue
      ctx.lst.write "$1 $2 [$3 + $4], $5\n" % [inst, size, $reg, d, $OP2]

  ctx.nim.write "for OP2 in $1.low..top[$1](ctx):\n" % [toLower(rr)]
  ctx.nim.write "  for reg in $1.low..top[$1](ctx):\n" % [toLower(r)]
  when TT is reg8 and T is reg64:
    ctx.nim.write "    if OP2 in {AH, CH, DH, BH} and reg >= R8: continue\n"
  elif TT is reg8 and T is reg32:
    ctx.nim.write "    if OP2 in {AH, CH, DH, BH} and reg >= R8D: continue\n"
  ctx.nim.write "    ctx.$1(nimjit.$2, reg(reg.int, $3), $5, reg(OP2.int, $4))\n" % [niminst, size, r, rr, d]
  ctx.nim.write "    ctx.add(\"$1 $2 [$$1 + $3], $$2\" % [$$reg, $$OP2])\n" % [inst, size, d]

template testArithDD(T, TT: typedesc, size, r, rr: string) =
  let d = $disp
  for OP2 in TT.low..top[TT](ctx):
    for base in T.low..top[T](ctx):
      for index in T.low..top[T](ctx):
        for scale in {1, 2, 4, 8}:
          when T is reg32:
            if index == ESP: continue
          else:
            if index == RSP: continue
          let sc = $scale

          when TT is reg8 and T is reg64:
            if OP2 in {AH, CH, DH, BH} and index >= R8: continue
            if OP2 in {AH, CH, DH, BH} and base >= R8: continue
          elif TT is reg8 and T is reg32:
            if OP2 in {AH, CH, DH, BH} and index >= R8D: continue
            if OP2 in {AH, CH, DH, BH} and base >= R8D: continue
          ctx.lst.write "$1 $2 [$3 + $4 * $5 + $6], $7\n" % [inst, size, $base, $index, sc, d, $OP2]

  ctx.nim.write "for OP2 in $1.low..top[$1](ctx):\n" % [toLower(rr)]
  ctx.nim.write "  for base in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "    for index in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "      for scale in {1, 2, 4, 8}:\n"
  ctx.nim.write "        if index.int == ESP.int: continue\n"
  when TT is reg8 and T is reg64:
    ctx.nim.write "        if OP2 in {AH, CH, DH, BH} and index >= R8: continue\n"
    ctx.nim.write "        if OP2 in {AH, CH, DH, BH} and base >= R8: continue\n"
  elif TT is reg8 and T is reg32:
    ctx.nim.write "        if OP2 in {AH, CH, DH, BH} and index >= R8D: continue\n"
    ctx.nim.write "        if OP2 in {AH, CH, DH, BH} and base >= R8D: continue\n"  
  ctx.nim.write "        ctx.$1(nimjit.$2, reg(base.int, $3), reg(index.int, $3), scale, $4, reg(OP2.int, $5))\n" % [niminst, size, r, d, rr]
  ctx.nim.write "        ctx.add(\"$1 $2 [$$1 + $$2 * $$3 + $3], $$4\" % [$$base, $$index, $$scale, $$OP2])\n" % [inst, size, d]

proc testArith(ctx: TestContext, inst, niminst, size: string, disp, imm: int) =
  testArithC(reg32, "REG32")
  if ctx.bits == BITS64: testArithC(reg64, "REG64")
  testArithCC(reg32, "REG32")
  if ctx.bits == BITS64: testArithCC(reg64, "REG64")

proc testArithMemRegA(ctx: TestContext, inst, niminst: string, disp: int) =
  testArithD(reg32, reg8, "byte", "REG32", "REG8")
  testArithD(reg32, reg16, "word", "REG32", "REG16")
  testArithD(reg32, reg32, "dword", "REG32", "REG32")
  if ctx.bits == BITS64: testArithD(reg32, reg64, "qword", "REG32", "REG64")

proc testArithMemRegB(ctx: TestContext, inst, niminst: string, disp: int) =
  testArithD(reg64, reg8, "byte", "REG64", "REG8")
  testArithD(reg64, reg16, "word", "REG64", "REG16")
  testArithD(reg64, reg32, "dword", "REG64", "REG32")
  testArithD(reg64, reg64, "qword", "REG64", "REG64")

proc testArithMemRegC(ctx: TestContext, inst, niminst: string, disp: int) =
  if ctx.bits == BITS64:
    beginTest(inst):
      testArithDD(reg32, reg8, "byte", "REG32", "REG8")
      testArithDD(reg32, reg16, "word", "REG32", "REG16")
      testArithDD(reg32, reg32, "dword", "REG32", "REG32")
      testArithDD(reg32, reg64, "qword", "REG32", "REG64")
  else:
    beginTest(inst):
      testArithDD(reg32, reg8, "byte", "REG32", "REG8")
      testArithDD(reg32, reg16, "word", "REG32", "REG16")
      testArithDD(reg32, reg32, "dword", "REG32", "REG32")
      
proc testArithMemRegD(ctx: TestContext, inst, niminst: string, disp: int) =
  beginTest(inst):
    testArithDD(reg64, reg8, "byte", "REG64", "REG8")
    testArithDD(reg64, reg16, "word", "REG64", "REG16")
    testArithDD(reg64, reg32, "dword", "REG64", "REG32")
    testArithDD(reg64, reg64, "qword", "REG64", "REG64")

proc testArithA(ctx: TestContext, inst, niminst: string) =
  ctx.testArith(inst, niminst, 0)
  ctx.testArith(inst, niminst, 10)
  ctx.testArith(inst, niminst, 3000)
  ctx.testArith(inst, niminst, 300000)

proc testArithB(ctx: TestContext, inst, niminst: string) =
  ctx.testArith(inst, niminst, "byte", 0, 0)
  ctx.testArith(inst, niminst, "byte", 0, 10)
  ctx.testArith(inst, niminst, "word", 0, 3000)
  ctx.testArith(inst, niminst, "dword", 0, 300000)

proc testArithC(ctx: TestContext, inst, niminst: string) =
  ctx.testArith(inst, niminst, "byte", 10, 0)
  ctx.testArith(inst, niminst, "byte", 10, 10)
  ctx.testArith(inst, niminst, "word", 10, 3000)
  ctx.testArith(inst, niminst, "dword", 10, 300000)

proc testArithD(ctx: TestContext, inst, niminst: string) =
  ctx.testArith(inst, niminst, "byte", 7000, 0)
  ctx.testArith(inst, niminst, "byte", 7000, 10)
  ctx.testArith(inst, niminst, "word", 7000, 3000)
  ctx.testArith(inst, niminst, "dword", 7000, 300000)

proc genArith(ctx: TestContext, inst, niminst: string) =
  beginTest(inst):
    ctx.testArithA(inst, niminst)
    ctx.testArithB(inst, niminst)
    ctx.testArithC(inst, niminst)
    ctx.testArithD(inst, niminst)
  
  beginTest(inst):
    ctx.testArithMemRegA(inst, niminst, 0)
    ctx.testArithMemRegA(inst, niminst, 100)
    ctx.testArithMemRegA(inst, niminst, 7000)
    ctx.testArithMemRegA(inst, niminst, 300000)
  
  if ctx.bits == BITS64:
    beginTest(inst):
      ctx.testArithMemRegB(inst, niminst, 0)
      ctx.testArithMemRegB(inst, niminst, 100)
      ctx.testArithMemRegB(inst, niminst, 7000)
      ctx.testArithMemRegB(inst, niminst, 300000)

  ctx.testArithMemRegC(inst, niminst, 0)
  ctx.testArithMemRegC(inst, niminst, 100)
  ctx.testArithMemRegC(inst, niminst, 7000)
  ctx.testArithMemRegC(inst, niminst, 300000)

  if ctx.bits == BITS64:
    ctx.testArithMemRegD(inst, niminst, 0)
    ctx.testArithMemRegD(inst, niminst, 100)
    ctx.testArithMemRegD(inst, niminst, 7000)
    ctx.testArithMemRegD(inst, niminst, 300000)