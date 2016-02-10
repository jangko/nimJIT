template genSingleOperandA(T: typedesc, r: string) =
  for reg in T.low..T.high:
    ctx.lst.write "$1 $2\n" % [inst, $reg]

  ctx.nim.write "for reg in $1.low..$1.high:\n" % [toLower(r)]
  ctx.nim.write "  ctx.$1(TReg(($2.int shl 8) or reg.int))\n" % [niminst, r]
  ctx.nim.write "  ctx.add(\"$1 $$1\" % [$$reg])\n" % [inst]

proc testSingleOperand(ctx: TestContext, inst, niminst: string) =
  genSingleOperandA(reg8, "REG8")
  genSingleOperandA(reg16, "REG16")
  genSingleOperandA(reg32, "REG32")
  genSingleOperandA(reg64, "REG64")

template genSingleOperandB(T: typedesc, r: string) =
  for reg in T.low..T.high:
    ctx.lst.write "$1 $2 [$3]\n" % [inst, size, $reg]
     
  ctx.nim.write "for reg in $1.low..$1.high:\n" % [toLower(r)]
  ctx.nim.write "  ctx.$1(nimjit.$3, TReg(($2.int shl 8) or reg.int))\n" % [niminst, r, size]
  ctx.nim.write "  ctx.add(\"$1 $2 [$$1]\" % [$$reg])\n" % [inst, size]
  
template genSingleOperandBB(T: typedesc, r: string) =
  for base in T.low..T.high:
    for index in T.low..T.high:
      for scale in {1, 2, 4, 8}:
        when T is reg32: 
          if index == ESP: continue
        else: 
          if index == RSP: continue
        ctx.lst.write "$1 $2 [$3 + $4 * $5]\n" % [inst, size, $base, $index, $scale]

  ctx.nim.write "for base in $1.low..$1.high:\n" % [toLower(r)]
  ctx.nim.write "  for index in $1.low..$1.high:\n" % [toLower(r)]
  ctx.nim.write "    for scale in {1, 2, 4, 8}:\n"
  ctx.nim.write "      if index.int == ESP.int: continue\n"
  ctx.nim.write "      ctx.$1(nimjit.$2, reg(base.int, $3), reg(index.int, $3), scale)\n" % [niminst, size, r]
  ctx.nim.write "      ctx.add(\"$1 $2 [$$1 + $$2 * $$3]\" % [$$base, $$index, $$scale])\n" % [inst, size]

proc testSingleOperand(ctx: TestContext, inst: string, niminst, size: string) =
  genSingleOperandB(reg32, "REG32")
  genSingleOperandB(reg64, "REG64")
  genSingleOperandBB(reg32, "REG32")
  genSingleOperandBB(reg64, "REG64")

template genSingleOperandC(T: typedesc, r: string) =
  for reg in T.low..T.high:
    ctx.lst.write "$1 $2 [$3 + $4]\n" % [inst, size, $reg, $disp]

  ctx.nim.write "for reg in $1.low..$1.high:\n" % [toLower(r)]
  ctx.nim.write "  ctx.$1(nimjit.$3, TReg(($2.int shl 8) or reg.int), $4)\n" % [niminst, r, size, $disp]
  ctx.nim.write "  ctx.add(\"$1 $2 [$$1 + $3]\" % [$$reg])\n" % [inst, size, $disp]

template genSingleOperandCC(T: typedesc, r: string) =
  for base in T.low..T.high:
    for index in T.low..T.high:
      for scale in {1, 2, 4, 8}:
        when T is reg32: 
          if index == ESP: continue
        else: 
          if index == RSP: continue
        ctx.lst.write "$1 $2 [$3 + $4 * $5 + $6]\n" % [inst, size, $base, $index, $scale, $disp]

  ctx.nim.write "for base in $1.low..$1.high:\n" % [toLower(r)]
  ctx.nim.write "  for index in $1.low..$1.high:\n" % [toLower(r)]
  ctx.nim.write "    for scale in {1, 2, 4, 8}:\n"
  ctx.nim.write "      if index.int == ESP.int: continue\n"
  ctx.nim.write "      ctx.$1(nimjit.$2, reg(base.int, $3), reg(index.int, $3), scale, $4)\n" % [niminst, size, r, $disp]
  ctx.nim.write "      ctx.add(\"$1 $2 [$$1 + $$2 * $$3 + $3]\" % [$$base, $$index, $$scale])\n" % [inst, size, $disp]

proc testSingleOperand(ctx: TestContext, inst: string, niminst, size: string, disp: int) =
  genSingleOperandC(reg32, "REG32")
  genSingleOperandC(reg64, "REG64")
  genSingleOperandCC(reg32, "REG32")
  genSingleOperandCC(reg64, "REG64")

proc testSingleOpA(ctx: TestContext, inst, niminst: string) =
  ctx.testSingleOperand(inst, niminst)
  ctx.testSingleOperand(inst, niminst, "byte")
  ctx.testSingleOperand(inst, niminst, "word")
  ctx.testSingleOperand(inst, niminst, "dword")
  ctx.testSingleOperand(inst, niminst, "qword")
  ctx.testSingleOperand(inst, niminst, "byte", 0)
  ctx.testSingleOperand(inst, niminst, "word", 0)
  ctx.testSingleOperand(inst, niminst, "dword", 0)
  ctx.testSingleOperand(inst, niminst, "qword", 0)
    
proc testSingleOpB(ctx: TestContext, inst, niminst: string) =
  ctx.testSingleOperand(inst, niminst, "byte", 10)
  ctx.testSingleOperand(inst, niminst, "word", 10)
  ctx.testSingleOperand(inst, niminst, "dword", 10)
  ctx.testSingleOperand(inst, niminst, "qword", 10)
  
  ctx.testSingleOperand(inst, niminst, "byte", 3000)
  ctx.testSingleOperand(inst, niminst, "word", 3000)
  ctx.testSingleOperand(inst, niminst, "dword", 3000)
  ctx.testSingleOperand(inst, niminst, "qword", 3000)

proc genSingleOperand(ctx: TestContext, inst, niminst: string) =
  beginTest(inst):   
    ctx.testSingleOpA(inst, niminst)
    ctx.testSingleOpB(inst, niminst)