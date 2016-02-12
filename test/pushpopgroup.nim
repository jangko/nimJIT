proc testPushPopA(ctx: TestContext, inst: string) =
  ctx.genSingleOperandA(inst, "REG16")
  if ctx.bits == BITS32: ctx.genSingleOperandA(inst, "REG32")
  if ctx.bits == BITS64: ctx.genSingleOperandA(inst, "REG64")

  if ctx.bits == BITS32:
    ctx.nim.write "ctx.$1a()\n" % [inst]
    ctx.nim.write "ctx.$1ad()\n" % [inst]

  if inst == "push":
    ctx.nim.write "ctx.push(10)\n"
    ctx.nim.write "ctx.push(1000)\n"
    ctx.nim.write "ctx.push(100000)\n"

    if ctx.bits == BITS64:
      ctx.nim.write "ctx.push(FS)\n"
      ctx.nim.write "ctx.push(GS)\n"
    else:
      ctx.nim.write "ctx.push(CS)\n"
      ctx.nim.write "ctx.push(SS)\n"
      ctx.nim.write "ctx.push(DS)\n"
      ctx.nim.write "ctx.push(ES)\n"
      ctx.nim.write "ctx.push(FS)\n"
      ctx.nim.write "ctx.push(GS)\n"

  if inst == "pop":
    if ctx.bits == BITS64:
      ctx.nim.write "ctx.pop(FS)\n"
      ctx.nim.write "ctx.pop(GS)\n"
    else:
      ctx.nim.write "ctx.pop(DS)\n"
      ctx.nim.write "ctx.pop(ES)\n"
      ctx.nim.write "ctx.pop(SS)\n"
      ctx.nim.write "ctx.pop(FS)\n"
      ctx.nim.write "ctx.pop(GS)\n"

proc testPushPopB(ctx: TestContext, inst: string, size: string) =
  ctx.genSingleOperandB(inst, size, "REG32")
  if ctx.bits == BITS64:
    ctx.genSingleOperandB(inst, size, "REG64")
  ctx.genSingleOperandBB(inst, size, "REG32")
  if ctx.bits == BITS64:
    ctx.genSingleOperandBB(inst, size, "REG64")

proc genPushPop(ctx: TestContext, inst: string) =
  if ctx.bits == BITS64:
    beginTest(inst):
      ctx.testPushPopA(inst)
      ctx.testPushPopB(inst, "word")
      ctx.testPushPopB(inst, "qword")
      ctx.testSingleOperand(inst, "word", 10)
      ctx.testSingleOperand(inst, "qword", 10)
      ctx.testSingleOperand(inst, "word", 3000)
      ctx.testSingleOperand(inst, "qword", 3000)
      ctx.testSingleOperandD(inst, "word", 10)
      ctx.testSingleOperandD(inst, "qword", 10)
      ctx.testSingleOperandD(inst, "word", 300000)
      ctx.testSingleOperandD(inst, "qword", 300000)

  if ctx.bits == BITS32:
    beginTest(inst):
      ctx.testPushPopA(inst)
      ctx.testPushPopB(inst, "word")
      ctx.testPushPopB(inst, "dword")
      ctx.testSingleOperand(inst, "word", 10)
      ctx.testSingleOperand(inst, "dword", 10)
      ctx.testSingleOperand(inst, "word", 3000)
      ctx.testSingleOperand(inst, "dword", 3000)
      ctx.testSingleOperandD(inst, "word", 10)
      ctx.testSingleOperandD(inst, "dword", 10)
      ctx.testSingleOperandD(inst, "word", 300000)
      ctx.testSingleOperandD(inst, "dword", 300000)
