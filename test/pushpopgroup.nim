proc testPushPopA(ctx: TestContext, inst, niminst: string) =
  genSingleOperandA(reg16, "REG16")
  if ctx.bits == BITS32: genSingleOperandA(reg32, "REG32")
  if ctx.bits == BITS64: genSingleOperandA(reg64, "REG64")

  if ctx.bits == BITS32:
    ctx.lst.write "$1a\n" % [inst]
    ctx.lst.write "$1ad\n" % [inst]
  
    ctx.nim.write "ctx.$1a()\n" % [niminst]
    ctx.nim.write "ctx.$1ad()\n" % [niminst]
  
    ctx.nim.write "ctx.add(\"$1a\")\n" % [inst]
    ctx.nim.write "ctx.add(\"$1ad\")\n" % [inst]
    
  if inst == "push":
    ctx.lst.write "push byte 10\n"
    ctx.lst.write "push word 1000\n"
    ctx.lst.write "push dword 100000\n"

    ctx.nim.write "ctx.push(10)\n"
    ctx.nim.write "ctx.push(1000)\n"
    ctx.nim.write "ctx.push(100000)\n"
    ctx.nim.write "ctx.add(\"push byte 10\")\n"
    ctx.nim.write "ctx.add(\"push word 1000\")\n"
    ctx.nim.write "ctx.add(\"push dword 100000\")\n"

    if ctx.bits == BITS64:
      ctx.lst.write "push FS\n"
      ctx.lst.write "push GS\n"
      ctx.nim.write "ctx.push(FS)\n"
      ctx.nim.write "ctx.push(GS)\n"
      ctx.nim.write "ctx.add(\"push FS\")\n"
      ctx.nim.write "ctx.add(\"push GS\")\n"
    else:
      ctx.lst.write "push CS\n"
      ctx.lst.write "push SS\n"
      ctx.lst.write "push DS\n"
      ctx.lst.write "push ES\n"
      ctx.lst.write "push FS\n"
      ctx.lst.write "push GS\n"
      ctx.nim.write "ctx.push(CS)\n"
      ctx.nim.write "ctx.push(SS)\n"
      ctx.nim.write "ctx.push(DS)\n"
      ctx.nim.write "ctx.push(ES)\n"
      ctx.nim.write "ctx.push(FS)\n"
      ctx.nim.write "ctx.push(GS)\n"
      ctx.nim.write "ctx.add(\"push CS\")\n"
      ctx.nim.write "ctx.add(\"push SS\")\n"
      ctx.nim.write "ctx.add(\"push DS\")\n"
      ctx.nim.write "ctx.add(\"push ES\")\n"
      ctx.nim.write "ctx.add(\"push FS\")\n"
      ctx.nim.write "ctx.add(\"push GS\")\n"

  if inst == "pop":
    if ctx.bits == BITS64:
      ctx.lst.write "pop FS\n"
      ctx.lst.write "pop GS\n"
      
      ctx.nim.write "ctx.pop(FS)\n"
      ctx.nim.write "ctx.pop(GS)\n"
      
      ctx.nim.write "ctx.add(\"pop FS\")\n"
      ctx.nim.write "ctx.add(\"pop GS\")\n"
    else:
      ctx.lst.write "pop DS\n"
      ctx.lst.write "pop ES\n"
      ctx.lst.write "pop SS\n"
      ctx.lst.write "pop FS\n"
      ctx.lst.write "pop GS\n"
      
      ctx.nim.write "ctx.pop(DS)\n"
      ctx.nim.write "ctx.pop(ES)\n"
      ctx.nim.write "ctx.pop(SS)\n"
      ctx.nim.write "ctx.pop(FS)\n"
      ctx.nim.write "ctx.pop(GS)\n"
      
      ctx.nim.write "ctx.add(\"pop DS\")\n"
      ctx.nim.write "ctx.add(\"pop ES\")\n"
      ctx.nim.write "ctx.add(\"pop SS\")\n"
      ctx.nim.write "ctx.add(\"pop FS\")\n"
      ctx.nim.write "ctx.add(\"pop GS\")\n"
      
proc testPushPopB(ctx: TestContext, inst: string, niminst, size: string) =
  genSingleOperandB(reg32, "REG32")
  if ctx.bits == BITS64:
    genSingleOperandB(reg64, "REG64")
  genSingleOperandBB(reg32, "REG32")
  if ctx.bits == BITS64:
    genSingleOperandBB(reg64, "REG64")

proc genPushPop(ctx: TestContext, inst, niminst: string) =
  if ctx.bits == BITS64:
    beginTest(inst):
      ctx.testPushPopA(inst, niminst)
      ctx.testPushPopB(inst, niminst, "word")
      ctx.testPushPopB(inst, niminst, "qword")
      ctx.testSingleOperand(inst, niminst, "word", 0)
      ctx.testSingleOperand(inst, niminst, "qword", 0)
      ctx.testSingleOperand(inst, niminst, "word", 100)
      ctx.testSingleOperand(inst, niminst, "qword", 100)
      ctx.testSingleOperand(inst, niminst, "word", 3000)
      ctx.testSingleOperand(inst, niminst, "qword", 3000)

  if ctx.bits == BITS32:
    beginTest(inst):
      ctx.testPushPopA(inst, niminst)
      ctx.testPushPopB(inst, niminst, "word")
      ctx.testPushPopB(inst, niminst, "dword")
      ctx.testSingleOperand(inst, niminst, "word", 0)
      ctx.testSingleOperand(inst, niminst, "dword", 0)
      ctx.testSingleOperand(inst, niminst, "word", 100)
      ctx.testSingleOperand(inst, niminst, "dword", 100)
      ctx.testSingleOperand(inst, niminst, "word", 3000)
      ctx.testSingleOperand(inst, niminst, "dword", 3000)