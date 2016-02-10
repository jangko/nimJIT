import strutils, nimjit, osproc, os, macros, times

type
  TestContext = ref object
    lst, nim: File
    lstName, nimName: string
    testName: string
    counter: int
    startTime: float
    symCounter: int
    bits: string

proc newTest(bits: string): TestContext =
  new(result)
  result.counter = 0
  result.startTime = epochTime()
  result.symCounter = 0
  result.bits = bits

proc setName(ctx: TestContext, testName: string) =
  ctx.testName = testName

proc open(ctx: TestContext) =
  inc ctx.counter
  ctx.lstName = "asm.asm"
  ctx.nimName = "asm.nim"

  if not open(ctx.lst, ctx.lstName, fmWrite):
    echo "cannot open ", ctx.lstName
    quit(-1)

  if not open(ctx.nim, ctx.nimName, fmWrite):
    echo "cannot open ", ctx.nimName
    quit(-1)

  ctx.nim.write "import nimjit, os\n"
  ctx.nim.write "var ctx = newAssembler($1)\n" % [ctx.bits]

proc close(ctx: TestContext) =
  ctx.nim.write "var output: File\n"
  ctx.nim.write "if not open(output, paramStr(1), fmWrite):\n"
  ctx.nim.write "  echo \"cannot open \" & paramStr(1)\n"
  ctx.nim.write "  quit(-1)\n"
  ctx.nim.write "ctx.listing(output)\n"

  ctx.lst.close()
  ctx.nim.close()

  var cmd = "nasm -f win64 -l nasmasm.lst $1" % [ctx.lstName]
  if execShellCmd(cmd) != 0: quit(-1)

  cmd = "nim c -d:release -o:njitasm.exe --verbosity:0 $1" % [ctx.nimName]
  if execShellCmd(cmd) != 0: quit(-1)

  cmd = "njitasm njitasm.lst"
  if execShellCmd(cmd) != 0: quit(-1)

  ctx.lstName = "nasmasm.lst"
  if not open(ctx.lst, ctx.lstName, fmRead):
    echo "cannot open ", ctx.lstName
    quit(-1)

  ctx.nimName = "njitasm.lst"
  if not open(ctx.nim, ctx.nimName, fmRead):
    echo "cannot open ", ctx.nimName
    quit(-1)

  var line1 = newString(100)
  var line2 = newString(100)

  var lineno = 1
  while true:
    if not ctx.lst.readLine(line1): break
    if not ctx.nim.readLine(line2): break
    if line1 != line2:
      echo ctx.lstName, ": ", line1
      echo ctx.nimName, ": ", line2
      quit(-1)

    inc lineno

  ctx.lst.close()
  ctx.nim.close()

  let timePassed = formatFloat(epochTime() - ctx.startTime, ffDecimal, 3)

  echo "$1 $2 OK $3" % [$ctx.counter, ctx.testName, timePassed]

macro beginTest(name: string, n: typed): stmt =
  var glue = "ctx.setName($1)\n" % [$name]
  result = parseStmt(glue)
  for c in n:
    result.add parseExpr("ctx.open()")
    result.add c
    result.add parseExpr("ctx.close()")
  #echo result.toStrLit.strVal

proc newSym(ctx: TestContext): string =
  let s = "jitsym" & $ctx.symCounter
  ctx.nim.write "const $1 = [\n" % [s]
  inc ctx.symCounter
  result = s

template genSingleOperandA(T: typedesc) =
  let sym = ctx.newSym()
  for reg in T.low..T.high:
    let r = toLower($reg)
    let lit = "$1 $2" % [inst, r]
    ctx.lst.write lit & "\n"
    ctx.nim.write "  ($1, \"$2\"),\n" % [r, lit]

  ctx.nim.write "]\n"
  ctx.nim.write "for k in $1:\n" % [sym]
  ctx.nim.write "  ctx.$1(k[0])\n" % [niminst]
  ctx.nim.write "  ctx.add(k[1])\n"

proc testSingleOperand(ctx: TestContext, inst, niminst: string) =
  genSingleOperandA(reg8)
  genSingleOperandA(reg16)
  genSingleOperandA(reg32)
  genSingleOperandA(reg64)

template genSingleOperandB(T: typedesc) =
  let sym = ctx.newSym()
  for reg in T.low..T.high:
    let r = toLower($reg)
    let lit = "$1 $2 [$3]" % [inst, size, r]
    ctx.lst.write lit & "\n"
    ctx.nim.write "  (nimjit.$1, $2, \"$3\"),\n" % [size, r, lit]

  ctx.nim.write "]\n"
  ctx.nim.write "for k in $1:\n" % [sym]
  ctx.nim.write "  ctx.$1(k[0], k[1])\n" % [niminst]
  ctx.nim.write "  ctx.add(k[2])\n"

template genSingleOperandBB(T: typedesc) =
  let sym = ctx.newSym()
  for base in T.low..T.high:
    let bs = toLower($base)
    for index in T.low..T.high:
      let idx = toLower($index)
      for scale in {1, 2, 4, 8}:
        when T is reg32:
          if index == ESP: continue
        else:
          if index == RSP: continue
        let sc = $scale
        let lit = "$1 $2 [$3 + $4 * $5]" % [inst, size, bs, idx, sc]
        ctx.lst.write lit & "\n"
        ctx.nim.write "  (nimjit.$1, $2, $3, $4, \"$5\"),\n" % [size, bs, idx, sc, lit]

  ctx.nim.write "]\n"
  ctx.nim.write "for k in $1:\n" % [sym]
  ctx.nim.write "  ctx.$1(k[0], k[1], k[2], k[3])\n" % [niminst]
  ctx.nim.write "  ctx.add(k[4])\n"

proc testSingleOperand(ctx: TestContext, inst: string, niminst, size: string) =
  genSingleOperandB(reg32)
  genSingleOperandB(reg64)
  genSingleOperandBB(reg32)
  genSingleOperandBB(reg64)

template genSingleOperandC(T: typedesc) =
  let sym = ctx.newSym()
  let d = $disp
  for reg in T.low..T.high:
    let r = toLower($reg)
    let lit = "$1 $2 [$3 + $4]" % [inst, size, r, d]
    ctx.lst.write lit & "\n"
    ctx.nim.write "  (nimjit.$1, $2, $3, \"$4\"),\n" % [size, r, d, lit]

  ctx.nim.write "]\n"
  ctx.nim.write "for k in $1:\n" % [sym]
  ctx.nim.write "  ctx.$1(k[0], k[1], k[2])\n" % [niminst]
  ctx.nim.write "  ctx.add(k[3])\n"

template genSingleOperandCC(T: typedesc) =
  let sym = ctx.newSym()
  let d = $disp
  for base in T.low..T.high:
    let bs = toLower($base)
    for index in T.low..T.high:
      let idx = toLower($index)
      for scale in {1, 2, 4, 8}:
        when T is reg32:
          if index == ESP: continue
        else:
          if index == RSP: continue
        let sc = $scale
        let lit = "$1 $2 [$3 + $4 * $5 + $6]" % [inst, size, bs, idx, sc, d]
        ctx.lst.write lit & "\n"
        ctx.nim.write "  (nimjit.$1, $2, $3, $4, $5, \"$6\"),\n" % [size, bs, idx, sc, d, lit]

  ctx.nim.write "]\n"
  ctx.nim.write "for k in $1:\n" % [sym]
  ctx.nim.write "  ctx.$1(k[0], k[1], k[2], k[3], k[4])\n" % [niminst]
  ctx.nim.write "  ctx.add(k[5])\n"

proc testSingleOperand(ctx: TestContext, inst: string, niminst, size: string, disp: int) =
  genSingleOperandC(reg32)
  genSingleOperandC(reg64)
  genSingleOperandCC(reg32)
  genSingleOperandCC(reg64)

proc genSingleOperand(ctx: TestContext, inst, niminst: string) =
  beginTest(inst):
    ctx.testSingleOperand(inst, niminst)

    ctx.testSingleOperand(inst, niminst, "byte")
    ctx.testSingleOperand(inst, niminst, "word")
    ctx.testSingleOperand(inst, niminst, "dword")
    ctx.testSingleOperand(inst, niminst, "qword")

    ctx.testSingleOperand(inst, niminst, "byte", 0)
    ctx.testSingleOperand(inst, niminst, "word", 0)
    ctx.testSingleOperand(inst, niminst, "dword", 0)
    ctx.testSingleOperand(inst, niminst, "qword", 0)

    ctx.testSingleOperand(inst, niminst, "byte", 10)
    ctx.testSingleOperand(inst, niminst, "word", 10)
    ctx.testSingleOperand(inst, niminst, "dword", 10)
    ctx.testSingleOperand(inst, niminst, "qword", 10)

    ctx.testSingleOperand(inst, niminst, "byte", 3000)
    ctx.testSingleOperand(inst, niminst, "word", 3000)
    ctx.testSingleOperand(inst, niminst, "dword", 3000)
    ctx.testSingleOperand(inst, niminst, "qword", 3000)

template genShiftA(T: typedesc, imm: string) =
  let sym = ctx.newSym()
  for reg in T.low..T.high:
    let r = toLower($reg)
    let lit = "$1 $2, $3" % [inst, r, imm]
    ctx.lst.write lit & "\n"
    ctx.nim.write "  ($1, $2, \"$3\"),\n" % [r, imm, lit]

  ctx.nim.write "]\n"
  ctx.nim.write "for k in $1:\n" % [sym]
  ctx.nim.write "  ctx.$1(k[0], k[1])\n" % [niminst]
  ctx.nim.write "  ctx.add(k[2])\n"

proc testShift(ctx: TestContext, inst, niminst: string) =
  genShiftA(reg8, "1")
  genShiftA(reg16, "1")
  genShiftA(reg32, "1")
  genShiftA(reg64, "1")
  genShiftA(reg8, "7")
  genShiftA(reg16, "7")
  genShiftA(reg32, "7")
  genShiftA(reg64, "7")
  genShiftA(reg8, "cl")
  genShiftA(reg16, "cl")
  genShiftA(reg32, "cl")
  genShiftA(reg64, "cl")

template genShiftB(T: typedesc, imm: string) =
  let sym = ctx.newSym()
  for reg in T.low..T.high:
    let r = toLower($reg)
    let lit = "$1 $2 [$3], $4" % [inst, size, r, imm]
    ctx.lst.write lit & "\n"
    ctx.nim.write "  (nimjit.$1, $2, $3, \"$4\"),\n" % [size, r, imm, lit]

  ctx.nim.write "]\n"
  ctx.nim.write "for k in $1:\n" % [sym]
  ctx.nim.write "  ctx.$1(k[0], k[1], 0, k[2])\n" % [niminst]
  ctx.nim.write "  ctx.add(k[3])\n"

template genShiftBB(T: typedesc, imm: string) =
  let sym = ctx.newSym()
  for base in T.low..T.high:
    let bs = toLower($base)
    for index in T.low..T.high:
      let idx = toLower($index)
      for scale in {1, 2, 4, 8}:
        when T is reg32:
          if index == ESP: continue
        else:
          if index == RSP: continue
        let sc = $scale
        let lit = "$1 $2 [$3 + $4 * $5], $6" % [inst, size, bs, idx, sc, imm]
        ctx.lst.write lit & "\n"
        ctx.nim.write "  (nimjit.$1, $2, $3, $4, $5, \"$6\"),\n" % [size, bs, idx, sc, imm, lit]

  ctx.nim.write "]\n"
  ctx.nim.write "for k in $1:\n" % [sym]
  ctx.nim.write "  ctx.$1(k[0], k[1], k[2], k[3], 0, k[4])\n" % [niminst]
  ctx.nim.write "  ctx.add(k[5])\n"

proc testShift(ctx: TestContext, inst: string, niminst, size: string) =
  genShiftB(reg32, "1")
  genShiftB(reg64, "1")
  genShiftBB(reg32, "1")
  genShiftBB(reg64, "1")
  genShiftB(reg32, "7")
  genShiftB(reg64, "7")
  genShiftBB(reg32, "7")
  genShiftBB(reg64, "7")
  genShiftB(reg32, "cl")
  genShiftB(reg64, "cl")
  genShiftBB(reg32, "cl")
  genShiftBB(reg64, "cl")

template genShiftC(T: typedesc, imm: string) =
  let sym = ctx.newSym()
  let d = $disp
  for reg in T.low..T.high:
    let r = toLower($reg)
    let lit = "$1 $2 [$3 + $4], $5" % [inst, size, r, d, imm]
    ctx.lst.write lit & "\n"
    ctx.nim.write "  (nimjit.$1, $2, $3, $4, \"$5\"),\n" % [size, r, d, imm, lit]

  ctx.nim.write "]\n"
  ctx.nim.write "for k in $1:\n" % [sym]
  ctx.nim.write "  ctx.$1(k[0], k[1], k[2], k[3])\n" % [niminst]
  ctx.nim.write "  ctx.add(k[4])\n"

template genShiftCC(T: typedesc, imm: string) =
  let sym = ctx.newSym()
  let d = $disp
  for base in T.low..T.high:
    let bs = toLower($base)
    for index in T.low..T.high:
      let idx = toLower($index)
      for scale in {1, 2, 4, 8}:
        when T is reg32:
          if index == ESP: continue
        else:
          if index == RSP: continue
        let sc = $scale
        let lit = "$1 $2 [$3 + $4 * $5 + $6], $7" % [inst, size, bs, idx, sc, d, imm]
        ctx.lst.write lit & "\n"
        ctx.nim.write "  (nimjit.$1, $2, $3, $4, $5, $6, \"$7\"),\n" % [size, bs, idx, sc, d, imm, lit]

  ctx.nim.write "]\n"
  ctx.nim.write "for k in $1:\n" % [sym]
  ctx.nim.write "  ctx.$1(k[0], k[1], k[2], k[3], k[4], k[5])\n" % [niminst]
  ctx.nim.write "  ctx.add(k[6])\n"

proc testShift(ctx: TestContext, inst: string, niminst, size: string, disp: int) =
  genShiftC(reg32, "1")
  genShiftC(reg64, "1")
  genShiftCC(reg32, "1")
  genShiftCC(reg64, "1")
  genShiftC(reg32, "7")
  genShiftC(reg64, "7")
  genShiftCC(reg32, "7")
  genShiftCC(reg64, "7")
  genShiftC(reg32, "cl")
  genShiftC(reg64, "cl")
  genShiftCC(reg32, "cl")
  genShiftCC(reg64, "cl")

proc genShift(ctx: TestContext, inst, niminst: string) =
  beginTest(inst):
    ctx.testShift(inst, niminst)
    ctx.testShift(inst, niminst, "byte")
    ctx.testShift(inst, niminst, "word")
    ctx.testShift(inst, niminst, "dword")
    ctx.testShift(inst, niminst, "qword")

    ctx.testShift(inst, niminst, "byte", 0)
    ctx.testShift(inst, niminst, "word", 0)
    ctx.testShift(inst, niminst, "dword", 0)
    ctx.testShift(inst, niminst, "qword", 0)

    ctx.testShift(inst, niminst, "byte", 10)
    ctx.testShift(inst, niminst, "word", 10)
    ctx.testShift(inst, niminst, "dword", 10)
    ctx.testShift(inst, niminst, "qword", 10)

    ctx.testShift(inst, niminst, "byte", 3000)
    ctx.testShift(inst, niminst, "word", 3000)
    ctx.testShift(inst, niminst, "dword", 3000)
    ctx.testShift(inst, niminst, "qword", 3000)

template testArithA(T: typedesc) =
  let sym = ctx.newSym()
  let im = $imm
  for reg in T.low..T.high:
    let r = toLower($reg)
    when T is reg8:
      let lit = "$1 $2, byte $3" % [inst, r, im]
    elif T is reg16:
      let lit = "$1 $2, word $3" % [inst, r, im]
    elif T is reg32:
      let lit = "$1 $2, dword $3" % [inst, r, im]
    else:
      let lit = "$1 $2, $3" % [inst, r, im]
    ctx.lst.write lit & "\n"
    ctx.nim.write "  ($1, $2, \"$3\"),\n" % [r, im, lit]

  ctx.nim.write "]\n"
  ctx.nim.write "for k in $1:\n" % [sym]
  ctx.nim.write "  ctx.$1(k[0], k[1])\n" % [niminst]
  ctx.nim.write "  ctx.add(k[2])\n"

template testArithB(T: typedesc) =
  let sym = ctx.newSym()
  let im = $imm
  for reg in T.low..T.high:
    let r = toLower($reg)
    let lit = "$1 $2, $3" % [inst, r, im]
    ctx.lst.write lit & "\n"
    ctx.nim.write "  ($1, $2, \"$3\"),\n" % [r, im, lit]

  ctx.nim.write "]\n"
  ctx.nim.write "for k in $1:\n" % [sym]
  ctx.nim.write "  ctx.$1(k[0], k[1])\n" % [niminst]
  ctx.nim.write "  ctx.add(k[2])\n"

proc testArith(ctx: TestContext, inst, niminst: string, imm: int) =
  if imm <= 0xFF:
    testArithA(reg8)
    testArithA(reg16)
    testArithA(reg32)
    testArithA(reg64)
  elif imm > 0xFF and imm <= 0xFFFF:
    testArithB(reg16)
    testArithB(reg32)
    testArithB(reg64)
  elif imm > 0xFFFF:
    testArithB(reg32)
    testArithB(reg64)

template testArithC(T: typedesc) =
  let sym = ctx.newSym()
  let d = $disp
  let im = $imm
  for reg in T.low..T.high:
    let r = toLower($reg)
    let lit = "$1 $2 [$3 + $4], $5" % [inst, size, r, d, im]
    ctx.lst.write lit & "\n"
    ctx.nim.write "  (nimjit.$1, $2, $3, $4, \"$5\"),\n" % [size, r, d, im, lit]
    
  ctx.nim.write "]\n"
  ctx.nim.write "for k in $1:\n" % [sym]
  ctx.nim.write "  ctx.$1(k[0], k[1], k[2], k[3])\n" % [niminst]
  ctx.nim.write "  ctx.add(k[4])\n"

template testArithCC(T: typedesc) =
  let sym = ctx.newSym()
  let d = $disp
  let im = $imm
  for base in T.low..T.high:
    let bs = toLower($base)
    for index in T.low..T.high:
      let idx = toLower($index)
      for scale in {1, 2, 4, 8}:
        when T is reg32:
          if index == ESP: continue
        else:
          if index == RSP: continue
        let sc = toLower($scale)
        let lit = "$1 $2 [$3 + $4 * $5 + $6], $7" % [inst, size, bs, idx, sc, d, im]
        ctx.lst.write lit & "\n"
        ctx.nim.write "  (nimjit.$1, $2, $3, $4, $5, $6, \"$7\"),\n" % [size, bs, idx, sc, d, im, lit]

  ctx.nim.write "]\n"
  ctx.nim.write "for k in $1:\n" % [sym]
  ctx.nim.write "  ctx.$1(k[0], k[1], k[2], k[3], k[4], k[5])\n" % [niminst]
  ctx.nim.write "  ctx.add(k[6])\n"

template testArithD(T, TT: typedesc, size: expr) =
  let sym = ctx.newSym()
  let d = $disp
  for OP2 in TT.low..TT.high:
    let op = toLower($OP2)
    for reg in T.low..T.high:
      let r = toLower($reg)
      when TT is reg8 and T is reg64:
        if OP2 in {AH, CH, DH, BH} and reg >= R8: continue
      elif TT is reg8 and T is reg32:
        if OP2 in {AH, CH, DH, BH} and reg >= R8D: continue

      let lit = "$1 $2 [$3 + $4], $5" % [inst, size, r, d, op]
      ctx.lst.write lit & "\n"
      ctx.nim.write "  (nimjit.$1, $2, $3, $4, \"$5\"),\n" % [size, r, d, op, lit]

  ctx.nim.write "]\n"
  ctx.nim.write "for k in $1:\n" % [sym]
  ctx.nim.write "  ctx.$1(k[0], k[1], k[2], k[3])\n" % [niminst]
  ctx.nim.write "  ctx.add(k[4])\n"

template testArithDD(T, TT: typedesc, size: expr) =
  let sym = ctx.newSym()
  let d = $disp
  for OP2 in TT.low..TT.high:
    let op = toLower($OP2)
    for base in T.low..T.high:
      let bs = toLower($base)
      for index in T.low..T.high:
        let idx = toLoweR($index)
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

          let lit = "$1 $2 [$3 + $4 * $5 + $6], $7" % [inst, size, bs, idx, sc, d, op]
          ctx.lst.write lit & "\n"
          ctx.nim.write "  (nimjit.$1, $2, $3, $4, $5, $6, \"$7\"),\n" % [size, bs, idx, sc, d, op, lit]

  ctx.nim.write "]\n"
  ctx.nim.write "for k in $1:\n" % [sym]
  ctx.nim.write "  ctx.$1(k[0], k[1], k[2], k[3], k[4], k[5])\n" % [niminst]
  ctx.nim.write "  ctx.add(k[6])\n"

proc testArith(ctx: TestContext, inst, niminst, size: string, disp, imm: int) =
  testArithC(reg32)
  testArithC(reg64)
  testArithCC(reg32)
  testArithCC(reg64)

proc testArithMemRegA(ctx: TestContext, inst, niminst: string, disp: int) =
  testArithD(reg32, reg8, "byte")
  testArithD(reg32, reg16, "word")
  testArithD(reg32, reg32, "dword")
  testArithD(reg32, reg64, "qword")

proc testArithMemRegB(ctx: TestContext, inst, niminst: string, disp: int) =
  testArithD(reg64, reg8, "byte")
  testArithD(reg64, reg16, "word")
  testArithD(reg64, reg32, "dword")
  testArithD(reg64, reg64, "qword")

proc testArithMemRegC(ctx: TestContext, inst, niminst: string, disp: int) =
  beginTest(inst):
    testArithDD(reg32, reg8, "byte")
    testArithDD(reg32, reg16, "word")
    testArithDD(reg32, reg32, "dword")
    testArithDD(reg32, reg64, "qword")

proc testArithMemRegD(ctx: TestContext, inst, niminst: string, disp: int) =
  beginTest(inst):
    testArithDD(reg64, reg8, "byte")
    testArithDD(reg64, reg16, "word")
    testArithDD(reg64, reg32, "dword")
    testArithDD(reg64, reg64, "qword")

proc genArith(ctx: TestContext, inst, niminst: string) =
  #beginTest(inst):
  #  ctx.testArith(inst, niminst, 0)
  #  ctx.testArith(inst, niminst, 10)
  #  ctx.testArith(inst, niminst, 3000)
  #  ctx.testArith(inst, niminst, 300000)
  #
  #beginTest(inst):
  #  ctx.testArith(inst, niminst, "byte", 0, 0)
  #  ctx.testArith(inst, niminst, "byte", 0, 10)
  #  ctx.testArith(inst, niminst, "word", 0, 3000)
  #  ctx.testArith(inst, niminst, "dword", 0, 300000)
  #
  #  ctx.testArith(inst, niminst, "byte", 10, 0)
  #  ctx.testArith(inst, niminst, "byte", 10, 10)
  #  ctx.testArith(inst, niminst, "word", 10, 3000)
  #  ctx.testArith(inst, niminst, "dword", 10, 300000)
  #
  #  ctx.testArith(inst, niminst, "byte", 7000, 0)
  #  ctx.testArith(inst, niminst, "byte", 7000, 10)
  #  ctx.testArith(inst, niminst, "word", 7000, 3000)
  #  ctx.testArith(inst, niminst, "dword", 7000, 300000)
  #
  #beginTest(inst):
  #  ctx.testArithMemRegA(inst, niminst, 0)
  #  ctx.testArithMemRegA(inst, niminst, 100)
  #  ctx.testArithMemRegA(inst, niminst, 7000)
  #  ctx.testArithMemRegA(inst, niminst, 300000)
  #
  #  ctx.testArithMemRegB(inst, niminst, 0)
  #  ctx.testArithMemRegB(inst, niminst, 100)
  #  ctx.testArithMemRegB(inst, niminst, 7000)
  #  ctx.testArithMemRegB(inst, niminst, 300000)
  
  #ctx.testArithMemRegC(inst, niminst, 0)
  ctx.testArithMemRegC(inst, niminst, 100)
  #ctx.testArithMemRegC(inst, niminst, 7000)
  #ctx.testArithMemRegC(inst, niminst, 300000)
  #
  #ctx.testArithMemRegD(inst, niminst, 0)
  #ctx.testArithMemRegD(inst, niminst, 100)
  ctx.testArithMemRegD(inst, niminst, 7000)
  #ctx.testArithMemRegD(inst, niminst, 300000)

var ctx = newTest("BITS64")
#ctx.genSingleOperand("neg", "neg")
#ctx.genSingleOperand("not", "`not`")
#ctx.genSingleOperand("mul", "mul")
#ctx.genSingleOperand("dec", "dec")
#ctx.genSingleOperand("inc", "inc")
#
#ctx.genShift("rol", "rol")
#ctx.genShift("ror", "ror")
#ctx.genShift("rcl", "rcl")
#ctx.genShift("rcr", "rcr")
#ctx.genShift("sal", "sal")
#ctx.genShift("shl", "`shl`")
#ctx.genShift("shr", "`shr`")
#ctx.genShift("sar", "sar")

#ctx.genArith("add", "add")
ctx.genArith("or", "`or`")
ctx.genArith("adc", "adc")
ctx.genArith("sbb", "sbb")
ctx.genArith("and", "`and`")
ctx.genArith("sub", "sub")
ctx.genArith("xor", "`xor`")
ctx.genArith("cmp", "cmp")

