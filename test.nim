import strutils, nimjit, osproc, os, macros, times

type
  TestContext = ref object
    lst, nim: File
    lstName, nimName: string
    testName: string
    counter: int
    startTime: float
    bits: asmFlag

proc top*[T](ctx: TestContext): T =
  when T is reg8:
    result = if ctx.bits == BITS32: BH else: R15B
  elif T is reg16:
    result = if ctx.bits == BITS32: DI else: R15W
  elif T is reg32:
    result = if ctx.bits == BITS32: EDI else: R15D
  elif T is reg64:
    result = if ctx.bits == BITS32: RDI else: R15
  elif T is regxmm:
    result = if ctx.bits == BITS32: XMM7 else: XMM15
  elif T is regymm:
    result = if ctx.bits == BITS32: YMM7 else: YMM15
  elif T is regcr:
    result = if ctx.bits == BITS32: CR7 else: CR15
  elif T is regdr:
    result = if ctx.bits == BITS32: DR7 else: DR15
  elif T is regfpu:
    result = ST7
  elif T is regmmx:
    result = MMX7
  elif T is regseg:
    result = GS

proc newTest(bits: asmFlag): TestContext =
  new(result)
  result.counter = 0
  result.startTime = epochTime()
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

  ctx.nim.write "import nimjit, os, strutils\n"
  ctx.nim.write "var output: File\n"
  ctx.nim.write "if not open(output, paramStr(1), fmWrite):\n"
  ctx.nim.write "  echo \"cannot open \" & paramStr(1)\n"
  ctx.nim.write "  quit(-1)\n"
  ctx.nim.write "var ctx = newAssembler($1)\n" % [$ctx.bits]

proc close(ctx: TestContext) =
  ctx.nim.write "ctx.listing(output)\n"
  ctx.lst.close()
  ctx.nim.close()

  let bits = if ctx.bits == BITS64: "win64" else: "win32"
  var cmd = "nasm -f $1 -l nasmasm.lst $2" % [bits, ctx.lstName]
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
      echo "BITS: ", ctx.bits
      quit(-1)

    inc lineno

  ctx.lst.close()
  ctx.nim.close()

  let timePassed = formatFloat(epochTime() - ctx.startTime, ffDecimal, 3)

  echo "$1 $2 $3 OK $4" % [$ctx.bits, $ctx.counter, ctx.testName, timePassed]

macro beginTest(name: string, n: typed): stmt =
  var glue = "ctx.setName($1)\n" % [$name]
  result = parseStmt(glue)
  for c in n:
    result.add parseExpr("ctx.open()")
    result.add c
    result.add parseExpr("ctx.close()")
  #echo result.toStrLit.strVal

include 
  "test/singleop", "test/shiftgroup", "test/arithgroup",
  "test/pushpopgroup"

proc testSuite(bits: asmFlag) =
  var ctx = newTest(bits)
  #ctx.genSingleOperand("neg", "neg")
  #ctx.genSingleOperand("not", "`not`")
  #ctx.genSingleOperand("mul", "mul")
  #ctx.genSingleOperand("dec", "dec")
  #ctx.genSingleOperand("inc", "inc")

  #ctx.genShift("rol", "rol")
  #ctx.genShift("ror", "ror")
  #ctx.genShift("rcl", "rcl")
  #ctx.genShift("rcr", "rcr")
  #ctx.genShift("sal", "sal")
  #ctx.genShift("shl", "`shl`")
  #ctx.genShift("shr", "`shr`")
  #ctx.genShift("sar", "sar")
  #
  #ctx.genArith("add", "add")
  #ctx.genArith("or", "`or`")
  #ctx.genArith("adc", "adc")
  #ctx.genArith("sbb", "sbb")
  #ctx.genArith("and", "`and`")
  #ctx.genArith("sub", "sub")
  #ctx.genArith("xor", "`xor`")
  #ctx.genArith("cmp", "cmp")

  #ctx.genPushPop("pop", "pop")
  #ctx.genPushPop("push", "push")

testSuite(BITS64)
testSuite(BITS32)