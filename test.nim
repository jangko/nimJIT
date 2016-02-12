import strutils, nimjit, osproc, os, macros, times

type
  TestContext = ref object
    lst, nim: File
    lstName, nimName: string
    testName: string
    counter: int
    startTime: float
    bits: asmFlag

proc newTest(bits: asmFlag): TestContext =
  new(result)
  result.counter = 0
  result.startTime = epochTime()
  result.bits = bits

proc setName(ctx: TestContext, testName: string) =
  ctx.testName = testName

proc open(ctx: TestContext) =
  inc ctx.counter
  ctx.nimName = "asm.nim"

  if not open(ctx.nim, ctx.nimName, fmWrite):
    echo "cannot open ", ctx.nimName
    quit(-1)

  ctx.nim.write "import nimjit, os, strutils, osproc\n"
  ctx.nim.write "var ctx = newAssembler($1, true)\n" % [$ctx.bits]

proc close(ctx: TestContext) =
  ctx.nim.write "var output: File\n"
  ctx.nim.write "if not open(output, paramStr(1), fmWrite):\n"
  ctx.nim.write "  echo \"cannot open \" & paramStr(1)\n"
  ctx.nim.write "  quit(-1)\n"
  ctx.nim.write "ctx.listing(output)\n"
  ctx.nim.write "output.close\n"
  ctx.nim.write "var source: File\n"
  ctx.nim.write "if not open(source, paramStr(2), fmWrite):\n"
  ctx.nim.write "  echo \"cannot open \" & paramStr(2)\n"
  ctx.nim.write "  quit(-1)\n"
  ctx.nim.write "ctx.asmSource(source)\n"
  ctx.nim.write "source.close\n"

  let bits = if ctx.bits == BITS64: "win64" else: "win32"
  ctx.nim.write "var cmd = \"nasm -f $1 -l nasmasm.lst $$1\" % [paramStr(2)]\n" % [bits]
  ctx.nim.write "if execShellCmd(cmd) != 0: quit(-1)\n"
  ctx.nim.close()

  var cmd = "nim c -d:release -o:njitasm.exe --verbosity:0 $1" % [ctx.nimName]
  if execShellCmd(cmd) != 0: quit(-1)

  cmd = "njitasm njitasm.lst asm.asm"
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

  if n.kind == nnkStmtList:
    for c in n:
      result.add parseExpr("ctx.open()")
      result.add c
      result.add parseExpr("ctx.close()")
  elif n.kind == nnkCall:
    result.add parseExpr("ctx.open()")
    result.add n
    result.add parseExpr("ctx.close()")
  else:
    echo n.treeRepr
    error("unsupported test mode")
  #echo result.toStrLit.strVal

include "test/singleop", "test/shiftgroup", "test/arithgroup",
  "test/pushpopgroup"

proc testSuite(bits: asmFlag) =
  var ctx = newTest(bits)
  #ctx.genSingleOperand("neg")
  #ctx.genSingleOperand("`not`")
  #ctx.genSingleOperand("mul")
  #ctx.genSingleOperand("dec")
  #ctx.genSingleOperand("inc")

  #ctx.genShift("rol")
  #ctx.genShift("ror")
  #ctx.genShift("rcl")
  #ctx.genShift("rcr")
  #ctx.genShift("sal")
  #ctx.genShift("`shl`")
  #ctx.genShift("`shr`")
  #ctx.genShift("sar")

  #ctx.genArith("add")
  #ctx.genArith("`or`")
  #ctx.genArith("adc")
  #ctx.genArith("sbb")
  #ctx.genArith("`and`")
  #ctx.genArith("sub")
  #ctx.genArith("`xor`")
  ctx.genArith("cmp")

  #ctx.genPushPop("pop")
  #ctx.genPushPop("push")

testSuite(BITS64)
testSuite(BITS32)