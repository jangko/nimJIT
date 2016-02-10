import strutils, nimjit, osproc, os, macros, times

type
  TestContext = ref object
    lst, nim: File
    lstName, nimName: string
    testName: string
    counter: int
    startTime: float
    bits: string

proc newTest(bits: string): TestContext =
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

include "test/singleop", "test/shiftgroup", "test/arithgroup"

var ctx = newTest("BITS64")
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

#ctx.genArith("add", "add")
#ctx.genArith("or", "`or`")
#ctx.genArith("adc", "adc")
#ctx.genArith("sbb", "sbb")
#ctx.genArith("and", "`and`")
#ctx.genArith("sub", "sub")
#ctx.genArith("xor", "`xor`")
#ctx.genArith("cmp", "cmp")

