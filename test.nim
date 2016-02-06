import strutils, nimjit, osproc, os, macros

type
  TestContext = ref object
    lst, nim: File
    lstName, nimName: string
    testName: string
    counter: int
    
proc newTest(): TestContext =
  new(result)
  result.counter = 0

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
  ctx.nim.write "var ctx = newAssembler()\n"

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
  echo "$1 $2 OK" % [$ctx.counter, ctx.testName]

macro beginTest(name: string, n: typed): stmt =
  var glue = "ctx.setName($1)\n" % [$name]
  result = parseStmt(glue)
  for c in n:
    result.add parseExpr("ctx.open()")
    result.add c
    result.add parseExpr("ctx.close()")
  #echo result.toStrLit.strVal
    
template genSingleOperandA(T: typedesc) =
  for reg in T.low.. <T.high:
    let lit = "$1 $2" % [inst, $reg]
    ctx.lst.write lit & "\n"
    ctx.nim.write "ctx.$1($2)\n" % [niminst, $reg]
    ctx.nim.write "ctx.add(\"$1\")\n" % [lit]

proc testSingleOperand(ctx: TestContext, inst, niminst: string) =
  genSingleOperandA(reg8)
  genSingleOperandA(reg16)
  genSingleOperandA(reg32)
  genSingleOperandA(reg64)
    
template genSingleOperandB(T: typedesc) =
  for reg in T.low.. <T.high:
    let lit = "$1 $2 [$3]" % [inst, size, $reg]
    ctx.lst.write lit & "\n"
    ctx.nim.write "ctx.$1(nimjit.$2, $3)\n" % [niminst, size, $reg]
    ctx.nim.write "ctx.add(\"$1\")\n" % [lit]
    
template genSingleOperandBB(T: typedesc) =
  for base in T.low.. <T.high:
    for index in T.low.. <T.high:
      for scale in {1, 2, 4, 8}:
        when T is reg32:
          if index == ESP: continue
        else:
          if index == RSP: continue
        let lit = "$1 $2 [$3 + $4 * $5]" % [inst, size, $base, $index, $scale]
        ctx.lst.write lit & "\n"
        ctx.nim.write "ctx.$1(nimjit.$2, $3, $4, $5)\n" % [niminst, size, $base, $index, $scale]
        ctx.nim.write "ctx.add(\"$1\")\n" % [lit]
    
proc testSingleOperand(ctx: TestContext, inst: string, niminst, size: string) =
  genSingleOperandB(reg32)
  genSingleOperandB(reg64)
  genSingleOperandBB(reg32)
  genSingleOperandBB(reg64)
  
template genSingleOperandC(T: typedesc) =
  for reg in T.low.. <T.high:
    let lit = "$1 $2 [$3 + $4]" % [inst, size, $reg, $disp]
    ctx.lst.write lit & "\n"
    ctx.nim.write "ctx.$1(nimjit.$2, $3, $4)\n" % [niminst, size, $reg, $disp]
    ctx.nim.write "ctx.add(\"$1\")\n" % [lit]

template genSingleOperandCC(T: typedesc) =
  for base in T.low.. <T.high:
    for index in T.low.. <T.high:
      for scale in {1, 2, 4, 8}:
        when T is reg32:
          if index == ESP: continue
        else:
          if index == RSP: continue
        let lit = "$1 $2 [$3 + $4 * $5 + $6]" % [inst, size, $base, $index, $scale, $disp]
        ctx.lst.write lit & "\n"
        ctx.nim.write "ctx.$1(nimjit.$2, $3, $4, $5, $6)\n" % [niminst, size, $base, $index, $scale, $disp]
        ctx.nim.write "ctx.add(\"$1\")\n" % [lit]

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
    
    ctx.testSingleOperand(inst, niminst, "byte", 10)
    ctx.testSingleOperand(inst, niminst, "word", 10)
    ctx.testSingleOperand(inst, niminst, "dword", 10)
    ctx.testSingleOperand(inst, niminst, "qword", 10)
    
    ctx.testSingleOperand(inst, niminst, "byte", 3000)
    ctx.testSingleOperand(inst, niminst, "word", 3000)
    ctx.testSingleOperand(inst, niminst, "dword", 3000)
    ctx.testSingleOperand(inst, niminst, "qword", 3000)
    
template genShiftA(T: typedesc, imm: string) =
  for reg in T.low.. <T.high:
    let lit = "$1 $2, $3" % [inst, $reg, imm]
    ctx.lst.write lit & "\n"
    ctx.nim.write "ctx.$1($2, $3)\n" % [niminst, $reg, $imm]
    ctx.nim.write "ctx.add(\"$1\")\n" % [lit]
        
proc testShift(ctx: TestContext, inst, niminst: string) =
  genShiftA(reg8, "1")
  genShiftA(reg16, "1")
  genShiftA(reg32, "1")
  genShiftA(reg64, "1")
  genShiftA(reg8, "7")
  genShiftA(reg16, "7")
  genShiftA(reg32, "7")
  genShiftA(reg64, "7")
  genShiftA(reg8, "CL")
  genShiftA(reg16, "CL")
  genShiftA(reg32, "CL")
  genShiftA(reg64, "CL")

template genShiftB(T: typedesc, imm: string) =
  for reg in T.low.. <T.high:
    let lit = "$1 $2 [$3], $4" % [inst, size, $reg, imm]
    ctx.lst.write lit & "\n"
    ctx.nim.write "ctx.$1(nimjit.$2, $3, 0, $4)\n" % [niminst, size, $reg, imm]
    ctx.nim.write "ctx.add(\"$1\")\n" % [lit]
    
template genShiftBB(T: typedesc, imm: string) =
  for base in T.low.. <T.high:
    for index in T.low.. <T.high:
      for scale in {1, 2, 4, 8}:
        when T is reg32:
          if index == ESP: continue
        else:
          if index == RSP: continue
        let lit = "$1 $2 [$3 + $4 * $5], $6" % [inst, size, $base, $index, $scale, imm]
        ctx.lst.write lit & "\n"
        ctx.nim.write "ctx.$1(nimjit.$2, $3, $4, $5, 0, $6)\n" % [niminst, size, $base, $index, $scale, imm]
        ctx.nim.write "ctx.add(\"$1\")\n" % [lit]
    
proc testShift(ctx: TestContext, inst: string, niminst, size: string) =
  genShiftB(reg32, "1")
  genShiftB(reg64, "1")
  genShiftBB(reg32, "1")
  genShiftBB(reg64, "1")
  genShiftB(reg32, "7")
  genShiftB(reg64, "7")
  genShiftBB(reg32, "7")
  genShiftBB(reg64, "7")
  genShiftB(reg32, "CL")
  genShiftB(reg64, "CL")
  genShiftBB(reg32, "CL")
  genShiftBB(reg64, "CL")

template genShiftC(T: typedesc, imm: string) =
  for reg in T.low.. <T.high:
    let lit = "$1 $2 [$3 + $4], $5" % [inst, size, $reg, $disp, imm]
    ctx.lst.write lit & "\n"
    ctx.nim.write "ctx.$1(nimjit.$2, $3, $4, $5)\n" % [niminst, size, $reg, $disp, imm]
    ctx.nim.write "ctx.add(\"$1\")\n" % [lit]

template genShiftCC(T: typedesc, imm: string) =
  for base in T.low.. <T.high:
    for index in T.low.. <T.high:
      for scale in {1, 2, 4, 8}:
        when T is reg32:
          if index == ESP: continue
        else:
          if index == RSP: continue
        let lit = "$1 $2 [$3 + $4 * $5 + $6], $7" % [inst, size, $base, $index, $scale, $disp, imm]
        ctx.lst.write lit & "\n"
        ctx.nim.write "ctx.$1(nimjit.$2, $3, $4, $5, $6, $7)\n" % [niminst, size, $base, $index, $scale, $disp, imm]
        ctx.nim.write "ctx.add(\"$1\")\n" % [lit]

proc testShift(ctx: TestContext, inst: string, niminst, size: string, disp: int) =
  genShiftC(reg32, "1")
  genShiftC(reg64, "1")
  genShiftCC(reg32, "1")
  genShiftCC(reg64, "1")
  genShiftC(reg32, "7")
  genShiftC(reg64, "7")
  genShiftCC(reg32, "7")
  genShiftCC(reg64, "7")
  genShiftC(reg32, "CL")
  genShiftC(reg64, "CL")
  genShiftCC(reg32, "CL")
  genShiftCC(reg64, "CL")
  
proc genShift(ctx: TestContext, inst, niminst: string) =
  beginTest(inst):
    ctx.testShift(inst, niminst)
    ctx.testShift(inst, niminst, "byte")
    ctx.testShift(inst, niminst, "word")
    ctx.testShift(inst, niminst, "dword")
    ctx.testShift(inst, niminst, "qword")
    
    ctx.testShift(inst, niminst, "byte", 10)
    ctx.testShift(inst, niminst, "word", 10)
    ctx.testShift(inst, niminst, "dword", 10)
    ctx.testShift(inst, niminst, "qword", 10)
    
    ctx.testShift(inst, niminst, "byte", 3000)
    ctx.testShift(inst, niminst, "word", 3000)
    ctx.testShift(inst, niminst, "dword", 3000)
    ctx.testShift(inst, niminst, "qword", 3000)
    
var ctx = newTest()
ctx.genSingleOperand("neg", "neg")
#ctx.genSingleOperand("not", "`not`")
#ctx.genSingleOperand("mul", "mul")
#ctx.genSingleOperand("dec", "dec")
#ctx.genSingleOperand("inc", "inc")
#
#ctx.genShift("rol", "rol")
#ctx.genShift("ror", "ror")
#ctx.genShift("rcl", "rcl")
#ctx.genShift("rcr", "rcr")
#
#ctx.genShift("sal", "sal")
#ctx.genShift("shl", "`shl`")
#ctx.genShift("shr", "`shr`")
#ctx.genShift("sar", "sar")