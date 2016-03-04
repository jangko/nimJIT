import strutils, nimjit, osproc, os, macros, times, streams

proc nasm_call(mode: int, input: cstring, insize: int, outsize: var int, output: var cstring): int {.cdecl, importc: "nasm_call", dynlib: "nasm.dll".}

var maxbits {.compileTime.} = BITS64

proc test(ctx: Assembler, name: string, bits: asmFlag) =
  echo name
  var listing = newStringStream()
  ctx.listing(listing)

  var source = newStringStream()
  ctx.asmSource(source)

  var outsize: int
  var output: cstring
  let nbit = if bits == BITS64: 64 else: 32
  
  if nasm_call(nbit, source.data, source.data.len, outsize, output) == 0:
    let res = newString(outsize)
    copyMem(res.cstring, output, outsize)
    if res != listing.data:
      var a = newStringStream(res)
      listing.setPosition(0)
      var line1 = newString(100)
      var line2 = newString(100)
      var lineno = 1
      while true:
        if not listing.readLine(line1): break
        if not a.readLine(line2): break
        if line1 != line2:
          echo "nimjit : ", line1
          echo "nasm   : ", line2
          echo "BITS   : ", bits
          quit(-1)
        inc lineno
  
proc genSOA(inst, r: string): string {.compileTime.} =
  result = "for reg in $1.low..top[$1](ctx):\n" % [toLower(r)]
  result.add "  ctx.$1(TReg(($2.int shl 8) or reg.int))\n" % [inst, r]

proc genSO1(inst: string): string {.compileTime.} =
  result = genSOA(inst, "REG8")
  result.add genSOA(inst, "REG16")
  result.add genSOA(inst, "REG32")
  if maxbits == BITS64: result.add genSOA(inst, "REG64")
  
proc genSOB(inst, size, r: string): string {.compileTime.} =
  result = "for reg in $1.low..top[$1](ctx):\n" % [toLower(r)]
  result.add "  ctx.$1(nimjit.$3, TReg(($2.int shl 8) or reg.int), 0)\n" % [inst, r, size]

proc genSOC(inst, size, r: string): string {.compileTime.} =
  result = "for base in $1.low..top[$1](ctx):\n" % [toLower(r)]
  result.add "  for index in $1.low..top[$1](ctx):\n" % [toLower(r)]
  result.add "    for scale in {1, 2, 4, 8}:\n"
  result.add "      if index.int == ESP.int: continue\n"
  result.add "      ctx.$1(nimjit.$2, reg(base.int, $3), reg(index.int, $3), scale, 0)\n" % [inst, size, r]

proc genSO2(inst: string, size: string): string {.compileTime.} =
  result = genSOB(inst, size, "REG32")
  if maxbits == BITS64: result.add genSOB(inst, size, "REG64")
  result.add genSOC(inst, size, "REG32")
  if maxbits == BITS64: result.add genSOC(inst, size, "REG64")
 
proc genSOD(inst: string, size: string, disp: int, r: string): string {.compileTime.} =
  result = "for reg in $1.low..top[$1](ctx):\n" % [toLower(r)]
  result.add "  ctx.$1(nimjit.$3, TReg(($2.int shl 8) or reg.int), $4)\n" % [inst, r, size, $disp]

proc genSOE(inst: string, size: string, disp: int, r: string): string {.compileTime.} =
  result = "for base in $1.low..top[$1](ctx):\n" % [toLower(r)]
  result.add "  for index in $1.low..top[$1](ctx):\n" % [toLower(r)]
  result.add "    for scale in {1, 2, 4, 8}:\n"
  result.add "      if index.int == ESP.int: continue\n"
  result.add "      ctx.$1(nimjit.$2, reg(base.int, $3), reg(index.int, $3), scale, $4)\n" % [inst, size, r, $disp]

proc genSO3(inst: string, size: string, disp: int): string {.compileTime.} =
  result = genSOD(inst, size, disp, "REG32")
  if maxbits == BITS64: result.add genSOD(inst, size, disp, "REG64")
  result.add genSOE(inst, size, disp, "REG32")
  if maxbits == BITS64: result.add genSOE(inst, size, disp, "REG64")
  
proc genSOX(inst: string): string {.compileTime.} =
  result = genSO1(inst)
  result.add genSO2(inst, "byte")
  result.add genSO2(inst, "word")
  result.add genSO2(inst, "dword")
  if maxbits == BITS64: result.add genSO2(inst, "qword")
  result.add genSO3(inst, "byte", 10)
  result.add genSO3(inst, "word", 10)
  result.add genSO3(inst, "dword", 10)
  if maxbits == BITS64: result.add genSO3(inst, "qword", 10)
  
proc genSOY(inst: string): string {.compileTime.} =
  result = genSO3(inst, "byte", 1000)
  #result.add genSO3(inst, "word", 1000)
  #result.add genSO3(inst, "dword", 1000)
  #if maxbits == BITS64: result.add genSO3(inst, "qword", 1000)

proc genSOW(inst: string): string {.compileTime.} =
  result = genSO3(inst, "byte", 300000)
  #result.add genSO3(inst, "word", 300000)
  #result.add genSO3(inst, "dword", 300000)
  #if maxbits == BITS64: result.add genSO3(inst, "qword", 300000)
  
proc genSOF(inst: string, size: string, disp: int, r: string): string {.compileTime.} =
  result = "for index in $1.low..top[$1](ctx):\n" % [toLower(r)]
  result.add "  for scale in {1, 2, 4, 8}:\n"
  result.add "    if index.int == ESP.int: continue\n"
  result.add "    ctx.$1(nimjit.$2, reg(index.int, $3), scale, $4)\n" % [inst, size, r, $disp]

proc genSO4(inst: string, size: string, disp: int): string {.compileTime.} =
  result = genSOF(inst, size, disp, "REG32")
  if maxbits == BITS64: result.add genSOF(inst, size, disp, "REG64")

proc genSOZ(inst: string): string {.compileTime.} =
  result = genSO4(inst, "byte", 10)
  result.add genSO4(inst, "word", 10)
  result.add genSO4(inst, "dword", 10)
  if maxbits == BITS64: result.add genSO4(inst, "qword", 10)

  result.add genSO4(inst, "byte", 300000)
  result.add genSO4(inst, "word", 300000)
  result.add genSO4(inst, "dword", 300000)
  if maxbits == BITS64: result.add genSO4(inst, "qword", 300000)
  
proc genTest(inst, body: string): string {.compileTime.} =
  result = "ctx = newAssembler($1, true)\n" % [$maxbits]
  result.add body
  result.add "ctx.test(\"$1\", $2)\n" % [inst, $maxbits]
  
macro testSingleOperand(inst: string): stmt =
  var glue = genTest(inst.strVal, genSOX(inst.strVal))
  glue.add genTest(inst.strVal, genSOY(inst.strVal))
  glue.add genTest(inst.strVal, genSOW(inst.strVal))
  glue.add genTest(inst.strVal, genSOZ(inst.strVal))
  result = parseStmt(glue)
  
var ctx: Assembler
testSingleOperand("neg")





