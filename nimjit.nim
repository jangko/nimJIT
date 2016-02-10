import strutils, macros

type
  reg8* = enum
    AL, CL, DL, BL, AH, CH, DH, BH,
    R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B

  reg16* = enum
    AX, CX, DX, BX, SP, BP, SI, DI,
    R8W, R9W, R10W, R11W, R12W, R13W, R14W, R15W

  reg32* = enum
    EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI,
    R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D

  reg64* = enum
    RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
    R8, R9, R10, R11, R12, R13, R14, R15

  regfpu* = enum
    ST0, ST1, ST2, ST3, ST4, ST5, ST6, ST7

  regmmx* = enum
    MMX0, MMX1, MMX2, MMX3, MMX4, MMX5, MMX6, MMX7

  regxmm* = enum
    XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7,
    XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15

  regymm* = enum
    YMM0, YMM1, YMM2, YMM3, YMM4, YMM5, YMM6, YMM7,
    YMM8, YMM9, YMM10, YMM11, YMM12, YMM13, YMM14, YMM15

  regcr* = enum
    CR0, CR1, CR2, CR3, CR4, CR5, CR6, CR7,
    CR8, CR9, CR10, CR11, CR12, CR13, CR14, CR15

  regdr* = enum
    DR0, DR1, DR2, DR3, DR4, DR5, DR6, DR7,
    DR8, DR9, DR10, DR11, DR12, DR13, DR14, DR15

  regseg* = enum
    ES, CS, SS, DS, FS, GS

  regType* = enum
    REG8, REG16, REG32, REG64, REGFPU, REGMMX,
    REGXMM, REGYMM, REGCR, REGDR, REGSEG

  TReg* = distinct int

  oprSize* = enum
    BYTE, WORD, DWORD, QWORD

  asmFlag* = enum
    BITS32, BITS64

  Instruction* = ref object
    data: string

  Assembler* = ref object
    instList: seq[Instruction]
    literal: seq[string]
    bits: asmFlag
    genListing: bool
        
const
  SPL* = AH
  BPL* = CH
  SIL* = DH
  DIL* = BH
  byte* = BYTE
  word* = WORD
  dword* = DWORD
  qword* = QWORD

const
  REX_BASE = 0b0100_0000
  REX_W  = 0b0000_1000
  REX_R  = 0b0000_0100
  REX_X  = 0b0000_0010
  REX_B  = 0b0000_0001
  MOD_00 = 0b0000_0000
  MOD_01 = 0b0100_0000
  MOD_10 = 0b1000_0000
  MOD_11 = 0b1100_0000

macro genRegisters(n, s: typed): stmt =
  let im = getImpl(n.symbol)[2]
  var glue = "const\n"
  for i in 1..im.len-1:
    glue.add "  $1* = TReg(($2.int shl 8) or $3.int)\n" % [toLower($im[i]), $s, $im[i]]
  result = parseStmt(glue)

genRegisters(reg8, REG8)
genRegisters(reg16, REG16)
genRegisters(reg32, REG32)
genRegisters(reg64, REG64)
genRegisters(regfpu, REGFPU)
genRegisters(regmmx, REGMMX)
genRegisters(regxmm, REGXMM)
genRegisters(regymm, REGYMM)
genRegisters(regcr, REGCR)
genRegisters(regdr, REGDR)
genRegisters(regseg, REGSEG)

proc reg*(r: int, t: regType): TReg {.inline.} =
  result = TReg((t.int shl 8) or r)

proc len(inst: Instruction): int =
  result = inst.data.len

proc top*[T](ctx: Assembler): T =
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

#----------------------LISTING------------------------
proc padr(w, line: int): string =
  var numstr = $line
  if numstr.len < w:
    result = repeat(' ', w - numstr.len) & numstr
  else:
    result = numstr

proc toHex(inst: Instruction): string =
  result = ""
  for c in inst.data: result.add toHex(c.ord, 2)

proc listing*(ctx: Assembler, s: File) =
  var line = 1
  var disp = 0
  var lit = 0
  for inst in ctx.instList:
    let hex = toHex(inst)
    if hex.len <= 18:
      s.write "$1 $2 $3    $4\n" % [padr(6, line), toHex(disp, 8), hex & repeat(' ', 20 - hex.len), ctx.literal[lit]]
      inc(disp, inst.len)
    else:
      let hex1 = hex.substr(0, 17)
      let hex2 = hex.substr(18)
      s.write "$1 $2 $3    $4\n" % [padr(6, line), toHex(disp, 8), hex1 & "-" & repeat(' ', 19 - hex1.len), ctx.literal[lit]]
      inc(disp, hex1.len div 2)
      inc line
      s.write "$1 $2 $3\n" % [padr(6, line), toHex(disp, 8), hex2 & repeat(' ', 19 - hex2.len)]
      inc(disp, hex2.len div 2)

    inc line
    inc lit

#----------------------LISTING------------------------
proc add(ctx: Assembler, inst: Instruction) =
  ctx.instList.add inst

proc add*(ctx: Assembler, lit: string) =
  ctx.literal.add lit

proc appendChar(ctx: Assembler, imm: char) =
  ctx.instList[ctx.instList.len-1].data.add imm

proc appendWord(ctx: Assembler, imm: int) =
  var inst = ctx.instList[ctx.instList.len-1]
  inst.data.add chr(imm and 0xFF)
  inst.data.add chr((imm and 0xFF00) shr 8)

proc appendDWord(ctx: Assembler, imm: int) =
  var inst = ctx.instList[ctx.instList.len-1]
  inst.data.add chr(imm and 0xFF)
  inst.data.add chr((imm and 0x0000FF00) shr 8)
  inst.data.add chr((imm and 0x00FF0000) shr 16)
  inst.data.add chr((imm and 0xFF000000) shr 24)

proc newAssembler*(bits: asmFlag, genListing: bool = false): Assembler =
  new(result)
  result.instList = @[]
  result.literal = @[]
  result.bits = bits
  result.genListing = genListing

proc getRegType*(opr: TReg): regType {.inline.} =
  result = regType((opr.int and 0xFF00) shr 8)

proc getRegVal(opr: TReg): int {.inline.} =
  result = opr.int and 0xFF

template splitReg() {.dirty.} =
  let oprVal = opr.int and 0xFF
  let oprType = regType((opr.int and 0xFF00) shr 8)

template splitBaseIndex() {.dirty.} =
  let baseVal = base.int and 0xFF
  let baseType = regType((base.int and 0xFF00) shr 8)
  let indexVal = index.int and 0xFF
  let indexType = regType((index.int and 0xFF00) shr 8)

proc emit(ctx: Assembler, opcode, regMod: int, opr: TReg) =
  if ctx.bits == BITS32:
    doAssert(getRegType(opr) != REG64)
    doAssert(getRegVal(opr) < 0x08)

  splitReg()
  var modrm = MOD11 or (regMod shl 3) or (oprVal and 0b0000_0111)
  var data = ""

  if oprType == REG16 and ctx.bits in {BITS64, BITS32}:
    data.add chr(0x66)

  if oprType == REG64 and ctx.bits == BITS64:
    var rex = REX_BASE or REX_W
    if oprVal >= 0x08: rex = rex or REX_B
    data.add chr(rex)
  else:
    if oprVal >= 0x08: data.add(chr(REX_BASE or REX_B))

  data.add chr(opcode)
  data.add chr(modrm)
  var inst = Instruction(data: data)
  ctx.add inst

proc emit(ctx: Assembler, opcode, regMod: int, size: oprSize, opr: TReg, disp: int) =
  splitReg()
  var data = ""
  var useREX = false
  var rex = REX_BASE

  if ctx.bits == BITS32:
    doAssert(getRegType(opr) != REG64)
    doAssert(getRegVal(opr) < 0x08)

  doAssert(oprType in {REG32, REG64})
  if size == WORD: data.add chr(0x66)
  if oprType == REG32 and ctx.bits == BITS64: data.add chr(0x67)

  if oprVal >= 0x08:
    rex = rex or REX_B
    useREX = true

  if size == QWORD:
    rex = rex or REX_W
    useREX = true

  if useREX: data.add chr(rex)
  data.add chr(opcode)
  var modrm = 0
  if disp == 0: modrm = MOD_00
  elif disp > 0 and disp <= 0xFF: modrm = MOD_01
  elif disp > 0xFF: modrm = MOD_10
  modrm = modrm or (regMod shl 3) or (oprVal and 0b0000_0111)

  if (oprVal and 0b0000_0111) == 0b00_000_101:
    #EBP/R13D special case
    #transform it into opcode [reg + 0] using disp 0
    if disp >= 0 and disp <= 0xFF:
      modrm = MOD_01 or (regMod shl 3) or (oprVal and 0b0000_0111)
      data.add chr(modrm)
      data.add chr(disp)
    else:
      data.add chr(modrm)
  elif (oprVal and 0b0000_0111) == 0b00_000_100:
    #ESP/R12D special case
    #transform it into opcode [reg * 0] using SIB
    data.add chr(modrm)
    data.add chr(0x24)
  else:
    data.add chr(modrm)

  if disp > 0 and disp <= 0xFF:
    if (oprVal and 0b0000_0111) != 0b00_000_101:
      data.add chr(disp)
  elif disp > 0xFF:
    var disp32: uint32 = cast[uint32](disp)
    var dispLE = cast[cstring](addr(disp32))
    data.add dispLE[0]
    data.add dispLE[1]
    data.add dispLE[2]
    data.add dispLE[3]

  var inst = Instruction(data: data)
  ctx.add inst

proc emit(ctx: Assembler, opcode: int, size: oprSize, opr: TReg, disp: int, opr2: TReg) =
  splitReg()
  var data = ""
  var useREX = false
  var rex = REX_BASE

  if ctx.bits == BITS32:
    doAssert(getRegType(opr) != REG64)
    doAssert(getRegVal(opr) < 0x08)
    doAssert(getRegType(opr2) != REG64)
    doAssert(getRegVal(opr2) < 0x08)

  doAssert(oprType in {REG32, REG64})
  doAssert(getRegType(opr2) in {REG8, REG16, REG32, REG64})
  if size == WORD: data.add chr(0x66)
  if oprType == REG32 and ctx.bits == BITS64: data.add chr(0x67)

  if oprVal >= 0x08:
    rex = rex or REX_B
    useREX = true

  if size == QWORD:
    rex = rex or REX_W
    useREX = true

  let opr2Val = getRegVal(opr2)
  if opr2Val >= 0x08:
    rex = rex or REX_R
    useREX = true

  if getRegType(opr2) == REG8:
    doAssert(not (opr2Val.reg8 in {AH, CH, DH, BH} and useREX))

  if useREX: data.add chr(rex)
  data.add chr(opcode)
  var modrm = 0
  if disp == 0: modrm = MOD_00
  elif disp > 0 and disp <= 0xFF: modrm = MOD_01
  elif disp > 0xFF: modrm = MOD_10
  modrm = modrm or ((opr2Val and 0b0000_0111) shl 3) or (oprVal and 0b0000_0111)

  if (oprVal and 0b0000_0111) == 0b00_000_101:
    #EBP/R13D special case
    #transform it into opcode [reg + 0] using disp 0
    if disp >= 0 and disp <= 0xFF:
      modrm = MOD_01 or ((opr2Val and 0b0000_0111) shl 3) or (oprVal and 0b0000_0111)
      data.add chr(modrm)
      data.add chr(disp)
    else:
      data.add chr(modrm)
  elif (oprVal and 0b0000_0111) == 0b00_000_100:
    #ESP/R12D special case
    #transform it into opcode [reg * 0] using SIB
    data.add chr(modrm)
    data.add chr(0x24)
  else:
    data.add chr(modrm)

  if disp > 0 and disp <= 0xFF:
    if (oprVal and 0b0000_0111) != 0b00_000_101:
      data.add chr(disp)
  elif disp > 0xFF:
    var disp32: uint32 = cast[uint32](disp)
    var dispLE = cast[cstring](addr(disp32))
    data.add dispLE[0]
    data.add dispLE[1]
    data.add dispLE[2]
    data.add dispLE[3]

  var inst = Instruction(data: data)
  ctx.add inst

proc scaleToScale(scale: int): int =
  case scale
  of 2: result = 0b0100_0000
  of 4: result = 0b1000_0000
  of 8: result = 0b1100_0000
  else: result = 0x00

proc emit(ctx: Assembler, opcode, regMod: int, size: oprSize, base, index: TReg, scale, disp: int) =
  splitBaseIndex()
  var data = ""
  var useREX = false
  var rex = REX_BASE

  if ctx.bits == BITS32:
    doAssert(getRegType(base) != REG64)
    doAssert(getRegVal(base) < 0x08)
    doAssert(getRegType(index) != REG64)
    doAssert(getRegVal(index) < 0x08)

  doAssert(indexType == baseType)
  doAssert(indexType in {REG32, REG64})
  doAssert(scale in {1, 2, 4, 8})
  doAssert(indexVal != 0x04) #ESP or RSP
  if size == WORD: data.add chr(0x66)
  if indexType == REG32 and ctx.bits == BITS64: data.add chr(0x67)

  if baseVal >= 0x08:
    rex = rex or REX_B
    useREX = true

  if indexVal >= 0x8:
    rex = rex or REX_X
    useREX = true

  if size == QWORD:
    rex = rex or REX_W
    useREX = true

  if useREX: data.add chr(rex)
  data.add chr(opcode)
  var modrm = 0
  if disp == 0: modrm = MOD_00
  elif disp > 0 and disp <= 0xFF: modrm = MOD_01
  elif disp > 0xFF: modrm = MOD_10
  modrm = modrm or (regMod shl 3) or 0b0000_0100
  var sib = scaleToScale(scale) or ((indexVal and 0b0000_0111) shl 3) or (baseVal and 0b0000_0111)

  if (baseVal and 0b0000_0111) == 0b00_000_101:
    #EBP/R13D special case
    if disp >= 0 and disp <= 0xFF:
      modrm = MOD_01 or (regMod shl 3) or 0b0000_0100
      data.add chr(modrm)
      data.add chr(sib)
      data.add chr(disp)
    else:
      data.add chr(modrm)
      data.add chr(sib)
  else:
    data.add chr(modrm)

  if(baseVal and 0b0000_0111) != 0b00_000_101: data.add chr(sib)

  if disp > 0 and disp <= 0xFF:
    if (baseVal and 0b0000_0111) != 0b00_000_101:
      data.add chr(disp)
  elif disp > 0xFF:
    var disp32: uint32 = cast[uint32](disp)
    var dispLE = cast[cstring](addr(disp32))
    data.add dispLE[0]
    data.add dispLE[1]
    data.add dispLE[2]
    data.add dispLE[3]

  var inst = Instruction(data: data)
  ctx.add inst

proc emit(ctx: Assembler, opcode: int, size: oprSize, base, index: TReg, scale, disp: int, opr2: TReg) =
  splitBaseIndex()
  var data = ""
  var useREX = false
  var rex = REX_BASE
  if ctx.bits == BITS32:
    doAssert(getRegType(base) != REG64)
    doAssert(getRegVal(base) < 0x08)
    doAssert(getRegType(index) != REG64)
    doAssert(getRegVal(index) < 0x08)
    doAssert(getRegType(opr2) != REG64)
    doAssert(getRegVal(opr2) < 0x08)

  doAssert(baseType == indexType)
  doAssert(indexType in {REG32, REG64})
  doAssert(scale in {1, 2, 4, 8})
  doAssert(getRegType(opr2) in {REG8, REG16, REG32, REG64})
  doAssert(indexVal != 0x04) #ESP or RSP
  if size == WORD: data.add chr(0x66)
  if indexType == REG32 and ctx.bits == BITS64: data.add chr(0x67)

  if baseVal >= 0x08:
    rex = rex or REX_B
    useREX = true

  if indexVal >= 0x08:
    rex = rex or REX_X
    useREX = true

  if size == QWORD:
    rex = rex or REX_W
    useREX = true

  let opr2Val = getRegVal(opr2)
  if opr2Val >= 0x08:
    rex = rex or REX_R
    useREX = true

  if getRegType(opr2) == REG8:
    doAssert(not (opr2Val.reg8 in {AH, CH, DH, BH} and useREX))

  if useREX: data.add chr(rex)
  data.add chr(opcode)
  var modrm = 0
  if disp == 0: modrm = MOD_00
  elif disp > 0 and disp <= 0xFF: modrm = MOD_01
  elif disp > 0xFF: modrm = MOD_10
  modrm = modrm or ((opr2Val and 0b0000_0111) shl 3) or 0b0000_0100
  var sib = scaleToScale(scale) or ((indexVal and 0b0000_0111) shl 3) or (baseVal and 0b0000_0111)

  if (baseVal and 0b0000_0111) == 0b00_000_101:
    #EBP/R13D special case
    if disp >= 0 and disp <= 0xFF:
      modrm = MOD_01 or ((opr2Val and 0b0000_0111) shl 3) or 0b0000_0100
      data.add chr(modrm)
      data.add chr(sib)
      data.add chr(disp)
    else:
      data.add chr(modrm)
      data.add chr(sib)
  else:
    data.add chr(modrm)

  if(baseVal and 0b0000_0111) != 0b00_000_101: data.add chr(sib)

  if disp > 0 and disp <= 0xFF:
    if (baseVal and 0b0000_0111) != 0b00_000_101:
      data.add chr(disp)
  elif disp > 0xFF:
    var disp32: uint32 = cast[uint32](disp)
    var dispLE = cast[cstring](addr(disp32))
    data.add dispLE[0]
    data.add dispLE[1]
    data.add dispLE[2]
    data.add dispLE[3]

  var inst = Instruction(data: data)
  ctx.add inst

template reg8p1(opr: TReg): expr =
  (getRegType(opr) != REG8).int

proc isReg(opr: TReg, typ: regType, regVal: int): bool =
  result = getRegType(opr) == typ and getRegVal(opr) == regVal

proc deacc(n: NimNode): string {.compileTime.} =
  if n.kind == nnkAccQuoted: result = $n[0]
  else: result = $n

proc toStr(opr: TReg): string =
  let val = getRegVal(opr)
  case getRegType(opr)
  of REG8: result = $reg8(val)
  of REG16: result = $reg16(val)    
  of REG32: result = $reg32(val)
  of REG64: result = $reg64(val)
  of REGXMM: result = $regxmm(val)
  of REGYMM: result = $regymm(val)
  of REGCR: result = $regcr(val)
  of REGDR: result = $regdr(val)
  of REGFPU: result = $regfpu(val)
  of REGMMX: result = $regmmx(val)
  of REGSEG: result = $regseg(val)

proc toStr(opr: int): string =
  result = $opr
  
proc lit(ctx: Assembler, ins: string, opr: TReg) =
  if not ctx.genListing: return
  ctx.add "$1 $2" % [ins, toStr(opr)]

proc lit(ctx: Assembler, ins: string) =
  if not ctx.genListing: return
  ctx.add ins
  
proc lit(ctx: Assembler, ins: string, imm: int) =
  if not ctx.genListing: return
  ctx.add "$1 $2" % [ins, toStr(imm)]
  
proc lit(ctx: Assembler, ins: string, size: oprSize, opr: TReg, disp: int) =
  if not ctx.genListing: return
  if disp == 0:
    ctx.add "$1 $2 [$3]" % [ins, toLower($size), toStr(opr)]
  else:
    ctx.add "$1 $2 [$3 + $4]" % [ins, toLower($size), toStr(opr), $disp]

proc lit(ctx: Assembler, ins: string, size: oprSize, base, index: TReg, scale, disp: int) =
  if not ctx.genListing: return
  if disp == 0:
    if scale == 0:
      ctx.add "$1 $2 [$3 + $4]" % [ins, toLower($size), toStr(base), toStr(index)]
    else:
      ctx.add "$1 $2 [$3 + $4 * $5]" % [ins, toLower($size), toStr(base), toStr(index), $scale]
  else:
    if scale == 0:
      ctx.add "$1 $2 [$3 + $4 + $5]" % [ins, toLower($size), toStr(base), toStr(index), $disp]
    else:
      ctx.add "$1 $2 [$3 + $4 * $5 + $6]" % [ins, toLower($size), toStr(base), toStr(index), $scale, $disp]
      
proc lit[B: int | TReg](ctx: Assembler, ins: string, opr: TReg, imm: B) =
  if not ctx.genListing: return
  ctx.add "$1 $2, $3" % [ins, toStr(opr), toStr(imm)]  

proc lit2[B: int | TReg](ctx: Assembler, ins: string, opr: TReg, imm: B) =
  if not ctx.genListing: return
  when B is int:
    var size = ""
    let oprType = getRegType(opr)
    if oprType == REG8: size = "byte"
    elif oprType == REG16: size = "word"
    elif oprType == REG32: size = "dword"
  else:
    let size = ""
  ctx.add "$1 $2, $3 $4" % [ins, toStr(opr), size, toStr(imm)] 
  
proc lit[B: int | TReg](ctx: Assembler, ins: string, size: oprSize, opr: TReg, disp: int, imm: B) =
  if not ctx.genListing: return
  if disp == 0:
    ctx.add "$1 $2 [$3], $4" % [ins, toLower($size), toStr(opr), toStr(imm)]
  else:
    ctx.add "$1 $2 [$3 + $4], $5" % [ins, toLower($size), toStr(opr), $disp, toStr(imm)]
    
proc lit[B: int | TReg](ctx: Assembler, ins: string, size: oprSize, base, index: TReg, scale, disp: int, imm: B) =
  if not ctx.genListing: return
  if disp == 0:
    if scale == 0:
      ctx.add "$1 $2 [$3 + $4], $5" % [ins, toLower($size), toStr(base), toStr(index), toStr(imm)]
    else:
      ctx.add "$1 $2 [$3 + $4 * $5], $6" % [ins, toLower($size), toStr(base), toStr(index), $scale, toStr(imm)]
  else:
    if scale == 0:
      ctx.add "$1 $2 [$3 + $4 + $5], $6" % [ins, toLower($size), toStr(base), toStr(index), $disp, toStr(imm)]
    else:
      ctx.add "$1 $2 [$3 + $4 * $5 + $6], $7" % [ins, toLower($size), toStr(base), toStr(index), $scale, $disp, toStr(imm)]

proc lit(ctx: Assembler, ins: string, opr1, opr2: TReg, size: oprSize, disp: int) =
  if not ctx.genListing: return
  if disp == 0:
    ctx.add "$1 $2, $3 [$4]" % [ins, toStr(opr1), toLower($size), toStr(opr2)]
  else:
    ctx.add "$1 $2, $3 [$4 + $5]" % [ins, toStr(opr1), toLower($size), toStr(opr2), $disp]

proc lit(ctx: Assembler, ins: string, opr: TReg, size: oprSize, base, index: TReg, scale, disp: int) =
  if not ctx.genListing: return
  if disp == 0:
    if scale == 0:
      ctx.add "$1 $5, $2 [$3 + $4]" % [ins, toLower($size), toStr(base), toStr(index), toStr(opr)]
    else:
      ctx.add "$1 $6, $2 [$3 + $4 * $5]" % [ins, toLower($size), toStr(base), toStr(index), $scale, toStr(opr)]
  else:
    if scale == 0:
      ctx.add "$1 $6, $2 [$3 + $4 + $5]" % [ins, toLower($size), toStr(base), toStr(index), $disp, toStr(opr)]
    else:
      ctx.add "$1 $7, $2 [$3 + $4 * $5 + $6]" % [ins, toLower($size), toStr(base), toStr(index), $scale, $disp, toStr(opr)]
    
macro singleOperandGroup(inst: untyped, opCode, regMod: int, opCode2: int = 0): stmt =
  let ins = inst.deacc
  if opCode2.intVal != 0:
    result = quote do:
      proc `inst`*(ctx: Assembler, opr: TReg) =
        ctx.lit(`ins`, opr)
        let rt = getRegType(opr)
        doAssert(rt in {REG8, REG16, REG32, REG64})
        if rt in {REG16, REG32} and ctx.bits == BITS32:
          doAssert(getRegVal(opr) < 0x08)
          var data = ""
          if rt == REG16: data.add chr(0x66)
          data.add chr(`opCode2` or (getRegVal(opr) and 0b0000_0111))
          var inst = Instruction(data: data)
          ctx.add inst
        else:
          ctx.emit(`opCode` + reg8p1(opr), `regMod`, opr)
  else:
    result = quote do:
      proc `inst`*(ctx: Assembler, opr: TReg) =
        ctx.lit(`ins`, opr)
        doAssert(getRegType(opr) in {REG8, REG16, REG32, REG64})
        ctx.emit(`opCode` + reg8p1(opr), `regMod`, opr)

  result.add quote do:
    proc `inst`*(ctx: Assembler, size: oprSize, opr: TReg, disp: int = 0) =
      ctx.lit(`ins`, size, opr, disp)
      doAssert(getRegType(opr) in {REG32, REG64})
      ctx.emit(`opcode` + (size != BYTE).int, `regMod`, size, opr, disp)

    proc `inst`*(ctx: Assembler, size: oprSize, base, index: TReg, scale, disp: int = 0) =
      ctx.lit(`ins`, size, base, index, scale, disp)
      doAssert(getRegType(base) in {REG32, REG64})
      doAssert(getRegType(index) in {REG32, REG64})
      ctx.emit(`opcode` + (size != BYTE).int, `regMod`, size, base, index, scale, disp)

singleOperandGroup(inc, 0xFE, 0, 0x40)
singleOperandGroup(dec, 0xFE, 1, 0x48)
singleOperandGroup(`not`, 0xF6, 2)
singleOperandGroup(neg, 0xF6, 3)
singleOperandGroup(mul, 0xF6, 4)

macro shiftGroup(inst: untyped, opCode, regMod: int): stmt =
  let ins = inst.deacc
  result = quote do:
    proc `inst`*[B: int | TReg](ctx: Assembler, opr: TReg, imm: B) =
      ctx.lit(`ins`, opr, imm)
      doAssert(getRegType(opr) in {REG8, REG16, REG32, REG64})
      when B is int:
        doAssert(imm >= 0 and imm <= 0xFF)
        let ext = if imm == 1: 0x10 else: 0
        ctx.emit(`opCode` + ext + reg8p1(opr), `regMod`, opr)
        if imm != 1: ctx.appendChar chr(imm)
      else:
        doAssert(imm.isReg(REG8, CL.int))
        ctx.emit(`opCode` + 0x12 + reg8p1(opr), `regMod`, opr)

    proc `inst`*[B: int | TReg](ctx: Assembler, size: oprSize, opr: TReg, disp: int = 0, imm: B) =
      ctx.lit(`ins`, size, opr, disp, imm)
      doAssert(getRegType(opr) in {REG32, REG64})
      when B is int:
        doAssert(imm >= 0 and imm <= 0xFF)
        let ext = if imm == 1: 0x10 else: 0
        ctx.emit(`opcode` + (size != BYTE).int + ext, `regMod`, size, opr, disp)
        if imm != 1: ctx.appendChar chr(imm)
      else:
        doAssert(imm.isReg(REG8, CL.int))
        ctx.emit(`opcode` + (size != BYTE).int + 0x12, `regMod`, size, opr, disp)

    proc `inst`*[B: int | TReg](ctx: Assembler, size: oprSize, base, index: TReg, scale, disp: int = 0, imm: B) =
      ctx.lit(`ins`, size, base, index, scale, disp, imm)
      doAssert(getRegType(base) in {REG32, REG64})
      doAssert(getRegType(index) in {REG32, REG64})
      when B is int:
        doAssert(imm >= 0 and imm <= 0xFF)
        let ext = if imm == 1: 0x10 else: 0
        ctx.emit(`opcode` + (size != BYTE).int + ext, `regMod`, size, base, index, scale, disp)
        if imm != 1: ctx.appendChar chr(imm)
      else:
        doAssert(imm.isReg(REG8, CL.int))
        ctx.emit(`opcode` + (size != BYTE).int + 0x12, `regMod`, size, base, index, scale, disp)

shiftGroup(rol, 0xC0, 0)
shiftGroup(ror, 0xC0, 1)
shiftGroup(rcl, 0xC0, 2)
shiftGroup(rcr, 0xC0, 3)
shiftGroup(sal, 0xC0, 4)
shiftGroup(`shl`, 0xC0, 4)
shiftGroup(`shr`, 0xC0, 5)
shiftGroup(sar, 0xC0, 7)

macro arithGroup(inst: untyped, opMod, regMod: int): stmt =
  let ins = inst.deacc
  result = quote do:
    proc `inst`*(ctx: Assembler, opr: TReg, imm: int) =
      
      let oprVal = opr.int and 0xFF
      let oprType = regType((opr.int and 0xFF00) shr 8)
      if oprType == REG8:
        if oprVal == AL.int:
          var data = ""
          data.add chr(0x04 + `opMod`)
          var inst = Instruction(data: data)
          ctx.add inst
        else:
          ctx.emit(0x80, `regMod`, opr)
        doAssert(imm <= 0xFF)
        ctx.appendChar chr(imm and 0xFF)
        ctx.lit2(`ins`, opr, imm)
      else:
        if imm >= 0 and imm < 0xFF:
          ctx.lit2(`ins`, opr, imm)
          ctx.emit(0x83, `regMod`, opr)
          ctx.appendChar chr(imm and 0xFF)
        else:
          ctx.lit(`ins`, opr, imm)
          if oprVal == 0x00: # AX, EAX, RAX
            var data = ""
            if oprType == REG16:
              data.add chr(0x66)
            elif oprType == REG64:
              var rex = REX_BASE or REX_W
              if oprVal >= 0x08: rex = rex or REX_B
              data.add chr(rex)
            data.add chr(0x05 + `opMod`)
            var inst = Instruction(data: data)
            ctx.add inst
          else:
            ctx.emit(0x81, `regMod`, opr)
          if oprType == REG16:
            doAssert(imm <= 0xFFFF)
            ctx.appendWord(imm and 0xFFFF)
          else:
            doAssert(imm <= 0xFFFFFFFF)
            ctx.appendDWord(imm and 0xFFFFFFFF)

    proc `inst`*(ctx: Assembler, size: oprSize, opr: TReg, disp: int = 0, imm: int) =
      ctx.lit(`ins`, size, opr, disp, imm)
      doAssert(getRegType(opr) in {REG32, REG64})
      ctx.emit(0x80 + (size != BYTE).int, `regMod`, size, opr, disp)
      if size == BYTE:
        doAssert(imm <= 0xFF)
        ctx.appendChar chr(imm and 0xFF)
      elif size == WORD:
        doAssert(imm <= 0xFFFF)
        ctx.appendWord(imm and 0xFFFF)
      else:
        doAssert(imm <= 0xFFFFFFFF)
        ctx.appendDWord(imm and 0xFFFFFFFF)

    proc `inst`*(ctx: Assembler, size: oprSize, opr1: TReg, disp: int, opr2: TReg) =
      ctx.lit(`ins`, size, opr1, disp, opr2)
      let opr2Type = getRegType(opr2)
      doAssert(getRegType(opr1) in {REG32, REG64})
      doAssert(opr2Type in {REG8, REG16, REG32, REG64})
      if opr2Type == REG8: doAssert(size == BYTE)
      elif opr2Type == REG16: doAssert(size == WORD)
      elif opr2Type == REG32: doAssert(size == DWORD)
      else: doAssert(size == QWORD)
      ctx.emit(0x00 + (size != BYTE).int + `opMod`, size, opr1, disp, opr2)

    proc `inst`*(ctx: Assembler, opr1, opr2: TReg, size: oprSize, disp: int) =
      ctx.lit(`ins`, opr1, opr2, size, disp)
      let opr2Type = getRegType(opr2)
      doAssert(getRegType(opr1) in {REG32, REG64})
      doAssert(opr2Type in {REG8, REG16, REG32, REG64})
      if opr2Type == REG8: doAssert(size == BYTE)
      elif opr2Type == REG16: doAssert(size == WORD)
      elif opr2Type == REG32: doAssert(size == DWORD)
      else: doAssert(size == QWORD)
      ctx.emit(0x02 + (size != BYTE).int + `opMod`, size, opr2, disp, opr1)

    proc `inst`*(ctx: Assembler, size: oprSize, base, index: TReg, scale, disp: int, imm: int) =
      ctx.lit(`ins`, size, base, index, scale, disp, imm)
      ctx.emit(0x80 + (size != BYTE).int, `regMod`, size, base, index, scale, disp)
      if size == BYTE:
        doAssert(imm <= 0xFF)
        ctx.appendChar chr(imm and 0xFF)
      elif size == WORD:
        doAssert(imm <= 0xFFFF)
        ctx.appendWord(imm and 0xFFFF)
      else:
        doAssert(imm <= 0xFFFFFFFF)
        ctx.appendDWord(imm and 0xFFFFFFFF)

    proc `inst`*(ctx: Assembler, size: oprSize, base, index: TReg, scale, disp: int, opr2: TReg) =
      ctx.lit(`ins`, size, base, index, scale, disp, opr2)
      let opr2Type = getRegType(opr2)
      doAssert(opr2Type in {REG8, REG16, REG32, REG64})
      if opr2Type == REG8: doAssert(size == BYTE)
      elif opr2Type == REG16: doAssert(size == WORD)
      elif opr2Type == REG32: doAssert(size == DWORD)
      else: doAssert(size == QWORD)
      ctx.emit(0x00 + (size != BYTE).int + `opMod`, size, base, index, scale, disp, opr2)

    proc `inst`*(ctx: Assembler, opr1: TReg, size: oprSize, base, index: TReg, scale, disp: int = 0) =
      ctx.lit(`ins`, opr1, size, base, index, scale, disp)
      let opr1Type = getRegType(opr1)
      doAssert(opr1Type in {REG8, REG16, REG32, REG64})
      if opr1Type == REG8: doAssert(size == BYTE)
      elif opr1Type == REG16: doAssert(size == WORD)
      elif opr1Type == REG32: doAssert(size == DWORD)
      else: doAssert(size == QWORD)
      ctx.emit(0x02 + (size != BYTE).int + `opMod`, size, base, index, scale, disp, opr1)

arithGroup(add, 0x00, 0)
arithGroup(`or`, 0x08, 1)
arithGroup(adc, 0x10, 2)
arithGroup(sbb, 0x18, 3)
arithGroup(`and`,0x20, 4)
arithGroup(sub, 0x28, 5)
arithGroup(`xor`, 0x30, 6)
arithGroup(cmp, 0x38, 7)

macro pushPopGroup(inst: untyped, opCode: int): stmt =
  let ins = inst.deacc
  result = quote do:
    proc `inst`*(ctx: Assembler, opr: TReg) =
      ctx.lit(`ins`, opr)
      let oprType = getRegType(opr)
      let oprVal = getRegVal(opr)
      doAssert(oprType in {REG16, REG32, REG64})
      var data = ""
      if ctx.bits == BITS64:
        doAssert(oprType != REG32)
        if oprType == REG16: data.add chr(0x66)
        if oprVal >= 0x08:
          data.add chr(REX_BASE or REX_B)
        data.add chr(`opCode` or (oprVal and 0b0000_0111))
      else:
        doAssert(oprType != REG64)
        doAssert(oprVal < 0x08)
        if oprType == REG16: data.add chr(0x66)
        data.add chr(`opCode` or (oprVal and 0b0000_0111))

      var inst = Instruction(data: data)
      ctx.add inst

macro pushPopGroup(inst: untyped, opCode: int, regMod: int): stmt =
  let ins = inst.deacc
  result = quote do:
    proc `inst`*(ctx: Assembler, size: oprSize, opr: TReg, disp: int = 0) =
      ctx.lit(`ins`, size, opr, disp)
      var sizex = size
      if ctx.bits == BITS64:
        doAssert(size in {WORD, QWORD})
        if size == QWORD: sizex = DWORD #no REX prefix
      else:
        doAssert(size in {WORD, DWORD})
      ctx.emit(`opcode`, `regMod`, sizex, opr, disp)

    proc `inst`*(ctx: Assembler, size: oprSize, base, index: TReg, scale, disp: int = 0) =
      ctx.lit(`ins`, size, base, index, scale, disp)
      var sizex = size
      if ctx.bits == BITS64:
        doAssert(size in {WORD, QWORD})
        if size == QWORD: sizex = DWORD #no REX prefix
      else:
        doAssert(size in {WORD, DWORD})
      ctx.emit(`opcode`, `regMod`, sizex, base, index, scale, disp)

proc push*(ctx: Assembler, imm: int = 0) =
  if imm <= 0xFF:
    ctx.lit("push byte", imm)
    var data = ""
    data.add chr(0x6A)
    var inst = Instruction(data: data)
    ctx.add inst
    ctx.appendChar chr(imm and 0xFF)
  elif imm > 0xFF and imm <= 0xFFFF:
    ctx.lit("push word", imm)
    var data = ""
    data.add chr(0x66)
    data.add chr(0x68)
    var inst = Instruction(data: data)
    ctx.add inst
    ctx.appendWord(imm and 0xFFFF)
  elif imm > 0xFFFF:
    ctx.lit("push dword", imm)
    var data = ""
    data.add chr(0x68)
    var inst = Instruction(data: data)
    ctx.add inst
    ctx.appendDWord(imm and 0xFFFFFFFF)

proc push*(ctx: Assembler, opr: regseg) =
  ctx.lit("push", reg(opr.int, REGSEG))
  var data = ""
  case opr
  of CS: data.add chr(0x0E)
  of SS: data.add chr(0x16)
  of DS: data.add chr(0x1E)
  of ES: data.add chr(0x06)
  of FS:
    data.add chr(0x0F)
    data.add chr(0xA0)
  of GS:
    data.add chr(0x0F)
    data.add chr(0xA8)
  var inst = Instruction(data: data)
  ctx.add inst

proc pop*(ctx: Assembler, opr: regseg) =
  ctx.lit("pop", reg(opr.int, REGSEG))
  var data = ""
  if ctx.bits == BITS64:
    case opr
    of FS:
      data.add chr(0x0F)
      data.add chr(0xA1)
    of GS:
      data.add chr(0x0F)
      data.add chr(0xA9)
    else:
      doAssert(false)
  else:
    case opr
    of DS: data.add chr(0x1F)
    of ES: data.add chr(0x07)
    of SS: data.add chr(0x17)
    of FS:
      data.add chr(0x0F)
      data.add chr(0xA1)
    of GS:
      data.add chr(0x0F)
      data.add chr(0xA9)
    else:
      doAssert(false)
  var inst = Instruction(data: data)
  ctx.add inst

proc popa*(ctx: Assembler) =
  ctx.lit("popa")
  doAssert(ctx.bits != BITS64)
  var data = ""
  data.add chr(0x61)
  var inst = Instruction(data: data)
  ctx.add inst
  
proc popad*(ctx: Assembler) =
  ctx.lit("popad")
  doAssert(ctx.bits != BITS64)
  var data = ""
  data.add chr(0x61)
  var inst = Instruction(data: data)
  ctx.add inst
 
proc pusha*(ctx: Assembler) =
  ctx.lit("pusha")
  doAssert(ctx.bits != BITS64)
  var data = ""
  data.add chr(0x60)
  var inst = Instruction(data: data)
  ctx.add inst
  
proc pushad*(ctx: Assembler) =
  ctx.lit("pushad")
  doAssert(ctx.bits != BITS64)
  var data = ""
  data.add chr(0x60)
  var inst = Instruction(data: data)
  ctx.add inst
 
proc retn*(ctx: Assembler) =
  ctx.lit("retn")
  var data = ""
  data.add chr(0xC3)
  var inst = Instruction(data: data)
  ctx.add inst
  
proc retf*(ctx: Assembler) =
  ctx.lit("retf")
  var data = ""
  data.add chr(0xCB)
  var inst = Instruction(data: data)
  ctx.add inst

proc retn*(ctx: Assembler, imm: int) =
  ctx.lit("retn", imm)
  if imm == 0x00: 
    ctx.retn()
    return
    
  var data = ""
  data.add chr(0xC2)
  var inst = Instruction(data: data)
  ctx.add inst
  ctx.appendWord(imm and 0xFFFF)

proc retf*(ctx: Assembler, imm: int) =
  ctx.lit("retf", imm)
  if imm == 0x00: 
    ctx.retf()
    return
    
  var data = ""
  data.add chr(0xCA)
  var inst = Instruction(data: data)
  ctx.add inst
  ctx.appendWord(imm and 0xFFFF)
  
pushPopGroup(pop, 0x58)
pushPopGroup(push, 0x50)
pushPopGroup(pop, 0x8F, 0)
pushPopGroup(push, 0xFF, 6)