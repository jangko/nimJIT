import strutils, macros, streams

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
    REGXMM, REGYMM, REGCR, REGDR, REGSEG, REGMOD

  TReg* = distinct int

  oprSize* = enum
    BYTE, WORD, DWORD, QWORD, TWORD, OWORD, YWORD, ZWORD

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

const NA = TReg(0x80 shl 16)

proc isAvailable(r: TReg): bool {.inline.} =
  result = (r.int and NA.int) == 0

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

proc typeToSize(r: regType): oprSize =
  case r
  of REG8: result = BYTE
  of REG16: result = WORD
  of REG32: result = DWORD
  of REG64: result = QWORD
  of REGFPU: result = TWORD
  of REGMMX: result = QWORD
  of REGXMM: result = OWORD
  of REGYMM: result = YWORD
  of REGCR: result = DWORD
  of REGDR: result = DWORD
  of REGSEG: result = WORD
  of REGMOD: doAssert(false)
  
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

proc asmSource*(ctx: Assembler, s: File) =
  for lit in ctx.literal:
    s.writeLine lit

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

proc appendQWord(ctx: Assembler, imm: int64) =
  var inst = ctx.instList[ctx.instList.len-1]
  let s = cast[cstring](unsafeAddr(imm))
  inst.data.add s[0]
  inst.data.add s[1]
  inst.data.add s[2]
  inst.data.add s[3]
  inst.data.add s[4]
  inst.data.add s[5]
  inst.data.add s[6]
  inst.data.add s[7]

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
  let baseAvailable = isAvailable(base)
  let baseVal = if baseAvailable: base.int and 0xFF else : 0b0000_0101
  let baseType = regType((base.int and 0xFF00) shr 8)
  let indexVal = index.int and 0xFF
  let indexType = regType((index.int and 0xFF00) shr 8)

proc emit(ctx: Assembler, opcode: int, opr, opr2: TReg) =
  if ctx.bits == BITS32:
    doAssert(getRegType(opr) != REG64)
    doAssert(getRegVal(opr) < 0x08)
    doAssert(getRegType(opr2) != REG64)
    doAssert(getRegVal(opr2) < 0x08)

  splitReg()
  let opr2Type = getRegType(opr2)
  if opr2Type != REGMOD:
    if oprType != REGSEG and opr2Type != REGSEG: doAssert(oprType == opr2Type)

  var data = ""
  if oprType == REG16 and ctx.bits in {BITS64, BITS32}:
    data.add chr(0x66)

  var useREX = false
  var rex = REX_BASE

  if oprType == REG64:
    rex = rex or REX_W
    useREX = true

  if oprVal >= 0x08:
    rex = rex or REX_B
    useREX = true

  let opr2Val = getRegVal(opr2)
  if opr2Val >= 0x08:
    rex = rex or REX_R
    useREX = true

  if getRegType(opr2) == REG8:
    doAssert(not (opr2Val.reg8 in {AH, CH, DH, BH} and useREX))

  var modrm = MOD11 or ((opr2Val and 0b0000_0111) shl 3) or (oprVal and 0b0000_0111)
  if useREX: data.add chr(rex)
  data.add chr(opcode)
  data.add chr(modrm)
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

  let opr2Type = getRegType(opr2)
  doAssert(oprType in {REG32, REG64})
  doAssert(opr2Type in {REG8, REG16, REG32, REG64, REGMOD, REGSEG})
  if size == WORD and opr2Type != REGSEG: data.add chr(0x66)
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

proc emitBase(ctx: Assembler, opcode: int, size: oprSize, base, index: TReg, scale, disp: int, opr2: TReg) =
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

  if baseAvailable:
    doAssert(baseType == indexType)
  doAssert(indexType in {REG32, REG64})
  doAssert(scale in {1, 2, 4, 8})
  
  let opr2Type = getRegType(opr2)
  doAssert(opr2Type in {REG8, REG16, REG32, REG64, REGMOD, REGSEG})
  doAssert(indexVal != 0x04) #ESP or RSP
  if size == WORD and opr2Type != REGSEG: data.add chr(0x66)
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

  if not baseAvailable and scale > 2:
    modrm = MOD_00 or ((opr2Val and 0b0000_0111) shl 3) or 0b0000_0100
    data.add chr(modrm)
    var sib = scaleToScale(scale) or ((indexVal and 0b0000_0111) shl 3) or (baseVal and 0b0000_0111)
    data.add chr(sib)
  else:
    modrm = modrm or ((opr2Val and 0b0000_0111) shl 3) or 0b0000_0100
    var sib = scaleToScale(scale) or ((indexVal and 0b0000_0111) shl 3) or (baseVal and 0b0000_0111)

    if ((baseVal and 0b0000_0111) == 0b00_000_101):
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

    if ((baseVal and 0b0000_0111) != 0b00_000_101): data.add chr(sib)

  if not baseAvailable and scale > 2:
    var disp32: uint32 = cast[uint32](disp)
    var dispLE = cast[cstring](addr(disp32))
    data.add dispLE[0]
    data.add dispLE[1]
    data.add dispLE[2]
    data.add dispLE[3]
  else:
    if disp > 0 and disp <= 0xFF:
      if ((baseVal and 0b0000_0111) != 0b00_000_101) and baseAvailable:
        data.add chr(disp)
      if not baseAvailable:
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

proc emit(ctx: Assembler, opCode: int, size: oprSize, base, index: TReg, scale, disp: int, opr2: TReg) =
  let baseAvailable = isAvailable(base)
  if not baseAvailable and scale == 1:
    ctx.emit(opCode, size, index, disp, opr2)
    return
  elif not baseAvailable and scale == 2:
    ctx.emitBase(opCode, size, index, index, 1, disp, opr2)
  else:
    ctx.emitBase(opCode, size, base, index, scale, disp, opr2)

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
  else: result = $val

proc toStr(opr: BiggestInt): string =
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
  let basex = if isAvailable(base): toStr(base) & " + " else: ""
  if not ctx.genListing: return
  if disp == 0:
    if scale == 0:
      ctx.add "$1 $2 [$3$4]" % [ins, toLower($size), basex, toStr(index)]
    else:
      ctx.add "$1 $2 [$3$4 * $5]" % [ins, toLower($size), basex, toStr(index), $scale]
  else:
    if scale == 0:
      ctx.add "$1 $2 [$3$4 + $5]" % [ins, toLower($size), basex, toStr(index), $disp]
    else:
      ctx.add "$1 $2 [$3$4 * $5 + $6]" % [ins, toLower($size), basex, toStr(index), $scale, $disp]

proc lit[B: int64 | int | TReg](ctx: Assembler, ins: string, opr: TReg, imm: B) =
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
  let basex = if isAvailable(base): toStr(base) & " + " else: ""
  if not ctx.genListing: return
  if disp == 0:
    if scale == 0:
      ctx.add "$1 $2 [$3$4], $5" % [ins, toLower($size), basex, toStr(index), toStr(imm)]
    else:
      ctx.add "$1 $2 [$3$4 * $5], $6" % [ins, toLower($size), basex, toStr(index), $scale, toStr(imm)]
  else:
    if scale == 0:
      ctx.add "$1 $2 [$3$4 + $5], $6" % [ins, toLower($size), basex, toStr(index), $disp, toStr(imm)]
    else:
      ctx.add "$1 $2 [$3$4 * $5 + $6], $7" % [ins, toLower($size), basex, toStr(index), $scale, $disp, toStr(imm)]

proc lit(ctx: Assembler, ins: string, opr1, opr2: TReg, size: oprSize, disp: int) =
  if not ctx.genListing: return
  if disp == 0:
    ctx.add "$1 $2, $3 [$4]" % [ins, toStr(opr1), toLower($size), toStr(opr2)]
  else:
    ctx.add "$1 $2, $3 [$4 + $5]" % [ins, toStr(opr1), toLower($size), toStr(opr2), $disp]

proc lit(ctx: Assembler, ins: string, opr: TReg, size: oprSize, base, index: TReg, scale, disp: int) =
  let basex = if isAvailable(base): toStr(base) & " + " else: ""
  if not ctx.genListing: return
  if disp == 0:
    if scale == 0:
      ctx.add "$1 $5, $2 [$3$4]" % [ins, toLower($size), basex, toStr(index), toStr(opr)]
    else:
      ctx.add "$1 $6, $2 [$3$4 * $5]" % [ins, toLower($size), basex, toStr(index), $scale, toStr(opr)]
  else:
    if scale == 0:
      ctx.add "$1 $6, $2 [$3$4 + $5]" % [ins, toLower($size), basex, toStr(index), $disp, toStr(opr)]
    else:
      ctx.add "$1 $7, $2 [$3$4 * $5 + $6]" % [ins, toLower($size), basex, toStr(index), $scale, $disp, toStr(opr)]

proc lit(ctx: Assembler, ins: string, opr1, opr2: TReg, disp: int) =
  if not ctx.genListing: return
  if disp == 0:
    ctx.add "$1 $2, [$3]" % [ins, toStr(opr1), toStr(opr2)]
  else:
    ctx.add "$1 $2, [$3 + $4]" % [ins, toStr(opr1), toStr(opr2), $disp]

proc lit(ctx: Assembler, ins: string, opr: TReg, base, index: TReg, scale, disp: int) =
  let basex = if isAvailable(base): toStr(base) & " + " else: ""
  if not ctx.genListing: return
  if disp == 0:
    if scale == 0:
      ctx.add "$1 $2, [$3$4]" % [ins, toStr(opr), basex, toStr(index)]
    else:
      ctx.add "$1 $2, [$3$4 * $5]" % [ins, toStr(opr), basex, toStr(index), $scale]
  else:
    if scale == 0:
      ctx.add "$1 $2, [$3$4 + $5]" % [ins, toStr(opr), basex, toStr(index), $disp]
    else:
      ctx.add "$1 $2, [$3$4 * $5 + $6]" % [ins, toStr(opr), basex, toStr(index), $scale, $disp]
      
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
          ctx.emit(`opCode` + reg8p1(opr), opr, reg(`regMod`, REGMOD))
  else:
    result = quote do:
      proc `inst`*(ctx: Assembler, opr: TReg) =
        ctx.lit(`ins`, opr)
        doAssert(getRegType(opr) in {REG8, REG16, REG32, REG64})
        ctx.emit(`opCode` + reg8p1(opr), opr, reg(`regMod`, REGMOD))

  result.add quote do:
    proc `inst`*(ctx: Assembler, size: oprSize, opr: TReg, disp: int) =
      ctx.lit(`ins`, size, opr, disp)
      doAssert(getRegType(opr) in {REG32, REG64})
      ctx.emit(`opcode` + (size != BYTE).int, size, opr, disp, reg(`regMod`, REGMOD))

    proc `inst`*(ctx: Assembler, size: oprSize, base, index: TReg, scale: int, disp: int) =
      ctx.lit(`ins`, size, base, index, scale, disp)
      if isAvailable(base): doAssert(getRegType(base) in {REG32, REG64})
      doAssert(getRegType(index) in {REG32, REG64})
      ctx.emit(`opcode` + (size != BYTE).int, size, base, index, scale, disp, reg(`regMod`, REGMOD))

    proc `inst`*(ctx: Assembler, size: oprSize, index: TReg, scale: int, disp: int) =
      ctx.`inst`(size, NA, index, scale, disp)

singleOperandGroup(inc, 0xFE, 0, 0x40)
singleOperandGroup(dec, 0xFE, 1, 0x48)
singleOperandGroup(`not`, 0xF6, 2)
singleOperandGroup(neg, 0xF6, 3)
singleOperandGroup(mul, 0xF6, 4)
singleOperandGroup(imul, 0xF6, 5)
singleOperandGroup(`div`, 0xF6, 6)
singleOperandGroup(idiv, 0xF6, 7)

macro shiftGroup(inst: untyped, opCode, regMod: int): stmt =
  let ins = inst.deacc
  result = quote do:
    proc `inst`*[B: int | TReg](ctx: Assembler, opr: TReg, imm: B) =
      ctx.lit(`ins`, opr, imm)
      doAssert(getRegType(opr) in {REG8, REG16, REG32, REG64})
      when B is int:
        doAssert(imm >= 0 and imm <= 0xFF)
        let ext = if imm == 1: 0x10 else: 0
        ctx.emit(`opCode` + ext + reg8p1(opr), opr, reg(`regMod`, REGMOD))
        if imm != 1: ctx.appendChar chr(imm)
      else:
        doAssert(imm.isReg(REG8, CL.int))
        ctx.emit(`opCode` + 0x12 + reg8p1(opr), opr, reg(`regMod`, REGMOD))

    proc `inst`*[B: int | TReg](ctx: Assembler, size: oprSize, opr: TReg, disp: int, imm: B) =
      ctx.lit(`ins`, size, opr, disp, imm)
      doAssert(getRegType(opr) in {REG32, REG64})
      when B is int:
        doAssert(imm >= 0 and imm <= 0xFF)
        let ext = if imm == 1: 0x10 else: 0
        ctx.emit(`opcode` + (size != BYTE).int + ext, size, opr, disp, reg(`regMod`, REGMOD))
        if imm != 1: ctx.appendChar chr(imm)
      else:
        doAssert(imm.isReg(REG8, CL.int))
        ctx.emit(`opcode` + (size != BYTE).int + 0x12, size, opr, disp, reg(`regMod`, REGMOD))

    proc `inst`*[B: int | TReg](ctx: Assembler, size: oprSize, base, index: TReg, scale, disp: int, imm: B) =
      ctx.lit(`ins`, size, base, index, scale, disp, imm)
      if isAvailable(base): doAssert(getRegType(base) in {REG32, REG64})
      doAssert(getRegType(index) in {REG32, REG64})
      when B is int:
        doAssert(imm >= 0 and imm <= 0xFF)
        let ext = if imm == 1: 0x10 else: 0
        ctx.emit(`opcode` + (size != BYTE).int + ext, size, base, index, scale, disp, reg(`regMod`, REGMOD))
        if imm != 1: ctx.appendChar chr(imm)
      else:
        doAssert(imm.isReg(REG8, CL.int))
        ctx.emit(`opcode` + (size != BYTE).int + 0x12, size, base, index, scale, disp, reg(`regMod`, REGMOD))

    proc `inst`*[B: int | TReg](ctx: Assembler, size: oprSize, index: TReg, scale, disp: int, imm: B) =
      ctx.`inst`(size, NA, index, scale, disp, imm)

shiftGroup(rol, 0xC0, 0)
shiftGroup(ror, 0xC0, 1)
shiftGroup(rcl, 0xC0, 2)
shiftGroup(rcr, 0xC0, 3)
shiftGroup(sal, 0xC0, 4)
shiftGroup(`shl`, 0xC0, 4)
shiftGroup(`shr`, 0xC0, 5)
shiftGroup(sar, 0xC0, 7)

proc stdRMREG(ctx: Assembler, inst: string, opCode: int, size: oprSize, opr1: TReg, disp: int, opr2: TReg) =
  ctx.lit(inst, size, opr1, disp, opr2)
  let opr2Type = getRegType(opr2)
  doAssert(getRegType(opr1) in {REG32, REG64})
  doAssert(opr2Type in {REG8, REG16, REG32, REG64})
  if opr2Type == REG8: doAssert(size == BYTE)
  elif opr2Type == REG16: doAssert(size == WORD)
  elif opr2Type == REG32: doAssert(size == DWORD)
  else: doAssert(size == QWORD)
  ctx.emit(opCode, size, opr1, disp, opr2)

proc stdREGRM(ctx: Assembler, inst: string, opCode: int, opr1, opr2: TReg, size: oprSize, disp: int) =
  ctx.lit(inst, opr1, opr2, size, disp)
  let opr2Type = getRegType(opr2)
  doAssert(getRegType(opr1) in {REG32, REG64})
  doAssert(opr2Type in {REG8, REG16, REG32, REG64})
  if opr2Type == REG8: doAssert(size == BYTE)
  elif opr2Type == REG16: doAssert(size == WORD)
  elif opr2Type == REG32: doAssert(size == DWORD)
  else: doAssert(size == QWORD)
  ctx.emit(opCode, size, opr2, disp, opr1)
 
proc stdRMREG(ctx: Assembler, inst: string, opCode: int, size: oprSize, base, index: TReg, scale, disp: int, opr2: TReg) =
  ctx.lit(inst, size, base, index, scale, disp, opr2)
  let opr2Type = getRegType(opr2)
  doAssert(opr2Type in {REG8, REG16, REG32, REG64})
  if opr2Type == REG8: doAssert(size == BYTE)
  elif opr2Type == REG16: doAssert(size == WORD)
  elif opr2Type == REG32: doAssert(size == DWORD)
  else: doAssert(size == QWORD)
  ctx.emit(opCode, size, base, index, scale, disp, opr2)
      
proc stdREGRM(ctx: Assembler, inst: string, opCode: int, opr1: TReg, size: oprSize, base, index: TReg, scale, disp: int) =
  ctx.lit(inst, opr1, size, base, index, scale, disp)
  let opr1Type = getRegType(opr1)
  doAssert(opr1Type in {REG8, REG16, REG32, REG64})
  if opr1Type == REG8: doAssert(size == BYTE)
  elif opr1Type == REG16: doAssert(size == WORD)
  elif opr1Type == REG32: doAssert(size == DWORD)
  else: doAssert(size == QWORD)
  ctx.emit(opCode, size, base, index, scale, disp, opr1)

proc stdRMIMM(ctx: Assembler, inst: string, opCode, regMod: int, size: oprSize, opr: TReg, disp, imm: int) =
  ctx.lit(inst, size, opr, disp, imm)
  doAssert(getRegType(opr) in {REG32, REG64})
  ctx.emit(opCode, size, opr, disp, reg(regMod, REGMOD))
  if size == BYTE:
    doAssert(imm <= 0xFF)
    ctx.appendChar chr(imm and 0xFF)
  elif size == WORD:
    doAssert(imm <= 0xFFFF)
    ctx.appendWord(imm and 0xFFFF)
  else:
    doAssert(imm <= 0xFFFFFFFF)
    ctx.appendDWord(imm and 0xFFFFFFFF)

proc stdRMIMM(ctx: Assembler, inst: string, opCode, regMod: int, size: oprSize, base, index: TReg, scale, disp: int, imm: int) =
  ctx.lit(inst, size, base, index, scale, disp, imm)
  ctx.emit(opCode, size, base, index, scale, disp, reg(regMod, REGMOD))
  if size == BYTE:
    doAssert(imm <= 0xFF)
    ctx.appendChar chr(imm and 0xFF)
  elif size == WORD:
    doAssert(imm <= 0xFFFF)
    ctx.appendWord(imm and 0xFFFF)
  else:
    doAssert(imm <= 0xFFFFFFFF)
    ctx.appendDWord(imm and 0xFFFFFFFF)
        
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
          ctx.emit(0x80, opr, reg(`regMod`, REGMOD))
        doAssert(imm <= 0xFF)
        ctx.appendChar chr(imm and 0xFF)
        ctx.lit2(`ins`, opr, imm)
      else:
        if imm >= 0 and imm < 0xFF:
          ctx.lit2(`ins`, opr, imm)
          ctx.emit(0x83, opr, reg(`regMod`, REGMOD))
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
            ctx.emit(0x81, opr, reg(`regMod`, REGMOD))
          if oprType == REG16:
            doAssert(imm <= 0xFFFF)
            ctx.appendWord(imm and 0xFFFF)
          else:
            doAssert(imm <= 0xFFFFFFFF)
            ctx.appendDWord(imm and 0xFFFFFFFF)

    proc `inst`*(ctx: Assembler, size: oprSize, opr: TReg, disp, imm: int) =
      ctx.stdRMIMM(`ins`, 0x80 + (size != BYTE).int, `regMod`, size, opr, disp, imm)
      
    proc `inst`*(ctx: Assembler, size: oprSize, opr1: TReg, disp: int, opr2: TReg) =
      ctx.stdRMREG(`ins`, 0x00 + (size != BYTE).int + `opMod`, size, opr1, disp, opr2)
      
    proc `inst`*(ctx: Assembler, opr1, opr2: TReg, size: oprSize, disp: int) =
      ctx.stdREGRM(`ins`, 0x02 + (size != BYTE).int + `opMod`, opr1, opr2, size, disp)

    proc `inst`*(ctx: Assembler, size: oprSize, base, index: TReg, scale, disp: int, imm: int) =
      ctx.stdRMIMM(`ins`, 0x80 + (size != BYTE).int, `regMod`, size, base, index, scale, disp, imm)
      
    proc `inst`*(ctx: Assembler, size: oprSize, base, index: TReg, scale, disp: int, opr2: TReg) =
      ctx.stdRMREG(`ins`, 0x00 + (size != BYTE).int + `opMod`, size, base, index, scale, disp, opr2)

    proc `inst`*[B: int | TReg](ctx: Assembler, size: oprSize, index: TReg, scale, disp: int, imm: B) =
      ctx.`inst`(size, NA, index, scale, disp, imm)

    proc `inst`*(ctx: Assembler, opr1: TReg, size: oprSize, base, index: TReg, scale, disp: int) =
      ctx.stdREGRM(`ins`, 0x02 + (size != BYTE).int + `opMod`, opr1, size, base, index, scale, disp)

    proc `inst`*(ctx: Assembler, opr1: TReg, size: oprSize, index: TReg, scale, disp: int) =
      ctx.`inst`(opr1, size, NA, index, scale, disp)

    proc `inst`*(ctx: Assembler, opr1, opr2: TReg) =
      ctx.lit(`ins`, opr1, opr2)
      ctx.emit(0x00 + (getRegType(opr1) != REG8).int + `opMod`, opr1, opr2)

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
    proc `inst`*(ctx: Assembler, size: oprSize, opr: TReg, disp: int) =
      ctx.lit(`ins`, size, opr, disp)
      var sizex = size
      if ctx.bits == BITS64:
        doAssert(size in {WORD, QWORD})
        if size == QWORD: sizex = DWORD #no REX prefix
      else:
        doAssert(size in {WORD, DWORD})
      ctx.emit(`opcode`, sizex, opr, disp, reg(`regMod`, REGMOD))

    proc `inst`*(ctx: Assembler, size: oprSize, base, index: TReg, scale, disp: int) =
      ctx.lit(`ins`, size, base, index, scale, disp)
      var sizex = size
      if ctx.bits == BITS64:
        doAssert(size in {WORD, QWORD})
        if size == QWORD: sizex = DWORD #no REX prefix
      else:
        doAssert(size in {WORD, DWORD})
      ctx.emit(`opcode`, sizex, base, index, scale, disp, reg(`regMod`, REGMOD))

    proc `inst`*(ctx: Assembler, size: oprSize, index: TReg, scale, disp: int) =
      ctx.`inst`(size, NA, index, scale, disp)

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

proc oneByte(ctx: Assembler, opCode: int, lit: string) =
  var data = ""
  data.add chr(opCode)
  var inst = Instruction(data: data)
  ctx.add inst
  ctx.lit(lit)

proc oneByteB(ctx: Assembler, opCode: int, lit: string, imm: int) =
  var data = ""
  data.add chr(opCode)
  var inst = Instruction(data: data)
  ctx.add inst
  ctx.lit(lit, imm)
  ctx.appendWord(imm and 0xFF)

proc oneByteW(ctx: Assembler, opCode: int, lit: string, imm: int) =
  var data = ""
  data.add chr(opCode)
  var inst = Instruction(data: data)
  ctx.add inst
  ctx.lit(lit, imm)
  ctx.appendWord(imm and 0xFFFF)

proc popa*(ctx: Assembler) =
  doAssert(ctx.bits != BITS64)
  ctx.oneByte(0x61, "popa")

proc popad*(ctx: Assembler) =
  doAssert(ctx.bits != BITS64)
  ctx.oneByte(0x61, "popad")

proc pusha*(ctx: Assembler) =
  doAssert(ctx.bits != BITS64)
  ctx.oneByte(0x60, "pusha")

proc pushad*(ctx: Assembler) =
  doAssert(ctx.bits != BITS64)
  ctx.oneByte(0x60, "pushad")

pushPopGroup(pop, 0x58)
pushPopGroup(push, 0x50)
pushPopGroup(pop, 0x8F, 0)
pushPopGroup(push, 0xFF, 6)


#misc group
#retn, retf, retn iw, retf iw
#int3, int0, int ib
proc retn*(ctx: Assembler) =
  ctx.oneByte(0xC3, "retn")

proc retf*(ctx: Assembler) =
  ctx.oneByte(0xCB, "retf")

proc retn*(ctx: Assembler, imm: int) =
  if imm == 0x00:
    ctx.retn()
    return
  ctx.oneByteW(0xC2, "retf", imm)

proc retf*(ctx: Assembler, imm: int) =
  if imm == 0x00:
    ctx.retf()
    return
  ctx.oneByteW(0xCA, "retf", imm)

proc int3*(ctx: Assembler) =
  ctx.oneByte(0xCC, "int 3")
  
proc int0*(ctx: Assembler) =
  ctx.oneByte(0xCE, "int0")

proc intx*(ctx: Assembler, imm: int) =
  ctx.oneByteB(0xCD, "int", imm)

proc lea*(ctx: Assembler, opr1, opr2: TReg, disp: int) =
  let opr1Type = getRegType(opr1)
  if ctx.bits == BITS64:
    doAssert(opr1Type in {REG16, REG32, REG64})
  else:
    doAssert(opr1Type in {REG16, REG32})
    
  let size = typeToSize(opr1Type)
  ctx.lit("lea", opr1, opr2, disp)
  ctx.emit(0x8D, size, opr2, disp, opr1)
      
proc lea*(ctx: Assembler, opr, base, index: TReg, scale, disp: int) =  
  let oprType = getRegType(opr)
  let size = typeToSize(oprType)
  if ctx.bits == BITS64:
    doAssert(oprType in {REG16, REG32, REG64})
  else:
    doAssert(oprType in {REG16, REG32})
  
  ctx.lit("lea", opr, base, index, scale, disp)  
  ctx.emit(0x8D, size, base, index, scale, disp, opr)

proc lea*(ctx: Assembler, opr, index: TReg, scale, disp: int) =
  ctx.lea(opr, NA, index, scale, disp)

proc mov*(ctx: Assembler, opr1, opr2: TReg) =
  ctx.lit("mov", opr1, opr2)
  
  let opr1Type = getRegType(opr1)
  let opr2Type = getRegType(opr2)
  var opCode = 0x88 + (opr1Type != REG8).int
  
  if opr2Type == REGSEG:
    opCode = 0x8C
    if ctx.bits == BITS64:
      doAssert(opr1Type in {REG16, REG32, REG64})
    else:
      doAssert(opr1Type in {REG16, REG32})
    let rtype = if opr1Type == REG64: REG32 else: opr1Type
    ctx.emit(opCode, reg(getRegVal(opr1), rtype), opr2)
    return
  
  if opr1Type == REGSEG:
    opCode = 0x8E
    if ctx.bits == BITS64:
      doAssert(opr2Type in {REG16, REG32, REG64})
    else:
      doAssert(opr2Type in {REG16, REG32})
    ctx.emit(opCode, reg(getRegVal(opr2), REG32), opr1)
    return
    
  ctx.emit(opCode, opr1, opr2)
  
proc mov*(ctx: Assembler, size: oprSize, opr1: TReg, disp: int, opr2: TReg) =
  var opCode = 0x88 + (size != BYTE).int
  
  if getRegType(opr2) == REGSEG:
    opCode = 0x8C
    if ctx.bits == BITS64:
      doAssert(size in {WORD, QWORD})
      doAssert(getRegType(opr1) in {REG32, REG64})
    else:
      doAssert(size == WORD)
      doAssert(getRegType(opr1) == REG32)
    ctx.lit("mov", size, opr1, disp, opr2)
    ctx.emit(opCode, size, opr1, disp, opr2)
    return
  
  ctx.stdRMREG("mov", opCode, size, opr1, disp, opr2)

proc mov*(ctx: Assembler, opr1, opr2: TReg, size: oprSize, disp: int) =
  var opCode = 0x8A + (size != BYTE).int
  
  if getRegType(opr1) == REGSEG:
    opCode = 0x8E
    if ctx.bits == BITS64:
      doAssert(size in {WORD, QWORD})
      doAssert(getRegType(opr2) in {REG32, REG64})
    else:
      doAssert(size == WORD)
      doAssert(getRegType(opr2) == REG32)
    ctx.lit("mov", opr1, opr2, size, disp)
    ctx.emit(opCode, size, opr2, disp, opr1)
    return
  
  ctx.stdREGRM("mov", opCode, opr1, opr2, size, disp)
      
proc mov*(ctx: Assembler, size: oprSize, base, index: TReg, scale, disp: int, opr2: TReg) =
  var opCode = 0x88 + (size != BYTE).int
  
  if getRegType(opr2) == REGSEG:
    opCode = 0x8C
    if ctx.bits == BITS64:
      doAssert(size in {WORD, QWORD})
    else:
      doAssert(size == WORD)
    ctx.lit("mov", size, base, index, scale, disp, opr2)
    ctx.emit(opCode, size, base, index, scale, disp, opr2)
    return
  
  ctx.stdRMREG("mov", opCode, size, base, index, scale, disp, opr2)

proc mov*[B: int | TReg](ctx: Assembler, size: oprSize, index: TReg, scale, disp: int, imm: B) =
  ctx.mov(size, NA, index, scale, disp, imm)

proc mov*(ctx: Assembler, opr1: TReg, size: oprSize, base, index: TReg, scale, disp: int) =
  var opCode = 0x8A + (size != BYTE).int
  
  if getRegType(opr1) == REGSEG:
    opCode = 0x8E
    if ctx.bits == BITS64:
      doAssert(size in {WORD, QWORD})
    else:
      doAssert(size == WORD)
    ctx.lit("mov", opr1, size, base, index, scale, disp)
    ctx.emit(opCode, size, base, index, scale, disp, opr1)
    return
    
  ctx.stdREGRM("mov", opCode, opr1, size, base, index, scale, disp)

proc mov*(ctx: Assembler, opr1: TReg, size: oprSize, index: TReg, scale, disp: int) =
  ctx.mov(opr1, size, NA, index, scale, disp)

proc mov*(ctx: Assembler, size: oprSize, opr: TReg, disp, imm: int) =
  ctx.stdRMIMM("mov", 0xC6 + (size != BYTE).int, 0, size, opr, disp, imm)
  
proc mov*(ctx: Assembler, size: oprSize, base, index: TReg, scale, disp: int, imm: int) =
  ctx.stdRMIMM("mov", 0xC6 + (size != BYTE).int, 0, size, base, index, scale, disp, imm)

proc mov*(ctx: Assembler, opr: TReg, imm: int64) =
  ctx.lit("mov", opr, imm)
  var data = ""
  var oprType = getRegType(opr)
  let oprVal = getRegVal(opr)
  let opCode = if oprType == REG8: 0xB0 else: 0xB8
  
  if ctx.bits == BITS32:
    doAssert(oprVal < 0x08)
    doAssert(oprType in {REG8, REG16, REG32})
  else:
    doAssert(oprType in {REG8, REG16, REG32, REG64})
  
  var rex = REX_BASE
  var useREX = false
  
  if oprType == REG64:
    if imm < 0xFFFFFFFF: oprType = REG32
    
  if oprType == REG16:
    data.add chr(0x66)
  elif oprType == REG64:
    rex = rex or REX_W
    useREX = true
    
  if oprVal >= 0x08:
    rex = rex or REX_B
    useREX = true
    
  if useREX: data.add chr(rex)
  data.add chr(opCode + (oprVal and 0b0000_0111))
  var inst = Instruction(data: data)
  ctx.add inst

  if oprType == REG8:
    doAssert(imm <= 0xFF)
    ctx.appendChar(chr(imm.int and 0xFF))
  elif oprType == REG16:
    doAssert(imm <= 0xFFFF)
    ctx.appendWord(imm and 0xFFFF)
  elif oprType == REG32:
    doAssert(imm <= 0xFFFFFFFF)
    ctx.appendDWord(imm and 0xFFFFFFFF)
  else:
    ctx.appendQWord(imm)
    
#all other moffset
#xchg
#test
#mov rm, moffs
#mov moffs, rm
#imul reg, rm, imm
#jmp