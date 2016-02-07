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

  oprSize* = enum
    BYTE, WORD, DWORD, QWORD

  Instruction* = ref object
    data: string

  Assembler* = ref object
    instList: seq[Instruction]
    literal: seq[string]

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

proc add(ctx: Assembler, inst: Instruction) =
  ctx.instList.add inst

proc add*(ctx: Assembler, lit: string) =
  ctx.literal.add lit

proc len(inst: Instruction): int =
  result = inst.data.len

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
  
proc newAssembler*(): Assembler =
  new(result)
  result.instList = @[]
  result.literal = @[]

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

proc emit[T: reg8 | reg16 | reg32 | reg64](ctx: Assembler, opcode, regMod: int, opr: T) =
  var modrm = MOD11 or (regMod shl 3) or (opr.int and 0b0000_0111)
  var data = ""
  when T is reg8:
    if opr >= R8B: data.add(chr(REX_BASE or REX_B))
  elif T is reg16:
    data.add chr(0x66)
    if opr >= R8W: data.add(chr(REX_BASE or REX_B))
  elif T is reg32:
    if opr >= R8D: data.add(chr(REX_BASE or REX_B))
  else:
    var rex = REX_BASE or REX_W
    if opr >= R8: rex = rex or REX_B
    data.add chr(rex)

  data.add chr(opcode)
  data.add chr(modrm)
  var inst = Instruction(data: data)
  ctx.add inst

proc emit[T: reg32 | reg64](ctx: Assembler, opcode, regMod: int, size: oprSize, opr: T, disp: int) =
  var data = ""
  var useREX = false
  var rex = REX_BASE
  if size == WORD: data.add chr(0x66)

  when T is reg32:
    data.add chr(0x67)
    if opr >= R8D:
      rex = rex or REX_B
      useREX = true
  else:
    if opr >= R8:
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
  modrm = modrm or (regMod shl 3) or (opr.int and 0b0000_0111)

  if (opr.int and 0b0000_0111) == 0b00_000_101:
    #EBP/R13D special case
    #transform it into opcode [reg + 0] using disp 0
    if disp >= 0 and disp <= 0xFF:
      modrm = MOD_01 or (regMod shl 3) or (opr.int and 0b0000_0111)
      data.add chr(modrm)
      data.add chr(disp)
    else:
      data.add chr(modrm)
  elif (opr.int and 0b0000_0111) == 0b00_000_100:
    #ESP/R12D special case
    #transform it into opcode [reg * 0] using SIB
    data.add chr(modrm)
    data.add chr(0x24)
  else:
    data.add chr(modrm)

  if disp > 0 and disp <= 0xFF:
    if (opr.int and 0b0000_0111) != 0b00_000_101:
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

proc emit[T: reg32 | reg64, TT: reg8 | reg16 | reg32 | reg64](ctx: Assembler, opcode: int, 
  size: oprSize, opr: T, disp: int, opr2: TT) =
  var data = ""
  var useREX = false
  var rex = REX_BASE
  if size == WORD: data.add chr(0x66)

  when T is reg32:
    data.add chr(0x67)
    if opr >= R8D:
      rex = rex or REX_B
      useREX = true
  else:
    if opr >= R8:
      rex = rex or REX_B
      useREX = true

  if size == QWORD:
    rex = rex or REX_W
    useREX = true
  
  when TT is reg8:
    if opr2 >= R8B:
      rex = rex or REX_R
      useREX = true
  elif TT is reg16:
    if opr2 >= R8W:
      rex = rex or REX_R
      useREX = true
  elif TT is reg32:
    if opr2 >= R8D:
      rex = rex or REX_R
      useREX = true
  else:
    if opr2 >= R8:
      rex = rex or REX_R
      useREX = true

  when T is reg64 and TT is reg8:
    doAssert(not (opr2 in {AH, CH, DH, BH} and useREX))
  elif T is reg32 and TT is reg8:
    doAssert(not (opr2 in {AH, CH, DH, BH} and useREX))
          
  if useREX: data.add chr(rex)
  data.add chr(opcode)
  var modrm = 0
  if disp == 0: modrm = MOD_00
  elif disp > 0 and disp <= 0xFF: modrm = MOD_01
  elif disp > 0xFF: modrm = MOD_10
  modrm = modrm or ((opr2.int and 0b0000_0111) shl 3) or (opr.int and 0b0000_0111)

  if (opr.int and 0b0000_0111) == 0b00_000_101:
    #EBP/R13D special case
    #transform it into opcode [reg + 0] using disp 0
    if disp >= 0 and disp <= 0xFF:
      modrm = MOD_01 or ((opr2.int and 0b0000_0111) shl 3) or (opr.int and 0b0000_0111)
      data.add chr(modrm)
      data.add chr(disp)
    else:
      data.add chr(modrm)
  elif (opr.int and 0b0000_0111) == 0b00_000_100:
    #ESP/R12D special case
    #transform it into opcode [reg * 0] using SIB
    data.add chr(modrm)
    data.add chr(0x24)
  else:
    data.add chr(modrm)

  if disp > 0 and disp <= 0xFF:
    if (opr.int and 0b0000_0111) != 0b00_000_101:
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

proc emit[T: reg32 | reg64](ctx: Assembler, opcode, regMod: int, size: oprSize, base, index: T, scale, disp: int) =
  var data = ""
  var useREX = false
  var rex = REX_BASE
  doAssert(scale in {1, 2, 4, 8})

  when T is reg32:
    doAssert(index != ESP)
  else:
    doAssert(index != RSP)

  if size == WORD: data.add chr(0x66)

  when T is reg32:
    data.add chr(0x67)

    if base >= R8D:
      rex = rex or REX_B
      useREX = true

    if index >= R8D:
      rex = rex or REX_X
      useREX = true
  else:
    if base >= R8:
      rex = rex or REX_B
      useREX = true

    if index >= R8:
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
  var sib = scaleToScale(scale) or ((index.int and 0b0000_0111) shl 3) or (base.int and 0b0000_0111)

  if (base.int and 0b0000_0111) == 0b00_000_101:
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

  if(base.int and 0b0000_0111) != 0b00_000_101: data.add chr(sib)

  if disp > 0 and disp <= 0xFF:
    if (base.int and 0b0000_0111) != 0b00_000_101:
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

proc emit[T: reg32 | reg64, TT: reg8 | reg16 | reg32 | reg64](ctx: Assembler, opcode: int, 
  size: oprSize, base, index: T, scale, disp: int, opr2: TT) =
  var data = ""
  var useREX = false
  var rex = REX_BASE
  doAssert(scale in {1, 2, 4, 8})

  when T is reg32:
    doAssert(index != ESP)
  else:
    doAssert(index != RSP)

  if size == WORD: data.add chr(0x66)

  when T is reg32:
    data.add chr(0x67)

    if base >= R8D:
      rex = rex or REX_B
      useREX = true

    if index >= R8D:
      rex = rex or REX_X
      useREX = true
  else:
    if base >= R8:
      rex = rex or REX_B
      useREX = true

    if index >= R8:
      rex = rex or REX_X
      useREX = true

  if size == QWORD:
    rex = rex or REX_W
    useREX = true

  when TT is reg8:
    if opr2 >= R8B:
      rex = rex or REX_R
      useREX = true
  elif TT is reg16:
    if opr2 >= R8W:
      rex = rex or REX_R
      useREX = true
  elif TT is reg32:
    if opr2 >= R8D:
      rex = rex or REX_R
      useREX = true
  else:
    if opr2 >= R8:
      rex = rex or REX_R
      useREX = true
  
  when T is reg64 and TT is reg8:
    doAssert(not (opr2 in {AH, CH, DH, BH} and useREX))
  elif T is reg32 and TT is reg8:
    doAssert(not (opr2 in {AH, CH, DH, BH} and useREX))
  
  if useREX: data.add chr(rex)
  data.add chr(opcode)
  var modrm = 0
  if disp == 0: modrm = MOD_00
  elif disp > 0 and disp <= 0xFF: modrm = MOD_01
  elif disp > 0xFF: modrm = MOD_10
  modrm = modrm or ((opr2.int and 0b0000_0111) shl 3) or 0b0000_0100
  var sib = scaleToScale(scale) or ((index.int and 0b0000_0111) shl 3) or (base.int and 0b0000_0111)

  if (base.int and 0b0000_0111) == 0b00_000_101:
    #EBP/R13D special case
    if disp >= 0 and disp <= 0xFF:
      modrm = MOD_01 or ((opr2.int and 0b0000_0111) shl 3) or 0b0000_0100
      data.add chr(modrm)
      data.add chr(sib)
      data.add chr(disp)
    else:
      data.add chr(modrm)
      data.add chr(sib)
  else:
    data.add chr(modrm)

  if(base.int and 0b0000_0111) != 0b00_000_101: data.add chr(sib)

  if disp > 0 and disp <= 0xFF:
    if (base.int and 0b0000_0111) != 0b00_000_101:
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

macro singleOperandGroup(inst: untyped, opCode, regMod: int): stmt =
  let opc = opCode.intVal + 1
  result = quote do:
    proc `inst`*[T: reg8 | reg16 | reg32 | reg64](ctx: Assembler, opr: T) =
      when T is reg8: ctx.emit(`opCode`, `regMod`, opr)
      else: ctx.emit(`opc`, `regMod`, opr)

    proc `inst`*[T: reg32 | reg64](ctx: Assembler, size: oprSize, opr: T, disp: int = 0) =
      ctx.emit(`opcode` + (size != BYTE).int, `regMod`, size, opr, disp)

    proc `inst`*[T: reg32 | reg64](ctx: Assembler, size: oprSize, base, index: T, scale, disp: int = 0) =
      ctx.emit(`opcode` + (size != BYTE).int, `regMod`, size, base, index, scale, disp)

singleOperandGroup(inc, 0xFE, 0)
singleOperandGroup(dec, 0xFE, 1)
singleOperandGroup(`not`, 0xF6, 2)
singleOperandGroup(neg, 0xF6, 3)
singleOperandGroup(mul, 0xF6, 4)

macro shiftGroup(inst: untyped, opCode, regMod: int): stmt =
  let opc = opCode.intVal + 1
  result = quote do:
    proc `inst`*[A: reg8 | reg16 | reg32 | reg64, B: int | reg8](ctx: Assembler, opr: A, imm: B) =
      when B is int:
        let ext = if imm == 1: 0x10 else: 0
        when A is reg8: ctx.emit(`opCode` + ext, `regMod`, opr)
        else: ctx.emit(`opc` + ext, `regMod`, opr)
        if imm != 1: ctx.appendChar chr(imm)
      else:
        doAssert(imm == CL)
        when A is reg8: ctx.emit(`opCode` + 0x12, `regMod`, opr)
        else: ctx.emit(`opc` + 0x12, `regMod`, opr)

    proc `inst`*[A: reg32 | reg64, B: int | reg8](ctx: Assembler, size: oprSize, opr: A, disp: int = 0, imm: B) =
      when B is int:
        let ext = if imm == 1: 0x10 else: 0
        ctx.emit(`opcode` + (size != BYTE).int + ext, `regMod`, size, opr, disp)
        if imm != 1: ctx.appendChar chr(imm)
      else:
        doAssert(imm == CL)
        ctx.emit(`opcode` + (size != BYTE).int + 0x12, `regMod`, size, opr, disp)

    proc `inst`*[A: reg32 | reg64, B: int | reg8](ctx: Assembler, size: oprSize, base, index: A, scale, disp: int = 0, imm: B) =
      when B is int:
        let ext = if imm == 1: 0x10 else: 0
        ctx.emit(`opcode` + (size != BYTE).int + ext, `regMod`, size, base, index, scale, disp)
        if imm != 1: ctx.appendChar chr(imm)
      else:
        doAssert(imm == CL)
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
  result = quote do:
    proc `inst`*[T: reg8 | reg16 | reg32 | reg64](ctx: Assembler, opr: T, imm: int) =
      when T is reg8:
        if opr == AL:
          var data = ""
          data.add chr(0x04 + `opMod`)
          var inst = Instruction(data: data)
          ctx.add inst
        else:
          ctx.emit(0x80, `regMod`, opr)
        doAssert(imm <= 0xFF)
        ctx.appendChar chr(imm and 0xFF)
      elif T is reg16:
        if imm >= 0 and imm < 0xFF:
          ctx.emit(0x83, `regMod`, opr)
          ctx.appendChar chr(imm and 0xFF)
        else:
          if opr == AX:
            var data = "\x66"
            data.add chr(0x05 + `opMod`)
            var inst = Instruction(data: data)
            ctx.add inst
          else:
            ctx.emit(0x81, `regMod`, opr)
          doAssert(imm <= 0xFFFF)
          ctx.appendWord(imm and 0xFFFF)
      elif T is reg32:
        if imm >= 0 and imm < 0xFF:
          ctx.emit(0x83, `regMod`, opr)
          ctx.appendChar chr(imm and 0xFF)
        else:
          if opr == EAX:
            var data = ""
            data.add chr(0x05 + `opMod`)
            var inst = Instruction(data: data)
            ctx.add inst
          else:
            ctx.emit(0x81, `regMod`, opr)
          doAssert(imm <= 0xFFFFFFFF)
          ctx.appendDWord(imm and 0xFFFFFFFF)
      else:
        if imm >= 0 and imm < 0xFF:
          ctx.emit(0x83, `regMod`, opr)
          ctx.appendChar chr(imm and 0xFF)
        else:
          if opr == RAX:
            var data = ""
            var rex = REX_BASE or REX_W
            if opr >= R8: rex = rex or REX_B
            data.add chr(rex)
            data.add chr(0x05 + `opMod`)
            var inst = Instruction(data: data)
            ctx.add inst
          else:
            ctx.emit(0x81, `regMod`, opr)
          doAssert(imm <= 0xFFFFFFFF)
          ctx.appendDWord(imm and 0xFFFFFFFF)

    proc `inst`*[T: reg32 | reg64](ctx: Assembler, size: oprSize, opr: T, disp: int = 0, imm: int) =
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
    
    proc `inst`*[T: reg32 | reg64, TT: reg8 | reg16 | reg32 | reg64](ctx: Assembler, size: oprSize, 
      opr1: T, disp: int, opr2: TT) =
      when TT is reg8:
        doAssert(size == BYTE)
      elif TT is reg16:
        doAssert(size == WORD)
      elif TT is reg32:
        doAssert(size == DWORD)
      else:
        doAssert(size == QWORD)
      ctx.emit(0x00 + (size != BYTE).int + `opMod`, size, opr1, disp, opr2)
      
    proc `inst`*[T: reg32 | reg64, TT: reg8 | reg16 | reg32 | reg64](ctx: Assembler, opr1: TT, 
      size: oprSize, opr2: T, disp: int = 0) =
      when TT is reg8:
        doAssert(size == BYTE)
      elif TT is reg16:
        doAssert(size == WORD)
      elif TT is reg32:
        doAssert(size == DWORD)
      else:
        doAssert(size == QWORD)
      ctx.emit(0x02 + (size != BYTE).int + `opMod`, size, opr2, disp, opr1)
      
    proc `inst`*[T: reg32 | reg64](ctx: Assembler, size: oprSize, base, index: T, scale, disp: int = 0, imm: int) =
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
      
    proc `inst`*[T: reg32 | reg64, TT: reg8 | reg16 | reg32 | reg64](ctx: Assembler, size: oprSize, 
      base, index: T, scale, disp: int = 0, opr2: TT) =
      when TT is reg8:
        doAssert(size == BYTE)
      elif TT is reg16:
        doAssert(size == WORD)
      elif TT is reg32:
        doAssert(size == DWORD)
      else:
        doAssert(size == QWORD)
      ctx.emit(0x00 + (size != BYTE).int + `opMod`, size, base, index, scale, disp, opr2)
    
    proc `inst`*[T: reg32 | reg64, TT: reg8 | reg16 | reg32 | reg64](ctx: Assembler, opr1: TT, 
      size: oprSize, base, index: T, scale, disp: int = 0) =
      when TT is reg8:
        doAssert(size == BYTE)
      elif TT is reg16:
        doAssert(size == WORD)
      elif TT is reg32:
        doAssert(size == DWORD)
      else:
        doAssert(size == QWORD)
      ctx.emit(0x02 + (size != BYTE).int + `opMod`, size, base, index, scale, disp, opr1)
    
arithGroup(add, 0x00, 0)
arithGroup(`or`, 0x08, 1)
arithGroup(adc, 0x10, 2)
arithGroup(sbb, 0x18, 3)
arithGroup(`and`,0x20, 4)
arithGroup(sub, 0x28, 5)
arithGroup(`xor`, 0x30, 6)
arithGroup(cmp, 0x38, 7)