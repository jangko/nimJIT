import strutils

type
  reg8* = enum
    AL, CL, DL, BL, AH, CH, DH, BH, 
    R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B,
    XL
    
  reg16* = enum
    AX, CX, DX, BX, SP, BP, SI, DI,
    R8W, R9W, R10W, R11W, R12W, R13W, R14W, R15W,
    XX
    
  reg32* = enum
    EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI,
    R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D,
    EXX
    
  reg64* = enum
    RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
    R8, R9, R10, R11, R12, R13, R14, R15,
    RXX
    
  regfpu* = enum
    ST0, ST1, ST2, ST3, ST4, ST5, ST6, ST7, STX
    
  regmmx* = enum
    MMX0, MMX1, MMX2, MMX3, MMX4, MMX5, MMX6, MMX7, MMXX
    
  regxmm* = enum
    XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7,
    XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15,
    XMMX
   
  regymm* = enum
    YMM0, YMM1, YMM2, YMM3, YMM4, YMM5, YMM6, YMM7,
    YMM8, YMM9, YMM10, YMM11, YMM12, YMM13, YMM14, YMM15,
    YMMX
   
  regcr* = enum
    CR0, CR1, CR2, CR3, CR4, CR5, CR6, CR7,
    CR8, CR9, CR10, CR11, CR12, CR13, CR14, CR15,
    CRX
    
  regdr* = enum
    DR0, DR1, DR2, DR3, DR4, DR5, DR6, DR7,
    DR8, DR9, DR10, DR11, DR12, DR13, DR14, DR15,
    DRX
    
  regseg* = enum
    ES, CS, SS, DS, FS, GS,
    XS
    
  oprSize* = enum
    BYTE, WORD, DWORD, QWORD
    
  Instruction* = ref object
    data: string
    
  Assembler* = ref object
    instructionList: seq[Instruction]
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
  ctx.instructionList.add inst
  
proc add*(ctx: Assembler, lit: string) =
  ctx.literal.add lit
  
proc len(inst: Instruction): int =
  result = inst.data.len
  
proc newAssembler*(): Assembler = 
  new(result)
  result.instructionList = @[]
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
  for inst in ctx.instructionList:
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

proc emit(ctx: Assembler, opcode, regmod: int, opr: reg8) =
  var modrm = MOD11 or (regmod shl 3) or (opr.int and 0b0000_0111)
  var data = ""
  if opr >= R8B: data.add(chr(REX_BASE or REX_B))
  data.add chr(opcode)
  data.add chr(modrm)
  var inst = Instruction(data: data)
  ctx.add inst

proc emit(ctx: Assembler, opcode, regmod: int, opr: reg16) =
  var modrm = MOD11 or (regmod shl 3) or (opr.int and 0b0000_0111)
  var data = ""
  data.add chr(0x66)
  if opr >= R8W: data.add(chr(REX_BASE or REX_B))
  data.add chr(opcode)
  data.add chr(modrm)
  var inst = Instruction(data: data)
  ctx.add inst

proc emit(ctx: Assembler, opcode, regmod: int, opr: reg32) =
  var modrm = MOD11 or (regmod shl 3) or (opr.int and 0b0000_0111)
  var data = ""
  if opr >= R8D: data.add(chr(REX_BASE or REX_B))
  data.add chr(opcode)
  data.add chr(modrm)
  var inst = Instruction(data: data)
  ctx.add inst
 
proc emit(ctx: Assembler, opcode, regmod: int, opr: reg64) =
  var rex = REX_BASE or REX_W
  if opr >= R8: rex = rex or REX_B
  var modrm = MOD11 or (regmod shl 3) or (opr.int and 0b0000_0111)
  var data = ""
  data.add chr(rex)
  data.add chr(opcode)
  data.add chr(modrm)
  var inst = Instruction(data: data)
  ctx.add inst

proc emit(ctx: Assembler, opcode, regmod: int, size: oprSize, opr: reg32, disp: int) =
  var data = ""
  var useREX = false
  var rex = REX_BASE
  if size == WORD: data.add chr(0x66)
  data.add chr(0x67)  
  if opr >= R8D:
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
  modrm = modrm or (regmod shl 3) or (opr.int and 0b0000_0111)
  
  if (opr.int and 0b0000_0111) == 0b00_000_101:
    #EBP/R13D special case
    #transform it into opcode [reg + 0] using disp 0
    if disp >= 0 and disp <= 0xFF:
      modrm = MOD_01 or (regmod shl 3) or (opr.int and 0b0000_0111)
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
    var disple = cast[cstring](addr(disp32))
    data.add disple[0]
    data.add disple[1]
    data.add disple[2]
    data.add disple[3]
    
  var inst = Instruction(data: data)
  ctx.add inst

proc emit(ctx: Assembler, opcode, regmod: int, size: oprSize, opr: reg64, disp: int) =
  var data = ""
  var useREX = false
  var rex = REX_BASE

  if size == WORD: data.add chr(0x66)
  
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
  modrm = modrm or (regmod shl 3) or (opr.int and 0b0000_0111)
  
  if (opr.int and 0b0000_0111) == 0b00_000_101:
    #EBP/R13D special case
    #transform it into opcode [reg + 0] using disp 0
    if disp >= 0 and disp <= 0xFF:
      modrm = MOD_01 or (regmod shl 3) or (opr.int and 0b0000_0111)
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
    var disple = cast[cstring](addr(disp32))
    data.add disple[0]
    data.add disple[1]
    data.add disple[2]
    data.add disple[3]
    
  var inst = Instruction(data: data)
  ctx.add inst

proc scaleToScale(scale: int): int =
  case scale
  of 2: result = 0b0100_0000
  of 4: result = 0b1000_0000
  of 8: result = 0b1100_0000
  else: result = 0x00
      
proc emit(ctx: Assembler, opcode, regmod: int, size: oprSize, base, index: reg32, scale, disp: int) =
  var data = ""
  var useREX = false
  var rex = REX_BASE
  doAssert(scale in {1, 2, 4, 8})
  doAssert(index != ESP)
  
  if size == WORD: data.add chr(0x66)
  data.add chr(0x67)  
  if base >= R8D:
    rex = rex or REX_B
    useREX = true
  
  if index >= R8D:
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
  modrm = modrm or (regmod shl 3) or 0b0000_0100
  var sib = scaleToScale(scale) or ((index.int and 0b0000_0111) shl 3) or (base.int and 0b0000_0111)

  if (base.int and 0b0000_0111) == 0b00_000_101:
    #EBP/R13D special case
    if disp >= 0 and disp <= 0xFF:
      modrm = MOD_01 or (regmod shl 3) or 0b0000_0100
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
    var disple = cast[cstring](addr(disp32))
    data.add disple[0]
    data.add disple[1]
    data.add disple[2]
    data.add disple[3]
    
  var inst = Instruction(data: data)
  ctx.add inst
  
proc emit(ctx: Assembler, opcode, regmod: int, size: oprSize, base, index: reg64, scale, disp: int) =
  var data = ""
  var useREX = false
  var rex = REX_BASE
  doAssert(scale in {1, 2, 4, 8})
  doAssert(index != RSP)
  
  if size == WORD: data.add chr(0x66)
  
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
  modrm = modrm or (regmod shl 3) or 0b0000_0100
  
  var sib = scaleToScale(scale) or ((index.int and 0b0000_0111) shl 3) or (base.int and 0b0000_0111)

  if (base.int and 0b0000_0111) == 0b00_000_101:
    #EBP/R13D special case
    if disp >= 0 and disp <= 0xFF:
      modrm = MOD_01 or (regmod shl 3) or 0b0000_0100
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
    var disple = cast[cstring](addr(disp32))
    data.add disple[0]
    data.add disple[1]
    data.add disple[2]
    data.add disple[3]
    
  var inst = Instruction(data: data)
  ctx.add inst
  
proc neg*(ctx: Assembler, opr: reg8) = ctx.emit(0xF6, 3, opr)
proc neg*(ctx: Assembler, opr: reg16) = ctx.emit(0xF7, 3, opr)
proc neg*(ctx: Assembler, opr: reg32) = ctx.emit(0xF7, 3, opr)
proc neg*(ctx: Assembler, opr: reg64) = ctx.emit(0xF7, 3, opr)
proc neg*(ctx: Assembler, size: oprSize, opr: reg32, disp: int = 0) = 
  ctx.emit(0xF6 + (size != BYTE).int, 3, size, opr, disp)
  
proc neg*(ctx: Assembler, size: oprSize, opr: reg64, disp: int = 0) = 
  ctx.emit(0xF6 + (size != BYTE).int, 3, size, opr, disp)
  
proc neg*(ctx: Assembler, size: oprSize, base, index: reg32, scale, disp: int = 0) = 
  ctx.emit(0xF6 + (size != BYTE).int, 3, size, base, index, scale, disp)

proc neg*(ctx: Assembler, size: oprSize, base, index: reg64, scale, disp: int = 0) = 
  ctx.emit(0xF6 + (size != BYTE).int, 3, size, base, index, scale, disp)
  
proc `not`*(ctx: Assembler, opr: reg8) = ctx.emit(0xF6, 2, opr)
proc `not`*(ctx: Assembler, opr: reg16) = ctx.emit(0xF7, 2, opr)
proc `not`*(ctx: Assembler, opr: reg32) = ctx.emit(0xF7, 2, opr)
proc `not`*(ctx: Assembler, opr: reg64) = ctx.emit(0xF7, 2, opr)
proc `not`*(ctx: Assembler, size: oprSize, opr: reg32, disp: int = 0) = 
  ctx.emit(0xF6 + (size != BYTE).int, 2, size, opr, disp)
  
proc `not`*(ctx: Assembler, size: oprSize, opr: reg64, disp: int = 0) = 
  ctx.emit(0xF6 + (size != BYTE).int, 2, size, opr, disp)

proc `not`*(ctx: Assembler, size: oprSize, base, index: reg32, scale, disp: int = 0) = 
  ctx.emit(0xF6 + (size != BYTE).int, 2, size, base, index, scale, disp)

proc `not`*(ctx: Assembler, size: oprSize, base, index: reg64, scale, disp: int = 0) = 
  ctx.emit(0xF6 + (size != BYTE).int, 2, size, base, index, scale, disp)
  
proc mul*(ctx: Assembler, opr: reg8) = ctx.emit(0xF6, 4, opr)
proc mul*(ctx: Assembler, opr: reg16) = ctx.emit(0xF7, 4, opr)
proc mul*(ctx: Assembler, opr: reg32) = ctx.emit(0xF7, 4, opr)
proc mul*(ctx: Assembler, opr: reg64) = ctx.emit(0xF7, 4, opr)
proc mul*(ctx: Assembler, size: oprSize, opr: reg32, disp: int = 0) = 
  ctx.emit(0xF6 + (size != BYTE).int, 4, size, opr, disp)
  
proc mul*(ctx: Assembler, size: oprSize, opr: reg64, disp: int = 0) = 
  ctx.emit(0xF6 + (size != BYTE).int, 4, size, opr, disp)

proc mul*(ctx: Assembler, size: oprSize, base, index: reg32, scale, disp: int = 0) = 
  ctx.emit(0xF6 + (size != BYTE).int, 4, size, base, index, scale, disp)

proc mul*(ctx: Assembler, size: oprSize, base, index: reg64, scale, disp: int = 0) = 
  ctx.emit(0xF6 + (size != BYTE).int, 4, size, base, index, scale, disp)
