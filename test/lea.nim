proc genLeaA(ctx: TestContext, inst, r, rr: string, disp = 0) =
  ctx.nim.write "for x in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  for y in $1.low..top[$1](ctx):\n" % [toLower(rr)]
  ctx.nim.write "    ctx.$1(reg(x.int, $2), reg(y.int, $3), $4)\n" % [inst, r, rr, $disp]
  
proc testLeaA(ctx: TestContext, inst: string, disp = 0) =
  ctx.genLeaA(inst, "REG16", "REG32", disp)
  ctx.genLeaA(inst, "REG32", "REG32", disp)
  if ctx.bits == BITS64: 
    ctx.genLeaA(inst, "REG64", "REG32", disp)
    ctx.genLeaA(inst, "REG16", "REG64", disp)
    ctx.genLeaA(inst, "REG32", "REG64", disp)
    ctx.genLeaA(inst, "REG64", "REG64", disp)
    
proc testLea1(ctx: TestContext, inst: string) =
  ctx.testLeaA(inst)
  ctx.testLeaA(inst, 10)
  ctx.testLeaA(inst, 300)
  ctx.testLeaA(inst, 70000)
  
proc genLeaB(ctx: TestContext, inst: string, r, rr: string, disp = 0) =
  ctx.nim.write "for opr in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  for base in $1.low..top[$1](ctx):\n" % [toLower(rr)]
  ctx.nim.write "    for index in $1.low..top[$1](ctx):\n" % [toLower(rr)]
  ctx.nim.write "      for scale in {1, 2, 4, 8}:\n"
  ctx.nim.write "        if index.int == ESP.int: continue\n"
  ctx.nim.write "        ctx.$1(reg(opr.int, $2), reg(base.int, $3), reg(index.int, $3), scale, $4)\n" % [inst, r, rr, $disp]

proc testLeaB(ctx: TestContext, inst: string, disp = 0) =
  ctx.genLeaB(inst, "REG16", "REG32", disp)
  ctx.genLeaB(inst, "REG32", "REG32", disp)
  if ctx.bits == BITS64: 
    ctx.genLeaB(inst, "REG64", "REG32", disp)
    ctx.genLeaB(inst, "REG16", "REG64", disp)
    ctx.genLeaB(inst, "REG32", "REG64", disp)
    ctx.genLeaB(inst, "REG64", "REG64", disp)

proc testLea2(ctx: TestContext, inst: string) =
  ctx.testLeaB(inst)
  ctx.testLeaB(inst, 10)
  ctx.testLeaB(inst, 300)
  ctx.testLeaB(inst, 70000)
    
proc genLeaC(ctx: TestContext, inst: string, r, rr: string, disp = 0) =
  ctx.nim.write "for opr in $1.low..top[$1](ctx):\n" % [toLower(r)]
  ctx.nim.write "  for index in $1.low..top[$1](ctx):\n" % [toLower(rr)]
  ctx.nim.write "    for scale in {1, 2, 4, 8}:\n"
  ctx.nim.write "      if index.int == ESP.int: continue\n"
  ctx.nim.write "      ctx.$1(reg(opr.int, $2), reg(index.int, $3), scale, $4)\n" % [inst, r, rr, $disp]

proc testLeaC(ctx: TestContext, inst: string, disp = 0) =
  ctx.genLeaC(inst, "REG16", "REG32", disp)
  ctx.genLeaC(inst, "REG32", "REG32", disp)
  if ctx.bits == BITS64: 
    ctx.genLeaC(inst, "REG64", "REG32", disp)
    ctx.genLeaC(inst, "REG16", "REG64", disp)
    ctx.genLeaC(inst, "REG32", "REG64", disp)
    ctx.genLeaC(inst, "REG64", "REG64", disp)

proc testLea3(ctx: TestContext, inst: string) =
  ctx.testLeaC(inst)
  ctx.testLeaC(inst, 10)
  ctx.testLeaC(inst, 300)
  ctx.testLeaC(inst, 70000)
  
proc genLea(ctx: TestContext, inst: string) =
  beginTest(inst):
    ctx.testLea1(inst)
    ctx.testLea2(inst)
    ctx.testLea3(inst)