import nimjit, os, strutils, osproc
var ctx = newAssembler(BITS32, true)
for OP2 in reg32.low..top[reg32](ctx):
  for index in reg32.low..top[reg32](ctx):
    for scale in {1, 2, 4, 8}:
      if index.int == ESP.int: continue
      ctx.mov(nimjit.dword, reg(index.int, REG32), scale, 300000, reg(OP2.int, REG32))
var output: File
if not open(output, paramStr(1), fmWrite):
  echo "cannot open " & paramStr(1)
  quit(-1)
ctx.listing(output)
output.close
var source: File
if not open(source, paramStr(2), fmWrite):
  echo "cannot open " & paramStr(2)
  quit(-1)
ctx.asmSource(source)
source.close
var cmd = "nasm -f win32 -l nasmasm.lst $1" % [paramStr(2)]
if execShellCmd(cmd) != 0: quit(-1)
