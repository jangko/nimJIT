import nimjit, os, strutils
var ctx = newAssembler(BITS64)
for OP2 in reg64.low..reg64.high:
  for base in reg64.low..reg64.high:
    for index in reg64.low..reg64.high:
      for scale in {1, 2, 4, 8}:
        if index.int == ESP.int: continue
        ctx.add(nimjit.qword, reg(base.int, REG64), reg(index.int, REG64), scale, 300000, reg(OP2.int, REG64))
        ctx.add("add qword [$1 + $2 * $3 + 300000], $4" % [$base, $index, $scale, $OP2])
var output: File
if not open(output, paramStr(1), fmWrite):
  echo "cannot open " & paramStr(1)
  quit(-1)
ctx.listing(output)
