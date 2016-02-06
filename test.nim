import strutils, nimjit, osproc, os

template genreg1(T: typedesc) =
  for reg in T.low.. <T.high:
    let lit = "$1 $2" % [inst, $reg]
    lst.write lit & "\n"
    nim.write "ctx.$1($2)\n" % [niminst, $reg]
    nim.write "ctx.add(\"$1\")\n" % [lit]

proc test(lst, nim: File, inst, niminst: string) =
  genreg1(reg8)
  genreg1(reg16)
  genreg1(reg32)
  genreg1(reg64)
    
template genreg2(T: typedesc) =
  for reg in T.low.. <T.high:
    let lit = "$1 $2 [$3]" % [inst, size, $reg]
    lst.write lit & "\n"
    nim.write "ctx.$1(nimjit.$2, $3)\n" % [niminst, size, $reg]
    nim.write "ctx.add(\"$1\")\n" % [lit]
    
template genreg22(T: typedesc) =
  for base in T.low.. <T.high:
    for index in T.low.. <T.high:
      for scale in {1, 2, 4, 8}:
        when T is reg32:
          if index == ESP: continue
        else:
          if index == RSP: continue
        let lit = "$1 $2 [$3 + $4 * $5]" % [inst, size, $base, $index, $scale]
        lst.write lit & "\n"
        nim.write "ctx.$1(nimjit.$2, $3, $4, $5)\n" % [niminst, size, $base, $index, $scale]
        nim.write "ctx.add(\"$1\")\n" % [lit]
    
proc test(lst, nim: File, inst: string, niminst, size: string) =
  genreg2(reg32)
  genreg2(reg64)
  genreg22(reg32)
  genreg22(reg64)
  
template genreg3(T: typedesc) =
  for reg in T.low.. <T.high:
    let lit = "$1 $2 [$3 + $4]" % [inst, size, $reg, $disp]
    lst.write lit & "\n"
    nim.write "ctx.$1(nimjit.$2, $3, $4)\n" % [niminst, size, $reg, $disp]
    nim.write "ctx.add(\"$1\")\n" % [lit]

template genreg32(T: typedesc) =
  for base in T.low.. <T.high:
    for index in T.low.. <T.high:
      for scale in {1, 2, 4, 8}:
        when T is reg32:
          if index == ESP: continue
        else:
          if index == RSP: continue
        let lit = "$1 $2 [$3 + $4 * $5 + $6]" % [inst, size, $base, $index, $scale, $disp]
        lst.write lit & "\n"
        nim.write "ctx.$1(nimjit.$2, $3, $4, $5, $6)\n" % [niminst, size, $base, $index, $scale, $disp]
        nim.write "ctx.add(\"$1\")\n" % [lit]
    
proc test(lst, nim: File, inst: string, niminst, size: string, disp: int) =
  #genreg3(reg32)
  #genreg3(reg64)
  genreg32(reg32)
  genreg32(reg64)
  
proc gen(lst, nim: File, inst, niminst: string) =
  #test(lst, nim, inst, niminst)
  #test(lst, nim, inst, niminst, "byte")
  #test(lst, nim, inst, niminst, "word")
  #test(lst, nim, inst, niminst, "dword")
  #test(lst, nim, inst, niminst, "qword")
  
  #test(lst, nim, inst, niminst, "byte", 10)
  #test(lst, nim, inst, niminst, "word", 10)
  #test(lst, nim, inst, niminst, "dword", 10)
  #test(lst, nim, inst, niminst, "qword", 10)
  
  test(lst, nim, inst, niminst, "byte", 3000)
  test(lst, nim, inst, niminst, "word", 3000)
  test(lst, nim, inst, niminst, "dword", 3000)
  test(lst, nim, inst, niminst, "qword", 3000)
  
var lst, nim: File 
if not open(lst, "asm.asm", fmWrite):
  echo "cannot open asm.asm"
  quit(-1)
  
if not open(nim, "asm.nim", fmWrite):
  echo "cannot open asm.nim"
  quit(-1)

nim.write "import nimjit, os\n"
nim.write "var ctx = newAssembler()\n"
gen(lst, nim, "neg", "neg")
gen(lst, nim, "not", "`not`")
gen(lst, nim, "mul", "mul")

nim.write "var output: File\n"
nim.write "if not open(output, paramStr(1), fmWrite):\n"
nim.write "  echo \"cannot open \" & paramStr(1)\n"
nim.write "  quit(-1)\n"
nim.write "ctx.listing(output)\n"

lst.close()
nim.close()

if execShellCmd("nasm -f win64 -l nasmasm.lst asm.asm") != 0:
  quit(-1)
  
if execShellCmd("nim c -d:release -o:nimjitasm.exe --verbosity:0 asm.nim") != 0:
  quit(-1)
  
if execShellCmd("nimjitasm nimjitasm.lst") != 0:
  quit(-1)
  
  
if not open(lst, "nasmasm.lst", fmRead):
  echo "cannot open nasmasm.lst"
  quit(-1)
  
if not open(nim, "nimjitasm.lst", fmRead):
  echo "cannot open nimjitasm.lst"
  quit(-1)

var line1 = newString(100)
var line2 = newString(100)

var lineno = 1
while true:
  if not lst.readLine(line1): break
  if not nim.readLine(line2): break
  if line1 != line2:
    echo "nasmasm.lst  : ", line1
    echo "nimjitasm.lst: ", line2
    quit(-1)
    
  inc lineno
  
lst.close()
nim.close()
echo "OK"
