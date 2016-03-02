proc nasm_call(mode: int, input: cstring, insize: int, outsize: var int, output: var cstring): int {.cdecl, importc: "nasm_call", dynlib: "nasm.dll".}

var outsize: int
var output: cstring

var src = """mov eax, [ebx + 10]
lea eax, [ebx + 100]
"""

var src2 = """mov eax, [ebx + 10]
lea eax, [ebx + 100]
add eax, ecx
"""

var src3 = """lea eax, [ebx + 10]
mov eax, [ebx + 100]
xor eax, ecx
"""

if nasm_call(64, src, src.len, outsize, output) == 0:
  let res = newString(outsize)
  copyMem(res.cstring, output, outsize)
  echo res
  
if nasm_call(32, src2, src2.len, outsize, output) == 0:
  let res = newString(outsize)
  copyMem(res.cstring, output, outsize)
  echo res
  
if nasm_call(64, src3, src3.len, outsize, output) == 0:
  let res = newString(outsize)
  copyMem(res.cstring, output, outsize)
  echo res
