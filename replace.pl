=pod
perl replace.pl > nasm/repl.sh
cd nasm
sh repl.sh
=cut

@fnlist = (
'fopen',
'fprintf',
'fclose',
'fputs',
'putc',
'fflush',
'ferror',
'fputc',
'fwrite',
'fseek',
'ftell',
'fread',
'fgets',
'feof',
'freopen',
'remove',
'rewind',
'tmpfile',
'vfprintf',
'fgetc',
'putchar',
'puts',
'ungetc',
'perror',
'stdout',
'stdin',
'stderr'
);

foreach $h (@fnlist) {
  print STDOUT "find . -type f -name \"*.c\" -exec sed -i 's/\\b${h}\\b/x${h}/g' {} +;\n";
}

print STDOUT "find . -type f -name \"*.c\" -exec sed -i 's/<stdio.h>/\\\"xstdio.h\\\"/' {} +;\n";
print STDOUT "find . -type f -name \"*.h\" -exec sed -i 's/<stdio.h>/\\\"xstdio.h\\\"/' {} +;\n";
print STDOUT "find . -type f -name \"*.c\" -exec sed -i 's/\\bFILE\\b/XFILE/g' {} +;\n";
print STDOUT "find . -type f -name \"*.h\" -exec sed -i 's/\\bFILE\\b/XFILE/g' {} +;\n";

print STDOUT "sed -i 's/main/xmain/g' nasm.c\n";
print STDOUT "sed -i 's/main/xmain/g' ndisasm.c\n";

print STDOUT "sed -i 's/NDISASM =/& xstdio.\$(O)/' Makefile\n";
print STDOUT "sed -i 's/NASM =/& xstdio.\$(O)/' Makefile\n";
print STDOUT "sed -i 's/ndisasm\$(X)\s+manpages\s+rdf//' Makefile\n";
print STDOUT "sed -i 's/LDFLAGS\s*=/& -shared/g' Makefile\n";
print STDOUT "sed -i 's/.exe/.dll/g' Makefile\n";

print STDOUT "echo \"void init_nasm() {\" >> nasm.c\n";
print STDOUT "echo \"  maxbits = 0;\" >> nasm.c\n";
print STDOUT "echo \"  globalrel = 0;\" >> nasm.c\n";
print STDOUT "echo \"  globalbnd = 0;\" >> nasm.c\n";
print STDOUT "echo \"  ofmt = &OF_DEFAULT;\" >> nasm.c\n";
print STDOUT "echo \"  ofmt_alias = NULL;\" >> nasm.c\n";
print STDOUT "echo \"  ofile = NULL;\" >> nasm.c\n";
print STDOUT "echo \"  optimizing = MAX_OPTIMIZE;\" >> nasm.c\n";
print STDOUT "echo \"  cmd_sb = 16;\" >> nasm.c\n";
print STDOUT "echo \"  depend_emit_phony = false;\" >> nasm.c\n";
print STDOUT "echo \"  depend_missing_ok = false;\" >> nasm.c\n";
print STDOUT "echo \"  depend_target = NULL;\" >> nasm.c\n";
print STDOUT "echo \"  depend_file = NULL;\" >> nasm.c\n";
print STDOUT "echo \"  user_nolist = 0;\" >> nasm.c\n";
print STDOUT "echo \"\}\" >> nasm.c\n";

print STDOUT "echo \"xstdio.\$(O): xstdio.c xstdio.h\" >> Makefile\n";


print STDOUT "echo \"void preproc_init(void) {\" >> preproc.c\n";
print STDOUT "echo \"  StackSize = 4;\" >> preproc.c\n";
print STDOUT "echo \"  StackPointer = \"ebp\";\" >> preproc.c\n";
print STDOUT "echo \"  ArgOffset = 8;\" >> preproc.c\n";
print STDOUT "echo \"  LocalOffset = 0;\" >> preproc.c\n";
print STDOUT "echo \"  ipath = NULL;\" >> preproc.c\n";
print STDOUT "echo \"  predef = NULL;\" >> preproc.c\n";
print STDOUT "echo \"  extrastdmac = NULL;\" >> preproc.c\n";
print STDOUT "echo \"  freeTokens = NULL;\" >> preproc.c\n";
print STDOUT "echo \"  blocks.next = NULL;\" >> preproc.c\n";
print STDOUT "echo \"  blocks.chunk = NULL;\" >> preproc.c\n";
print STDOUT "echo \"\}\" >> preproc.c\n";