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
print STDOUT "sed -i 's/manpages rdf//' Makefile\n";
print STDOUT "sed -i 's/LDFLAGS\s*=/& -shared/g' Makefile\n";
print STDOUT "sed -i 's/.exe/.dll/g' Makefile\n";