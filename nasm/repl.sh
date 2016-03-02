echo "void init_nasm() {" >> nasm.c
echo "  maxbits = 0;" >> nasm.c
echo "  globalrel = 0;" >> nasm.c
echo "  globalbnd = 0;" >> nasm.c
echo "  ofmt = &OF_DEFAULT;" >> nasm.c
echo "  ofmt_alias = NULL;" >> nasm.c
echo "  ofile = NULL;" >> nasm.c
echo "  optimizing = MAX_OPTIMIZE;" >> nasm.c
echo "  cmd_sb = 16;" >> nasm.c
echo "  depend_emit_phony = false;" >> nasm.c
echo "  depend_missing_ok = false;" >> nasm.c
echo "  depend_target = NULL;" >> nasm.c
echo "  depend_file = NULL;" >> nasm.c
echo "  user_nolist = 0;" >> nasm.c
echo "}" >> nasm.c
