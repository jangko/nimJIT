#include "xstdio.h"
#include <stdio.h>
#include <stdarg.h>

XFILE* xfopen(const char* fname, const char* mode) {
  return (XFILE*) fopen(fname, mode);
}

int xfputs(const char* str, XFILE* stream) {
  return fputs(str, (FILE*) stream);
}

int xputc(int c, XFILE* stream) {
  return putc(c, (FILE*) stream);
}

int xfprintf(XFILE* stream, const char* format, ...) {
  int ret;

  /* Declare a va_list type variable */
  va_list arg;

  /* Initialise the va_list variable with the ... after fmt */
  va_start(arg, format);

  /* Forward the '...' to vprintf */
  ret = vfprintf((FILE*) stream, format, arg);

  /* Clean up the va_list */
  va_end(arg);

  return ret;
}

int xfclose(XFILE* stream) {
  return fclose((FILE*) stream);
}

int xfflush(XFILE* stream) {
  return fflush((FILE*) stream);
}

int xferror(XFILE* stream) {
  return ferror((FILE*) stream);
}

int xfputc(int character, XFILE* stream) {
  return fputc(character, (FILE*) stream);
}

size_t xfwrite(const void * ptr, size_t size, size_t count, XFILE* stream) {
  return fwrite(ptr, size, count, (FILE*) stream);
}

int xfseek(XFILE* stream, long int offset, int origin) {
  return fseek((FILE*) stream, offset, origin);
}

int xftell(XFILE* stream) {
  return ftell((FILE*) stream);
}

size_t xfread(void * ptr, size_t size, size_t count, XFILE* stream) {
  return fread(ptr, size, count, (FILE*) stream);
}

char* xfgets(char* str, int num, XFILE* stream) {
  return fgets(str, num, (FILE*) stream);
}

int xfeof(XFILE* stream) {
  return feof((FILE*) stream);
}

XFILE* xfreopen(const char* filename, const char* mode, XFILE* stream) {
  return (XFILE*) freopen(filename, mode, (FILE*) stream);
}

int xremove(const char* filename) {
  return remove(filename);
}

void xrewind(XFILE* stream) {
  rewind((FILE*) stream);
}

XFILE* xtmpfile(void) {
  return (XFILE*) tmpfile();
}

int xvfprintf(XFILE* stream, const char* format, va_list arg) {
  return vfprintf((FILE*) stream, format, arg);
}

int xfgetc(XFILE* stream) {
  return fgetc((FILE*) stream);
}

int xputchar(int character) {
  return xputchar(character);
}

int xputs(const char* str) {
  return puts(str);
}

int xungetc(int character, XFILE* stream) {
  return ungetc(character, (FILE*) stream);
}

void xperror(const char* str) {
  perror(str);
}

int main(int argc, char **argv) {
  return xmain(argc, argv);
}