#ifndef XSTDIO_H
#define XSTDIO_H

#include <stdio.h>

typedef enum {
  NO_ERROR,
  OPEN_ERROR,
  WRITE_ERROR,
  READ_ERROR
} ERR_CODE;

typedef struct XFILE {
  int idx;
  int bufsize;
  ERR_CODE err;
  int avail;
  int size;
  int pos;
  int readMode;
  char* body;
  char* name;
} XFILE;

extern XFILE xfs[];

#define xstdout (&xfs[0])
#define xstdin  (&xfs[1])
#define xstderr (&xfs[2])

/*If the XFILE is successfully opened, the function returns a pointer to a XFILE object that can be used to identify the stream on future operations.
Otherwise, a null pointer is returned.
On most library implementations, the errno variable is also set to a system-specific error code on failure.*/
XFILE* xfopen(const char* fname, const char* mode);

/*On success, a non-negative value is returned.
On error, the function returns EOF and sets the error indicator (ferror).*/
int xfputs(const char* str, XFILE* stream);

/*On success, the character written is returned.
If a writing error occurs, EOF is returned and the error indicator (ferror) is set.*/
int xputc(int c, XFILE* stream);

/*On success, the total number of characters written is returned.
If a writing error occurs, the error indicator (ferror) is set and a negative number is returned.
If a multibyte character encoding error occurs while writing wide characters, errno is set to EILSEQ and a negative number is returned.*/
int xfprintf(XFILE* stream, const char* format, ...);

/*If the stream is successfully closed, a zero value is returned.
On failure, EOF is returned.*/
int xfclose(XFILE* stream);

/*A zero value indicates success.
If an error occurs, EOF is returned and the error indicator is set (see ferror).*/
int xfflush(XFILE* stream);

/*A non-zero value is returned in the case that the error indicator associated with the stream is set.
Otherwise, zero is returned.*/
int xferror(XFILE* stream);

/*On success, the character written is returned.
If a writing error occurs, EOF is returned and the error indicator (ferror) is set.*/
int xfputc(int character, XFILE* stream);

/*The total number of elements successfully written is returned.
If this number differs from the count parameter, a writing error prevented the function from completing. In this case, the error indicator (ferror) will be set for the stream.
If either size or count is zero, the function returns zero and the error indicator remains unchanged.
size_t is an unsigned integral type.*/
size_t xfwrite(const void * ptr, size_t size, size_t count, XFILE* stream);

/*If successful, the function returns zero.
Otherwise, it returns non-zero value.
If a read or write error occurs, the error indicator (ferror) is set.*/
int xfseek(XFILE* stream, long int offset, int origin);

/*On success, the current value of the position indicator is returned.
On failure, -1L is returned, and errno is set to a system-specific positive value.*/
int xftell(XFILE* stream);

/*The total number of elements successfully read is returned.
If this number differs from the count parameter, either a reading error occurred or the end-of-XFILE was reached while reading. In both cases, the proper indicator is set, which can be checked with ferror and feof, respectively.
If either size or count is zero, the function returns zero and both the stream state and the content pointed by ptr remain unchanged.
size_t is an unsigned integral type.*/
size_t xfread(void * ptr, size_t size, size_t count, XFILE* stream);

/*On success, the function returns str.
If the end-of-XFILE is encountered while attempting to read a character, the eof indicator is set (feof). If this happens before any characters could be read, the pointer returned is a null pointer (and the contents of str remain unchanged).
If a read error occurs, the error indicator (ferror) is set and a null pointer is also returned (but the contents pointed by str may have changed).*/
char* xfgets(char* str, int num, XFILE* stream);

/*A non-zero value is returned in the case that the end-of-XFILE indicator associated with the stream is set.
Otherwise, zero is returned.*/
int xfeof(XFILE* stream);

/*If the XFILE is successfully reopened, the function returns the pointer passed as parameter stream, which can be used to identify the reopened stream.
Otherwise, a null pointer is returned.
On most library implementations, the errno variable is also set to a system-specific error code on failure.*/
XFILE* xfreopen(const char* filename, const char* mode, XFILE* stream);

/*If the XFILE is successfully deleted, a zero value is returned.
On failure, a nonzero value is returned.
On most library implementations, the errno variable is also set to a system-specific error code on failure.*/
int xremove(const char* filename);

void xrewind(XFILE* stream);

/*If successful, the function returns a stream pointer to the temporary XFILE created.
On failure, NULL is returned.*/
XFILE* xtmpfile(void);

/*On success, the total number of characters written is returned.
If a writing error occurs, the error indicator (ferror) is set and a negative number is returned.
If a multibyte character encoding error occurs while writing wide characters, errno is set to EILSEQ and a negative number is returned.*/
int xvfprintf(XFILE* stream, const char* format, va_list arg);

/*On success, the character read is returned (promoted to an int value).
The return type is int to accommodate for the special value EOF, which indicates failure:
If the position indicator was at the end-of-XFILE, the function returns EOF and sets the eof indicator (feof) of stream.
If some other reading error happens, the function also returns EOF, but sets its error indicator (ferror) instead.*/
int xfgetc(XFILE* stream);

/*On success, the character written is returned.
If a writing error occurs, EOF is returned and the error indicator (ferror) is set.*/
int xputchar(int character);

/*On success, a non-negative value is returned.
On error, the function returns EOF and sets the error indicator (ferror).*/
int xputs(const char* str);

/*On success, the character put back is returned.
If the operation fails, EOF is returned.*/
int xungetc(int character, XFILE* stream);

void xperror(const char* str);

int xmain(int argc, char **argv);
void init_nasm(void);
void preproc_init(void);
int nasm_call(int mode, const char* input, int insize, int* outsize, char** output);
#endif