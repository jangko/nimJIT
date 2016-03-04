#include "xstdio.h"
#include <stdio.h>
#include <stdarg.h>
#include "nasmlib.h"
#include <string.h>

#define UNUSED(x) (void)(x)

XFILE xfs[] = {
  {1, 0, NO_ERROR, 0, 0, 0, 1, 0, "stdout"},
  {2, 0, NO_ERROR, 0, 0, 0, 1, 0, "stdin"},
  {3, 0, NO_ERROR, 0, 0, 0, 1, 0, "stderr"},
  {4, 0, NO_ERROR, 0, 0, 0, 1, 0, "asm.o"},
  {5, 0, NO_ERROR, 0, 0, 0, 1, 0, "asm.lst"},
  {6, 0, NO_ERROR, 0, 0, 0, 1, 0, "asm.asm"},
  {7, 0, NO_ERROR, 0, 0, 0, 1, 0, 0},
  {8, 0, NO_ERROR, 0, 0, 0, 1, 0, 0},
  {9, 0, NO_ERROR, 0, 0, 0, 1, 0, 0},
  {10, 0, NO_ERROR, 0, 0, 0, 1, 0, 0}
};

#define ASM_O_IDX (3)
#define ASM_LST_IDX (4)
#define ASM_ASM_IDX (5)

static int counter = 100;
static int debug_on = 0;

static void init_xstdio(const char* input, int insize) {
  int i;
  XFILE* stream = &xfs[ASM_ASM_IDX];
  for(i = 0; i < 10; i++) {
    xfs[i].bufsize = 0;
    xfs[i].err = NO_ERROR;
    xfs[i].avail = 0;
    xfs[i].size = 0;
    xfs[i].body = 0;
    if(i > ASM_ASM_IDX) {
      xfs[i].name = 0;
    }
    
    xfs[i].pos = 0;
    xfs[i].readMode = 1;
  }
  
  xfopen("stdout", "w");
  xfopen("stdin", "r");
  xfopen("stderr", "w");
  
  stream->body = nasm_malloc(insize);
  stream->size = insize;
  stream->bufsize = insize;
  stream->avail = 1;
  memcpy(stream->body, input, insize);
}

static void deinit_xstdio() {
  int i;
  for(i = 0; i < 10; i++) {
    if(xfs[i].body != 0) {
      nasm_free(xfs[i].body);
    }
    
    if(i > ASM_ASM_IDX) {
      if(xfs[i].name != 0) {
        nasm_free(xfs[i].name);
      }
    }
  }
}

XFILE* xfopen(const char* fname, const char* mode) {
  int i;
  XFILE* stream = NULL;
    
  if(debug_on) printf("xfopen: %s %s\n", fname, mode);
  
  for(i = 0; i < 10; i++) {
    if(xfs[i].name != 0) {
      if(strcmp(xfs[i].name, fname) == 0) {
        stream = &xfs[i];
        break;
      }
    }
  }
    
  if(stream != NULL) {
    while(*mode) {
      if(*mode == 'w') {
        stream->readMode = 0;
        stream->bufsize = 0x80000;
        stream->size = 0;
        stream->body = nasm_malloc(stream->bufsize);
        stream->avail = 1;
        stream->pos = 0;
        /*printf("fopen write ok %s\n", stream->name);*/
        return stream;
      }
      if(*mode == 'r') {
        stream->readMode = 1;
        stream->pos = 0;
        if(stream->avail == 0) {
          return NULL;
        }
        /*printf("fopen read ok %s\n", stream->name);*/
        return stream;
      }
      mode++;
    }
  }
  
  if(stream == NULL) {
    for(i = 4; i < 10; i++) {
      if(xfs[i].name == 0) {
        stream = &xfs[i];
        break;
      }
    }
    
    if(stream != NULL) {
      while(*mode) {
        if(*mode == 'w') {
          stream->readMode = 0;
          stream->bufsize = 0x80000;
          stream->size = 0;
          stream->body = nasm_malloc(stream->bufsize);
          stream->avail = 1;
          stream->name = strdup(fname);
          /*printf("fopen write 2 ok %s\n", stream->name);*/
          return stream;
        }
        if(*mode == 'r') {
          return NULL;
        }
        mode++;
      }
    }
  }
  
  return stream;
}

int xfputs(const char* str, XFILE* stream) {
  int numWrite = 0;
  int len = strlen(str);
  
  /*printf("trace: %s %d\n", file, line);*/
  
  if(stream != NULL && str != NULL) {
    int idx = (int)(((size_t)stream - (size_t)&xfs[0]) / sizeof(xfs[0]));
    if(debug_on) printf("xfputs: %d %d, %s %s\n", idx, stream->idx, stream->name, str);
  
    /*if(stream->idx == 3) {
      printf("STDERR: %s\n", str);
    }*/
    
    if(stream->readMode == 1) {
      stream->err = WRITE_ERROR;
      return EOF;
    }
    
    if(stream->pos + len >= stream->bufsize) {
      stream->bufsize += 0x80000;
      stream->body = nasm_realloc(stream->body, stream->bufsize);
    }
    
    while(*str++) {
      stream->body[stream->pos++] = *str;
      numWrite++;
    }
    
    if(stream->pos > stream->size) stream->size = stream->pos;
    return numWrite;
  }
  
  return EOF;
}

int xputc(int c, XFILE* stream) {
  if(debug_on) printf("xputc: %s %d\n", stream->name, c);
  
  if(stream != NULL) {
    if(stream->readMode == 1) {
      stream->err = WRITE_ERROR;
      return EOF;
    }
    
    if(stream->pos + 1 >= stream->bufsize) {
      stream->bufsize += 0x80000;
      stream->body = nasm_realloc(stream->body, stream->bufsize);
    }
    
    stream->body[stream->pos++] = c;
    if(stream->pos > stream->size) stream->size = stream->pos;
    return c;
  }
  
  return EOF;
}

int xfprintf(XFILE* stream, const char* format, ...) {
  int ret = EOF;
  char buf[1000];
  
  /* Declare a va_list type variable */
  va_list arg;
  
  if(debug_on) printf("xfprintf: %s %s\n", stream->name, format);

  if(stream != NULL && format != NULL) {
    if(stream->readMode == 1) {
      stream->err = WRITE_ERROR;
      return EOF;
    }
  
    /* Initialise the va_list variable with the ... after fmt */
    va_start(arg, format);

    /* Forward the '...' to vprintf */
    ret = vsprintf(buf, format, arg);

    /* Clean up the va_list */
    va_end(arg);

    if(ret > 0) {
      /*if(stream->idx == 3) {
        printf("STDERR2: %s\n", buf);
      }*/
      
      /*printf("%d %d\n", stream->pos, stream->size);*/
      if(stream->pos + ret >= stream->bufsize) {
        stream->bufsize += 0x80000;
        stream->body = nasm_realloc(stream->body, stream->bufsize);
      }
      memcpy(&stream->body[stream->pos], buf, ret);
      stream->pos += ret;
      if(stream->pos > stream->size) stream->size = stream->pos;
    }
  }
  
  return ret;
}

int xfclose(XFILE* stream) {
  if(debug_on) printf("xfclose %s\n", stream->name);
  
  if(stream != NULL) {
    stream->err = NO_ERROR;
    return 0;
  }
  return EOF;
}

int xfflush(XFILE* stream) {
  if(debug_on) printf("xfflush %s\n", stream->name);
  if(stream != NULL) {
    return 0;
  }
  return EOF;
}

int xferror(XFILE* stream) {
  if(debug_on) printf("xferror %s\n", stream->name);
  if(stream != NULL) {
    return stream->err;
  }
  return EOF;
}

int xfputc(int character, XFILE* stream) {
  return xputc(character, stream);
}

size_t xfwrite(const void * ptr, size_t size, size_t count, XFILE* stream) {
  int len = size * count;
  if(debug_on) printf("xfwrite %s %d %d\n", stream->name, (int)size, (int)count);
  if(stream != NULL && ptr != NULL) {
    if(stream->readMode == 1) {
      stream->err = WRITE_ERROR;
      return EOF;
    }
      
    if(len > 0) {
      if(stream->pos + len >= stream->bufsize) {
        stream->bufsize += 0x80000;
        stream->body = nasm_realloc(stream->body, stream->bufsize);
      }
      memcpy(&stream->body[stream->pos], ptr, len);
      stream->pos += len;
      if(stream->pos > stream->size) stream->size = stream->pos;
    }
  }
  
  return len;
}

int xfseek(XFILE* stream, long int offset, int origin) {
  if(debug_on) printf("xfseek %s %d %d\n", stream->name, (int)offset, origin);
  
  if(stream != NULL) {
    
    switch(origin) {
      case SEEK_CUR:
        if (stream->pos + offset > stream->size || stream->pos + offset < 0) {
          return EOF;
        }
        stream->pos += offset;
      break;
      case SEEK_SET:
        if (offset < 0 || offset > stream->size) {
          return EOF;
        }
        stream->pos = offset;
      break;
      case SEEK_END:
        if(stream->size + offset > stream->size || stream->size + offset < 0) {
          return EOF;
        }
        stream->pos = stream->size + offset;
      break;
      default:
        return EOF;
    }
    
    return 0;
  }
  return EOF;
}

int xftell(XFILE* stream) {
  if(debug_on) printf("xftell %s\n", stream->name);
  if(stream != NULL) {
    return stream->pos;
  }
  return EOF;
}

size_t xfread(void * ptr, size_t size, size_t count, XFILE* stream) {
  int numRead = 0;
  
  if(debug_on) printf("xfread %s %d %d\n", stream->name, (int)size, (int)count);
  if(stream != NULL) {
    if(stream->readMode != 1) {
      stream->err = READ_ERROR;
      return EOF;
    }
    
    numRead = size * count;
    
    if(stream->pos + numRead > stream->size) {
      numRead = stream->size - stream->pos;
      memcpy(ptr, &stream->body[stream->pos], numRead);
      return numRead / size;
    }
    
    if(stream->pos + numRead < 0) {
      return EOF;
    }
    
    return count;
  }
  return EOF;
}

int xfeof(XFILE* stream) {
  if(debug_on) printf("xfeof %s\n", stream->name);
  if(stream != NULL) {
    return stream->pos == stream->size;
  }
  return 0;
}

char* xfgets(char* str, int num, XFILE* stream) {
  int i, c;
  
  if(debug_on) printf("xfgets %s %d\n", stream->name, num);
  for(i = 0; i < num; i++) {
    c = xfgetc(stream);
    if(c == '\n') {
      str[i] = c;
      str[i+1] = 0;
      return str;
    }
    
    if(c == EOF) {
      str[i] = 0;
      if(i == 0) {
        return NULL;
      } else {
        return str;
      }
    }
    
    str[i] = c;
  }
  
  str[num] = 0;
  return str;
}

XFILE* xfreopen(const char* filename, const char* mode, XFILE* stream) {
  UNUSED(filename);
  UNUSED(mode);
  return stream;
}

XFILE* xtmpfile(void) {
  char buf[20];
  counter++;
  sprintf(buf, "tmp_%d", counter);
  printf("xtmpfile %s\n", buf);
  return xfopen(buf, "w");
}

int xremove(const char* filename) {
  int i;
  XFILE* stream = NULL;
  
  if(debug_on) printf("xremove %s\n", filename);
  for(i = 0; i < 10; i++) {
    if(xfs[i].name != 0) {
      if(strcmp(xfs[i].name, filename) == 0) {
        stream = &xfs[i];
        if(i > ASM_ASM_IDX) {
          if(stream->name != 0) {
            nasm_free(stream->name);
            stream->name = 0;
          }
        }    
        break;
      }
    }
  }
  
  if(stream != NULL) {
    stream->readMode = 1;
    stream->bufsize = 0;
    stream->size = 0;
    if(stream->body) {
      nasm_free(stream->body);
      stream->body = 0;
    }
    stream->avail = 0;
    return 0;
  }
  return EOF;
}

void xrewind(XFILE* stream) {
  if(debug_on) printf("xrewind %s\n", stream->name);
  xfseek(stream, 0, SEEK_SET);
}

int xvfprintf(XFILE* stream, const char* format, va_list arg) {
  int ret = EOF;
  char buf[1000];

  if(debug_on) printf("xvfprintf %s %s\n", stream->name, format);
  if(stream != NULL && format != NULL) {
    if(stream->readMode == 1) {
      stream->err = WRITE_ERROR;
      return EOF;
    }
  
    /* Forward the '...' to vprintf */
    ret = vsprintf(buf, format, arg);

    if(ret > 0) {
      if(stream->pos + ret >= stream->bufsize) {
        stream->bufsize += 0x80000;
        stream->body = nasm_realloc(stream->body, stream->bufsize);
      }
      memcpy(&stream->body[stream->pos], buf, ret);
      stream->pos += ret;
      if(stream->pos > stream->size) stream->size = stream->pos;
    }
  }
  
  return ret;
}

int xfgetc(XFILE* stream) {
  if(debug_on) printf("xfgetc %s\n", stream->name);
  
  if(stream != NULL) {
    if(stream->readMode != 1) {
      stream->err = READ_ERROR;
      return EOF;
    }
    
    if(stream->pos + 1 > stream->size) {
      stream->err = READ_ERROR;
      return EOF;
    }
    
    return stream->body[stream->pos++];
  }
  return EOF;
}

int xputchar(int character) {
  return xputc(character, xstdout);
}

int xputs(const char* str) {
  int ret1 = xfputs(str, xstdout);
  int ret2 = xputc('\n', xstdout);
  if(ret1 == EOF || ret2 != EOF) {
    return EOF;
  }
  return ret1 + 1;
}

int xungetc(int character, XFILE* stream) {
  if(debug_on) printf("xungetc %s %d\n", stream->name, character);
  
  if(stream != NULL) {
    if(stream->pos == 0) {
      return EOF;
    }
    stream->pos--;
    stream->body[stream->pos] = character;
    return character;
  }
  return EOF;
}

void xperror(const char* str) {
  perror(str);
}

static int debug_on2 = 0;
int nasm_call(int mode, const char* input, int insize, int* outsize, char** output) {
  int result;
  int argc = 8;
  char* argv[8] = {"nasm", "-f", "win32", "-o", "asm.o", "-l", "asm.lst", "asm.asm"};
  XFILE* stream = &xfs[ASM_LST_IDX];
  
  if(mode == 32) {
    argv[2] = "win32";
  } else if(mode == 64) {
    argv[2] = "win64";
  } else {
    return -1;
  }
  
  if(input == NULL || insize == 0) {
    return -1;
  }
  
  /*printf("%d: %s\n", insize, input);*/
  
  if(debug_on2) printf("ok1\n");
  deinit_xstdio();
  
  if(debug_on2) printf("ok2\n");
  init_xstdio(input, insize);
  
  if(debug_on2) printf("ok3\n");
  init_nasm();
  preproc_init();
  
  if(debug_on2) printf("ok4\n");
  result = xmain(argc, argv);
  
  if(debug_on2) printf("ok5\n");
 
  *outsize = stream->size;
  *output = stream->body;
  
  if(xstderr->size > 0) {
    printf("STDERR: %s\n", xstderr->body);
  }
  return result;
}
