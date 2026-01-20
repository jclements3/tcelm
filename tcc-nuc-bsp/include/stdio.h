/*
 * stdio.h - Standard I/O for TCC-NUC-BSP
 *
 * Minimal implementation for RTEMS console output
 */

#ifndef _STDIO_H
#define _STDIO_H

#include <stddef.h>
#include <stdarg.h>

/* File type (opaque) */
typedef struct _FILE FILE;

/* Standard streams */
extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

/* EOF marker */
#define EOF (-1)

/* Seek origins */
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

/* Buffer modes */
#define _IOFBF 0
#define _IOLBF 1
#define _IONBF 2

/* Buffer size */
#define BUFSIZ 1024

/* Formatted output */
int printf(const char *format, ...);
int fprintf(FILE *stream, const char *format, ...);
int sprintf(char *str, const char *format, ...);
int snprintf(char *str, size_t size, const char *format, ...);
int vprintf(const char *format, va_list ap);
int vfprintf(FILE *stream, const char *format, va_list ap);
int vsprintf(char *str, const char *format, va_list ap);
int vsnprintf(char *str, size_t size, const char *format, va_list ap);

/* Formatted input */
int scanf(const char *format, ...);
int fscanf(FILE *stream, const char *format, ...);
int sscanf(const char *str, const char *format, ...);

/* Character I/O */
int fgetc(FILE *stream);
int fputc(int c, FILE *stream);
int getc(FILE *stream);
int putc(int c, FILE *stream);
int getchar(void);
int putchar(int c);
int ungetc(int c, FILE *stream);

/* String I/O */
char *fgets(char *s, int size, FILE *stream);
int fputs(const char *s, FILE *stream);
char *gets(char *s);
int puts(const char *s);

/* File operations */
FILE *fopen(const char *path, const char *mode);
FILE *freopen(const char *path, const char *mode, FILE *stream);
int fclose(FILE *stream);
int fflush(FILE *stream);

/* Binary I/O */
size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);

/* File positioning */
int fseek(FILE *stream, long offset, int whence);
long ftell(FILE *stream);
void rewind(FILE *stream);
int fgetpos(FILE *stream, long *pos);
int fsetpos(FILE *stream, const long *pos);

/* Error handling */
int feof(FILE *stream);
int ferror(FILE *stream);
void clearerr(FILE *stream);
void perror(const char *s);

/* File removal */
int remove(const char *pathname);
int rename(const char *oldpath, const char *newpath);

#endif /* _STDIO_H */
