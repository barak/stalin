#define CHAR char
#define FIXNUM int
#define FLOAT float
#define DOUBLE double
#define LENGTH unsigned
#define TAG unsigned
#define SQUISHED unsigned
#define SQUISHED64 unsigned long
#define SIGNEDSQUISHED int
#define SIGNEDSQUISHED64 long

#define CHARNAME "char"
#define FIXNUMNAME "int"
#define FLOATNAME "float"
#define DOUBLENAME "double"
#define LENGTHNAME "unsigned"
#define TAGNAME "unsigned"
#define SQUISHEDNAME "unsigned"
#define SQUISHED64NAME "unsigned long"
#define SIGNEDSQUISHEDNAME "int"
#define SIGNEDSQUISHED64NAME "long"

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <setjmp.h>

int panic(void);
int lg(int);

int panic(void)
{ fprintf(stderr, "Can't compute alignment\n");
  exit(-1);
  return -1;}

int lg(int o)
{ return o==16?4:o==8?3:o==4?2:o==2?1:o==1?0:panic();}

int main(int argc, char **argv)
{ if (argc!=2)
  { fprintf(stderr, "usage: stalin-architecture <architecture-name>\n");
    exit(-1);}
  if (!(((sizeof(void *)==sizeof(SQUISHED))&&
	 (sizeof(void *)==sizeof(SIGNEDSQUISHED)))||
	((sizeof(void *)==sizeof(SQUISHED64))&&
	 (sizeof(void *)==sizeof(SIGNEDSQUISHED64)))))
  { fprintf(stderr, "Stalin cannot run on this architecture because\n");
    fprintf(stderr, "pointer size is not the same as int or long\n");
    exit(-1);}
  system("uname -m");
  system("uname -s");
  printf(" (\"%s\"\n", argv[1]);
  printf("  \"%s\"\t\t\t\t;*char*\n", CHARNAME);
  printf("  \"%s\"\t\t\t\t\t;*fixnum*\n", FIXNUMNAME);
  printf("  \"%s\"\t\t\t\t;*flonum* when float\n", FLOATNAME);
  printf("  \"%s\"\t\t\t\t;*flonum* when double\n", DOUBLENAME);
  printf("  \"%s\"\t\t\t\t;*length*\n", LENGTHNAME);
  printf("  \"%s\"\t\t\t\t;*tag*\n", TAGNAME);
  printf("  \"%s\"\t\t\t\t;*squished*\n",
	 sizeof(void *)==sizeof(SQUISHED)?SQUISHEDNAME:SQUISHED64NAME);
  printf("  \"%s\"\t\t\t\t\t;*signed-squished*\n",
	 sizeof(void *)==sizeof(SIGNEDSQUISHED)?
	 SIGNEDSQUISHEDNAME:
	 SIGNEDSQUISHED64NAME);
  printf("  \"FILE\"\t\t\t\t;*file*\n");
  printf("  \"jmp_buf\"\t\t\t\t;*jmpbuf*\n");
  printf("  %d\t\t\t\t\t;*char-alignment*\n",
	 lg(offsetof(struct{char dummy; CHAR probe;}, probe)));
  printf("  %d\t\t\t\t\t;*fixnum-alignment*\n",
	 lg(offsetof(struct{char dummy; FIXNUM probe;}, probe)));
  printf("  %d\t\t\t\t\t;*flonum-alignment* when float\n",
	 lg(offsetof(struct{char dummy; FLOAT probe;}, probe)));
  printf("  %d\t\t\t\t\t;*flonum-alignment* when double\n",
	 lg(offsetof(struct{char dummy; DOUBLE probe;}, probe)));
  printf("  %d\t\t\t\t\t;*pointer-alignment*\n",
	 lg(offsetof(struct{char dummy; void *probe;}, probe)));
  printf("  %d\t\t\t\t\t;*length-alignment*\n",
	 lg(offsetof(struct{char dummy; LENGTH probe;}, probe)));
  printf("  %d\t\t\t\t\t;*tag-alignment*\n",
	 lg(offsetof(struct{char dummy; TAG probe;}, probe)));
  printf("  %d\t\t\t\t\t;*squished-alignment*\n",
	 sizeof(void *)==sizeof(SQUISHED)?
	 lg(offsetof(struct{char dummy; SQUISHED probe;}, probe)):
	 lg(offsetof(struct{char dummy; SQUISHED64 probe;}, probe)));
  printf("  %d\t\t\t\t\t;*file-alignment*\n",
	 lg(offsetof(struct{char dummy; FILE probe;}, probe)));
  printf("  %d\t\t\t\t\t;*jmpbuf-alignment*\n",
	 lg(offsetof(struct{char dummy; jmp_buf probe;}, probe)));
  printf("  %d\t\t\t\t\t;*char-size*\n", sizeof(CHAR));
  printf("  %d\t\t\t\t\t;*fixnum-size*\n", sizeof(FIXNUM));
  printf("  %d\t\t\t\t\t;*flonum-size* when float\n", sizeof(FLOAT));
  printf("  %d\t\t\t\t\t;*flonum-size* when double\n", sizeof(DOUBLE));
  printf("  %d\t\t\t\t\t;*pointer-size*\n", sizeof(void *));
  printf("  %d\t\t\t\t\t;*length-size*\n", sizeof(LENGTH));
  printf("  %d\t\t\t\t\t;*tag-size*\n", sizeof(TAG));
  printf("  %d\t\t\t\t\t;*squished-size*\n",
	 sizeof(void *)==sizeof(SQUISHED)?sizeof(SQUISHED):sizeof(SQUISHED64));
  /* This must be set by hand. */
  printf("  #f)\t\t\t\t\t;*include-malloc-for-alloca?*\n");
  return 0;}
