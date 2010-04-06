CC = gcc

OPTIONS = -d1 -d5 -d6 -On -t -c -db\
          -clone-size-limit 4 -split-even-if-no-widening\
          -do-not-align-strings\
          -treat-all-symbols-as-external\
          -do-not-index-constant-structure-types-by-expression\
          -do-not-index-allocated-structure-types-by-expression

# Stalin can be compiled with -freg-struct-return on most platforms. But gcc
# and egcs have a bug on Linux/Alpha that causes them to crash when given
# -freg-struct-return. ARCH_OPTS is set up apprpriately by ./build to handle
# this.

stalin: stalin.c
	$(CC) -o stalin -I./include -O3 -fomit-frame-pointer\
              -fno-strict-aliasing ${ARCH_OPTS}\
	      stalin.c -L./include -lm -lgc
	./post-make

stalin-architecture: stalin-architecture.c
	$(CC) -o stalin-architecture stalin-architecture.c

stalin.c: stalin.sc
	./stalin $(OPTIONS) stalin

stalin-IA32.c: stalin.sc
	./stalin $(OPTIONS) -architecture IA32 stalin
	mv -f stalin.c stalin-IA32.c

stalin-IA32-align-double.c: stalin.sc
	./stalin $(OPTIONS) -architecture IA32-align-double stalin
	mv -f stalin.c stalin-IA32-align-double.c

stalin-SPARC.c: stalin.sc
	./stalin $(OPTIONS) -architecture SPARC stalin
	mv -f stalin.c stalin-SPARC.c

stalin-SPARCv9.c: stalin.sc
	./stalin $(OPTIONS) -architecture SPARCv9 stalin
	mv -f stalin.c stalin-SPARCv9.c

stalin-SPARC64.c: stalin.sc
	./stalin $(OPTIONS) -architecture SPARC64 stalin
	mv -f stalin.c stalin-SPARC64.c

stalin-MIPS.c: stalin.sc
	./stalin $(OPTIONS) -architecture MIPS stalin
	mv -f stalin.c stalin-MIPS.c

stalin-Alpha.c: stalin.sc
	./stalin $(OPTIONS) -architecture Alpha stalin
	mv -f stalin.c stalin-Alpha.c

stalin-ARM.c: stalin.sc
	./stalin $(OPTIONS) -architecture ARM stalin
	mv -f stalin.c stalin-ARM.c

stalin-M68K.c: stalin.sc
	./stalin $(OPTIONS) -architecture M68K stalin
	mv -f stalin.c stalin-M68K.c

stalin-PowerPC.c: stalin.sc
	./stalin $(OPTIONS) -architecture PowerPC stalin
	mv -f stalin.c stalin-PowerPC.c

stalin-S390.c: stalin.sc
	./stalin $(OPTIONS) -architecture S390 stalin
	mv -f stalin.c stalin-S390.c

stalin-PowerPC64.c: stalin.sc
	./stalin $(OPTIONS) -architecture PowerPC64 stalin
	mv -f stalin.c stalin-PowerPC64.c

# Should ./include/stalin, ./stalin.c, and ./stalin only be deleted in a
# "distclean" target?
clean:
	rm -f ./include/gc.h
	rm -f ./include/gc_config_macros.h
	rm -f ./include/libgc.a
	rm -f ./include/libstalin.a
	rm -f ./include/libTmk.a
	rm -f ./include/stalin
	rm -f ./stalin.c
	rm -f ./stalin
