#-- flags
CXXFLAGS = -Wall -g

#-- files
SOURCES = ast.C cfg.C error.C instr.C reg.C symbol.C symtab.C type.C util.C \
ast.H cfg.H error.H instr.H reg.H symbol.H symtab.H type.H util.H \
tiny.l tiny.y Makefile read_int.s print_int.s read_chr.s print_chr.s run.sh

CCFILES = ast.C cfg.C error.C instr.C reg.C symbol.C symtab.C type.C util.C tiny.C
HFILES = ast.H cfg.H error.H instr.H reg.H symbol.H symtab.H type.H util.H tiny.H

OBJECTS = $(CCFILES:%.C=%.o)

PROGRAM=tiny

#-- rules
all:	$(PROGRAM)

lex.yy.c:	tiny.l
		flex tiny.l
tiny.C:	tiny.y
	bison -dv tiny.y -o tiny.C

lex.yy.o:	lex.yy.c
		gcc -c lex.yy.c -o lex.yy.o

tiny:	tiny.o lex.yy.o util.o error.o type.o symbol.o symtab.o instr.o ast.o cfg.o reg.o
	g++ $(CXXFLAGS) -o $@ $^

include makedeps

makedeps:	$(CCFILES) $(HFILES)
		g++ -M $(CCFILES) > $@
clean:
	rm -f $(OBJECTS)
	rm -f $(PROGRAM)
	rm -f makedeps
	rm -f lex.yy.c lex.yy.o
	rm -f tiny.C *~ tiny.output why.s

dist:	$(SOURCES)
	tar cf tiny.tar $(SOURCES)
