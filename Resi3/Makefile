FILES = Makefile resi3.f
OBJECTS = absf.o binset.o covset.o iloc.o kemset.o parind.o \
	parlib.o parold.o pivot.o resa.o resi3.o sqrtf.o squp.o \
	unresa.o weight.o indout.o logo.o exsing.o
LIBS = 
FFLAGS =  -v
HOMEDIR = $$HOME
BINDIR  = $(HOMEDIR)/bin4
SRCDIR  = $(HOMEDIR)/src

resi3:	$(OBJECTS)
	f77 $(FFLAGS)  -o resi3 $(OBJECTS) $(LIBS)
	size resi3

clean:
	rm *.o

install:
	@size resi3 
	install -s resi3 $(BINDIR)

print:	$(FILES)
	cat $? | pr | lpr
	touch print

arch:
	ar uv $(SRCDIR)/resi3.a $(FILES)
	ranlib resi3.a

docsprint:

