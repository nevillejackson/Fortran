FILES = makefile pmain.f
OBJECTS = pivot.o absf.o pmain.o
LIBS = 
FFLAGS =  -v -g
FLAGS =  -v -g
DESTDIR = /usr/local

pmain:	$(OBJECTS)
	f77 $(FFLAGS)  -o pmain $(OBJECTS) $(LIBS)
	size pmain

clean:
	rm *.o pmain

install:
	@size pmain $(DESTDIR)/bin/pmain
	mv pmain $(DESTDIR)/bin

print:	$(FILES)
	cat $? | pr | lpr
	touch print

arch:
	ar uv $(DESTDIR)/src/pmain.a $(FILES)
	ranlib pmain.a

docsprint:

