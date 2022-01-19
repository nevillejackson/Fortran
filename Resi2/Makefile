FILES = Makefile resi2.f
OBJECTS = resi2.o
LIBS = -lskym
FFLAGS = -f -lskym
FLAGS = -f -v
DESTDIR = /usr/local

resi2:	$(OBJECTS)
	f77 $(FLAGS) $(LIBS) -o resi2 $(OBJECTS)
	size resi2

clean:
	rm *.o

install:
	@size resi2 $(DESTDIR)/bin/resi2
	mv resi2 $(DESTDIR)/bin

print:	$(FILES)
	cat $? | pr | lpr
	touch print

arch:
	ar uv $(DESTDIR)/src/resi2.a $(FILES)
	ranlib resi2.a

docsprint:

