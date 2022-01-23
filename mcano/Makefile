FILES = Makefile mcano.f
OBJECTS = canor.o corre.o data.o eigen.o loc.o mcano.o minv.o\
	nroot.o statin.o
LIBS = 
FFLAGS =  -v -g
FLAGS =  -v 
DESTDIR = /home/nevj/bin

mcano:	$(OBJECTS)
	f77 $(FLAGS) -o mcano $(OBJECTS) $(LIBS)
	size mcano

clean:
	rm *.o

install:
	@size mcano 
	mv mcano $(DESTDIR)

print:	$(FILES)
	cat $? | pr | lpr
	touch print

arch:
	ar uv $(DESTDIR)/src/mcano.a $(FILES)
	ranlib mcano.a

docsprint:

