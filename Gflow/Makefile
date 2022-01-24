FILES = Makefile gflow.f pathse.f 
OBJECTS = gflow.o pathse.o
LIBS = -lskym
FFLAGS = -f -lskym
FLAGS = -f -v
DESTDIR = /usr/local

gflow:	$(OBJECTS)
	f77 $(FLAGS) -o gflow $(OBJECTS) $(LIBS)
	size gflow

clean:
	rm *.o

install:
	@size gflow $(DESTDIR)/gflow
	mv gflow $(DESTDIR)

print:	$(FILES)
	cat $? | pr | lpr
	touch print
