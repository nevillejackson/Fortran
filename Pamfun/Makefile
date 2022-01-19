FILES = Makefile pamfun.f diagmy.f quadf.f hermf.f
OBJECTS = pamfun.o diagmy.o quadf.o hermf.o
LIBS = -lskym
FFLAGS = -f -v
FLAGS = -f -v
DESTDIR = $HOME

pamfun:	$(OBJECTS)
	f77 $(FLAGS) -o pamfun $(OBJECTS) $(LIBS)
	size pamfun

clean:
	rm *.o

install:
	@size pamfun $(DESTDIR)/pamfun
	mv pamfun $(DESTDIR)

print:	$(FILES)
	cat $? | pr | lpr
	touch print

arch:
	ar uv $(DESTDIR)/src/pamfun.a $(FILES)
	ranlib xyz.a

docsprint:

