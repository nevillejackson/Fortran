#########################################################################
#               Unix : Makefile for dfuni                               #
#########################################################################

#---------------change as required for local installation----------------

# my directory 
MYDIR = /home/agbu/kmeyer

#DFREML directory
DFDIR = $(MYDIR)/DF2000

# Directory where executable file (dfuni) is put
BINDIR = $(MYDIR)/bin

# Directory where libary (libkm90.a) is held
LIBDIR = $(BINDIR)

# Directory where modules used by all programs are
MODIR=$(DFDIR)/LIB

#Fortran compiler
FC=f95

# Compiler options (for DEC compiler)
FFLAGS= -v -fast -fpe1 -O5 -arch generic -tune generic -L$(LIBDIR) -I$(MODIR)
#FFLAGS=  -g -C  -fpe1  -L$(LIBDIR) -I$(MODIR)
# ... solaris compiler
#FFLAGS=  -fast -fnonstop  -L$(LIBDIR) -M$(MODIR)
#FFLAGS= -I$(MODIR) -g -L$(LIBDIR)  
#-----------------------end of changes----------------------------------

# Object files
OBJECTS= dfuni.o \
df51u1.o \
df59r1.o \
dfadj1.o \
dfase1.o \
dfinp1.o \
dflik1.o \
dflsq1.o \
dfmmd1.o \
dfmme1.o \
dfmmp1.o \
dford1.o \
dfpow1.o \
dfpre1.o \
dfqua1.o \
dfslw1.o \
dfspa1.o \
dfspx1.o \
dfwrt1.o \
modules.o 

# Build program
all: modules dfuni

modules:
	$(FC) $(FFLAGS) -c -o modules.o modules.f90 $(MODIR)/modules_all.f90

dfuni: $(OBJECTS)
	$(FC) $(FFLAGS) $(OBJECTS) -o $(BINDIR)/dfuni -lkm90

# Dependencies
dfuni.o  : modules.o
df51u1.o : modules.o
df59r1.o : modules.o 
dfadj1.o : modules.o 
dfase1.o : modules.o 
dfinp1.o : modules.o
dflik1.o : modules.o 
dflsq1.o : modules.o
dfmmd1.o : modules.o
dfmme1.o : modules.o
dfmmp1.o : modules.o
dford1.o : modules.o
dfpow1.o : modules.o
dfpre1.o : modules.o
dfqua1.o : modules.o 
dfslw1.o : modules.o
dfspa1.o : modules.o 
dfspx1.o : modules.o
dfwrt1.o : modules.o
modules.o :  $(MODIR)/modules_all.f90











