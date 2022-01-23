#########################################################################
#                      Makefile for DXMUX                               #
#                      fortran 90 version                               #
#########################################################################

#  ... might have to compile modules.f90 'by hand' before this works ...
#        f90 -c -fast -fpe2 -O5 modules.f90

#  ... if your f77 compiler is much better at optimisation than f90,
#        f77 -c -fast -fpe3 -O5 dxspb3.f

#-------------------- customize for local installation -------------------

# my home directory ...
MYDIR = /home/agbu/kmeyer

# DFREML directory
DFDIR = $(MYDIR)/DF2000

# directory in which executable file "dxmux" is to be placed
BINDIR = $(MYDIR)/bin

# directory in which library file "libkm90.a" is held
LIBDIR = $(BINDIR)

# directory for modules used by all programs
MODIR =$(DFDIR)/LIB

# FORTRAN compiler
FC=f95

# Compiler options
# ... regular setting - solaris compiler !
#FFLAGS= -fast -fnonstop  -L$(LIBDIR) -M$(MODIR)
# ... debug
#FFLAGS= -g -dalign  -L$(LIBDIR) -M$(MODIR)

# DEC - regular
FFLAGS= -fast -fpe1 -v -)5 -arch generic -tune generic -L$(LIBDIR) -I$(MODIR)
#FFLAGS= -g -C  -L$(LIBDIR) -I$(MODIR)
#--------------------- end of changes -----------------------------------

# Object files
OBJECTS=dxmux.o \
df51u3.o \
df59r3.o \
dxadj3.o \
dfcor3.o \
dfinp3.o \
dfipm3.o \
dxlik3.o \
dxmmd3.o \
dxmmp3.o \
dfmme3.o \
dfmmp3.o \
dxnrp3.o \
dxest3.o \
dxsrch.o \
dxord3.o \
dxpbl3.o \
dfspa3.o \
dfwrh3.o \
dfwrt3.o \
dfwrv3.o \
dxcmb3.o \
dxran3.o \
dxres3.o \
dxcovm.o \
isplc3.o\
dxspa3.o \
dxspb3.o \
dxslw3.o \
dxypy3.o \
dfpre3.o \
dflsq3.o \
dxsca3.o \
dxchn3.o \
setup.o \
chksig.o \
dxase3.o \
dxkfg3.o \
dxktg3.o \
dxphi.o \
dxkdv3.o \
dxlgnd.o\
dxcovf.o \
dfspx3.o \
dfpow3.o \
dxgrd3.o \
modules.o  

# Build program
all: modules dxmux

modules:
	$(FC) $(FFLAGS) -c -o modules.o modules.f90 $(MODIR)/modules_all.f90

dxmux: $(OBJECTS)
	$(FC) $(FFLAGS) $(OBJECTS) -o $(BINDIR)/dxmux  -lkm90

# Dependencies
dxmux.o  :  modules.o 
df51u3.o :  modules.o 
dxadj3.o :  modules.o 
dfcor3.o :  modules.o 
dfinp3.o :  modules.o 
df59r3.o :  modules.o 
dxlik3.o :  modules.o 
dfmme3.o :  modules.o 
dfmmp3.o :  modules.o 
dxmmp3.o :  modules.o 
dxmmd3.o :  modules.o 
dvnrp3.o :  modules.o 
dxord3.o :  modules.o 
dxpbl3.o :  modules.o 
dxran3.o :  modules.o 
dxres3.o :  modules.o 
dxcovm.o :  modules.o 
dfspa3.o :  modules.o 
dfwrh3.o :  modules.o 
dfwrt3.o :  modules.o 
dfwrv3.o :  modules.o 
isplc3.o :  modules.o 
dxspa3.o :  modules.o 
dxypy3.o :  modules.o 
dxest3.o :  modules.o 
dxnrp3.o :  modules.o 
dxsrch.o :  modules.o 
dxslw3.o :  modules.o 
dxase3.o :  modules.o 
dfipm3.o :  modules.o 
dxcmb3.o :  modules.o 
dfpre3.o :  modules.o 
dflsq3.o :  modules.o 
dxsca3.o :  modules.o 
dxchn3.o :  modules.o 
setup.o  :  modules.o 
chksig.o :  modules.o 
dxkfg3.o :  modules.o 
dxktg3.o :  modules.o 
dxphi.o  :  modules.o 
dxkdv3.o :  modules.o 
dxlgnd.o :  modules.o 
dxcovf.o :  modules.o 
dfspx3.o :  modules.o 
dfpow3.o :  modules.o 
dxgrd3.o :  modules.o 













