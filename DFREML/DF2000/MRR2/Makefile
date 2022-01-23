#########################################################################
#                      Makefile for DXMRR                               #
#                      fortran 90 version                               #
#########################################################################


#  ... might have to compile modules.f90 'by hand' before this works ...
#        f90 -c -fast -fpe2 -O5 modules.f90

#--------------------- change as necessary ---------------------------------

# my home directory ...
MYDIR = /home/agbu/kmeyer

# DFREML directory
DFDIR = $(MYDIR)/DFREML

# directory where executable file "dxmrr" is to be placed
BINDIR = $(MYDIR)/bin

# directory where library file "libkm90.a" is held
LIBDIR = $(BINDIR)

# directory for modules used by all programs
MODIR =$(DFDIR)/LIB

# FORTRAN compiler
FC=f90

# Compiler options
FFLAGS= -fast -O5 -fpe2  -L$(LIBDIR) -I$(MODIR)
#--------------------- end of changes --------------------------------------

# Object files
OBJECTS=dxmrr.o \
df51u3.o \
df59r3.o \
dxadj3.o \
dfcor3.o \
dfinp3.o \
dxipm3.o \
dxlik3.o \
dxmmd3.o \
dxmmp3.o \
dfmme3.o \
dfmmp3.o \
dxnrp3.o \
dxest3.o \
dxsrch.o \
dvord3.o \
dxpbl3.o \
dfspa3.o \
dfwrh3.o \
dfwrt3.o \
dfwrv3.o \
dxcmb3.o \
dxran3.o \
dxres3.o \
dxcovm.o \
iparn3.o\
isplc3.o\
dxspa3.o \
dxspb3.o \
dxslw3.o \
dxypy3.o \
dfpre3.o \
dflsq3.o \
dxsca3.o \
dxchl3.o \
dxchn3.o \
setup.o \
chksig.o \
dxase3.o \
dxphi.o \
dxlgnd.o\
dxcovf.o \
dfspx3.o \
dfpow3.o \
dxgrd3.o \
genmmd.o \
dxkdv3.o \
dxdet3.o \
modules.o\
$(MODIR)/modules_all.o

# Build program
dxmrr: $(OBJECTS)
	$(FC) $(FFLAGS) $(OBJECTS) -o $(BINDIR)/dxmrr2  -lkm90

# Dependencies
dxmrr.o  :  modules.o $(MODIR)/modules_all.o
df51u3.o :  modules.o 
dxadj3.o :  modules.o  
dfcor3.o :  modules.o  
dfinp3.o :  modules.o  $(MODIR)/modules_all.o
df59r3.o :  modules.o  
dxlik3.o :  modules.o  
dxmmd3.o :  modules.o 
dfmme3.o :  modules.o 
dfmmp3.o :  modules.o 
dxmmp3.o :  modules.o 
dxmmd3.o :  modules.o 
dvnrp3.o :  modules.o 
dvord3.o :  modules.o 
dxpbl3.o :  modules.o  
dxran3.o :  modules.o
dxres3.o : modules.o 
dxcovm.o : modules.o 
dfspa3.o : modules.o  
dfwrh3.o :  modules.o 
dfwrt3.o :  modules.o  
dfwrv3.o :  modules.o  
isplc3.o :  modules.o
iparn3.o :  modules.o
dxspa3.o : modules.o
dxypy3.o : modules.o
dxest3.o : modules.o
dxnrp3.o : modules.o
dxsrch.o : modules.o
dxslw3.o : modules.o $(MODIR)/modules_all.o
dxase3.o : modules.o
dxipm3.o : modules.o $(MODIR)/modules_all.o
dxcmb3.o : modules.o
dfpre3.o : modules.o $(MODIR)/modules_all.o
dflsq3.o : modules.o
dxsca3.o : modules.o
dxchl3.o : modules.o
dxchn3.o : modules.o
setup.o  : modules.o
chksig.o : modules.o
dxphi.o  : modules.o
dxkdv3.o : modules.o
dxlgnd.o : modules.o
dxcovf.o : modules.o
dfspx3.o : modules.o $(MODIR)/modules_all.o
dfpow3.o : modules.o $(MODIR)/modules_all.o
dxgrd3.o : modules.o
genmmd.o : modules.o
dxkdv3.o : modules.o
dxdet3.o : modules.o











