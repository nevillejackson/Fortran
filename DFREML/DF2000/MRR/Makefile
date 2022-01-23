#########################################################################
#                      Makefile for DXMRR                               #
#                      fortran 90 version                               #
#########################################################################

#--------------------- change as necessary ---------------------------------

# my home directory ...
MYDIR = /home/agbu/kmeyer

# DFREML directory
DFDIR = $(MYDIR)/DF2000

# directory where executable file "dxmrr" is to be placed
BINDIR = $(MYDIR)/bin

# directory where library file "libkm90.a" is held
LIBDIR = $(BINDIR)

# directory for modules used by all programs
MODIR =$(DFDIR)/LIB

# FORTRAN compiler
#FC=f95
# Lahey's LF95
# FC=lf95

# Compiler options
# LF95 under Linux
#FFLAGS --verbose --staticlink --mod $(MODIR)
# ... solaris
#FFLAGS= -fast -fnonstop -dalign -v -L$(LIBDIR) -M$(MODIR)
#FFLAGS= -g  -dalign  -L$(LIBDIR) -M$(MODIR)
# ... COMPAQ Alpha station
#FFLAGS= -v -arch host -tune host  -fast -fpe1 -L$(LIBDIR) -I$(MODIR)
FFLAGS= -v -arch generic -tune generic  -fast -fpe1 -O5 -L$(LIBDIR) -I$(MODIR)
#FFLAGS= -g -C  -v -L$(LIBDIR) -I$(MODIR)
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
dxrez3.o \
dxraz3.o \
dxcovm.o \
isplc3.o\
dxspa3.o \
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
dxkdv3.o \
dxdet3.o \
modules.o\
dxspb3.o

# Build program
all: modules  dxmrr

modules:
	$(FC) $(FFLAGS) -o modules.o -c modules.f90 $(MODIR)/modules_all.f90

dxmrr: $(OBJECTS)
	$(FC) $(FFLAGS) $(OBJECTS) -o $(BINDIR)/dxmrr  -lkm90

# Dependencies
modules.o : modules.f90 $(MODIR)/modules_all.f90
dxmrr.o  :  modules.o 
df51u3.o :  modules.o 
dxadj3.o :  modules.o  
dfcor3.o :  modules.o  
dfinp3.o :  modules.o 
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
dxraz3.o : modules.o 
dxrez3.o : modules.o 
dxcovm.o : modules.o 
dfspa3.o : modules.o  
dfwrh3.o :  modules.o 
dfwrt3.o :  modules.o  
dfwrv3.o :  modules.o  
isplc3.o :  modules.o
dxspa3.o : modules.o
dxypy3.o : modules.o
dxest3.o : modules.o
dxnrp3.o : modules.o
dxsrch.o : modules.o
dxslw3.o : modules.o 
dxase3.o : modules.o
dxipm3.o : modules.o 
dxcmb3.o : modules.o
dfpre3.o : modules.o 
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
dfspx3.o : modules.o 
dfpow3.o : modules.o 
dxgrd3.o : modules.o
dxkdv3.o : modules.o
dxdet3.o : modules.o














