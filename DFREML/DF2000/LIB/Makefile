#########################################################################
#                      Makefile for libkm90.a                           #
#########################################################################

#-------------------- Customize for local installation -----------------

# my home directory
MYDIR = /home/agbu/kmeyer

#DFREML directory
DFDIR = $(MYDIR)/DFREML

# directory in which library file "libkm90.a" is to be placed
LIBDIR = $(MYDIR)/bin

# FORTRAN compiler
FC=f90

# Compiler options - solaris f90
#FFLAGS= -fast -fnonstop -v  -c

# Compiler options - DEC f90
FFLAGS= -fast -O5  -fpe1  -c -v -arch generic -tune generic
#----------------------end of changes ----------------------------------

# Object files
OBJECTS= canon.o \
chkdig.o \
chklev.o \
correl.o \
dfanim.o \
dfconv.o \
dferrs.o \
dfkzer.o \
dfparm.o \
dfpopt.o \
dfstep.o \
dfwarn.o \
dfwdis.o \
dkmwhf.o \
eigen.o \
fconct.o \
genmmd.o \
gsfct.o \
gsslv.o \
ihmssf.o \
invgj.o \
ipsmin.o \
kount.o \
mulaba.o \
option.o \
opzero.o \
pedsum.o \
rvalue.o \
smbfct.o \
times.o \
triord.o \
vratio.f

# do everything
all:  modall routines libkm 

# compile modules
modall:
	$(FC) $(FFLAGS) modules_all.f90

# compile other routines
routines: $(OBJECTS)
	$(FC) $(FFLAGS) $(OBJECTS) 

# build library
libkm:
	ar rv $(LIBDIR)/libkm90.a *.o

# Dependencies
