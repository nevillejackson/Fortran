.TH CALC l CHARM x "CSIRO CHARM USERS MANUAL"
.SH NAME
CALC - performs arithmetic and/or character operations on specified field(s)
of every record in a "created" file. The program exists as a .o file which 
must be compiled using the sky processor and including a bxa subroutine.
.SH SYNOPSIS
.nf
f77 -v -f -lskym -o calc /dev1g/grp2/CHARMFTN5/calc.o bxa.f
calc infile outfile

*cm,constant,beg1,len1,ndec1,beg2,len2,ndec2
*cm,constant,beg1,len1,ndec1,e,len2,ndec2
*cm,constant,beg1,len1,ndec1,o,ndec
*ca,constant,beg1,len1,ndec1,beg2,len2,ndec2
*ca,constant,beg1,len1,ndec1,e,len2,ndec2
*ca,constant,beg1,len1,ndec1,o,ndec

with BXA subroutine :
*a,name,beg,len,ndec
*b,name,beg,len,ndec
*c,name,beg,len
*d,name,beg,len
.SH DESCRIPTION
Two arguments required by CALC :
.TP 10
infile
input file
.TP 10
outfile
output file with record length equal to OR greater than that of input file
depending on whether or not a result field is defined to extend the record
length ("e" for single field operations, and `beg+len-1` exceeding input
record length for BXA result fields `*b` and `*d`).
.PP
Single-field operations (add a constant / multiply by a constant) are specified
by directives alone. Multi-field and more complex operations require a user
written FTN5 subroutine (BXA) to define the operations required in addition to
the directives defining the source and result fields.
.PP
There is a limit of 10 `*cm` +/or `*ca` directives/run
.PP
Directives for a user writen BXA subroutine for CALC usually contain :
.nf
(1) at least one source field directive (either *a or *c)
(2) at least one result field directive (either *b or *d)
(3) no more than 100 of any one directive type (may have up
    to 100 of each)
.fi
The order of these directives is vital to the subroutine but they may be 
interspersed with *cm and *ca directives. 
.PP
The program will always execute a BXA subroutine before any *cm or *ca 
directives, despite their order in a job deck.
.SH DIRECTIVES
.nf
.TP 5
*cm 
constant multiply
constant ... a number to be used as a multiplying 
             factor for a specified field of each 
             record. It may be a floating point number 
             or integer, but if it is a floating point       
             then the point and the whole part of the 
             number must both be explicitly entered.
             eg. "0.5" NOT ".5"
beg1  ...... beginning position of the field to be              
             multiplied (source field)
len1  ...... length in characters of this field
ndec1 ...... the number of digits after the decimal
             in this field. It is assumed to be 
             encoded WITHOUT an explicit decimal
             point.
beg2  ...... beginning position at which the result 
             field is to be written
len2  ...... length of result field to be written
len2  ...... length to give result field
ndec2 ...... the number of digits after the decimal 
             in the result field as written
e     ...... indicates record extension, the result              
             field being appended at the end of the
             record
o     ...... result field to overwrite the source 
             field, thus both the fields must be of
             equal length
.TP 5
*ca
constant addition
             the `constant` is added to the specified
             field of each record

             parameters are as for *cm
.TP 5
*a
numeric source field
defines a numeric source field which will be decoded
(if possible) to a floating point variable and placed
in array element "afld(k)" with logical array element 
"ta(k)" set to TRUE (indicating decoding was carried 
out successfully). If decoding is not possible (right-
most character in the field non-numeric) "ta(k)" is
set to FALSE and the field is placed in character 
array element "achar(k)" with "afld(k)" undefined. The
particular element (k) is determined by the order of
`*a` directives in the job deck. Intermingling with 
other directives will not affect this dependence.  
Decoding etc. is carried out in the program prior to 
the BXA subroutine being called.
.nf
.TP 5
     where
*a  ...... identifies a numeric source field directive
name ..... is a character or hollerith string which 
           serves as an alphabetic name for the field 
           in question. It's length is restricted to 
           10 characters.
beg ...... is the beginning position of the field
len ...... is the length in characters of the field
ndec ..... is the number of digits after the decimal 
           in the field, which is assumed to be 
           encoded without an explicit decimal point.
.TP 5
*b
numeric result field
defines either a numeric result field in which a  
floating point variable is placed in array element 
"bfld(l)" and logical array element "tb(l)" is set to 
TRUE, Or, a non-numeric result field (left justified 
character string) placed in array element "bchar(l)"  
with "tb(l)" set to FALSE. In either case the part-
icular element (l) of bfld,bchar and tb, is determined
by the order of the `*b` directives on input.
.TP 5
     where
*b  ...... identifies a numeric result field directive
           and other parameters are as for `*a`.
.TP 5
*c
defines a hollerith or alphabetic source field, which
is passed into subroutine BXA, without decoding, in
array element "cfld(m)". The particular element (m)   
of the array is determined by the order of `*c`    
directives in the jobdeck. The length of the source
field is restricted to a maximum of 10 characters.
.TP 5
     where
*c  ...... identifies a hollerith source field    
           directive and other parameters are as for
           `*a`
.TP 5
*d
defines a hollerith or alphabetic result field, which
is passed out of subroutine BXA, without encoding, in
character array element "dfld(n)". The particular  
element (n) of the array is determined by the order   
of `*d` directives in the jobdeck. Field length is
restricted to a maximum of 10 characters.
.TP 5
     where
*d  ...... identifies a hollerith result field       
           directive and other parameters are as for
           `*a`
.nf


.SH EXAMPLE
A skeleton BXA subroutine deck :
.nf

      subroutine bxa (afld,bfld,cfld,dfld,ta,tb,achar,bchar)
      dimension afld(100),bfld(100),ta(100),tb(100),cfld(100)
     +,dfld(100),achar(100),bchar(100)
      character cfld*10,dfld*10,achar*10,bchar*10
      double precision afld,bfld
      logical ta,tb
c-----calculations required by user make the body of the deck
c-----It is advisable to check the validity of all `afld`              
c-----parameters before using them in calculations. ie.
c-----         if (ta(k)) then
c-----otherwise incorrect results may occur. 
c-----The user must set tb(l) to TRUE or FALSE on return
      return
      end

.SH DIAGNOSTICS
errors written to standard error
