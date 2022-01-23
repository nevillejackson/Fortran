.TH DUPCHK l CHARM x "CSIRO CHARM USERS MANUAL"
.SH NAME
DUPCHK - for extracting duplicate records from a file
.SH SYNOPSIS
.nf
dupchk infile outu outd

*f,beg,len,order                           limit = 50
.SH DESCRIPTION
DUPCHK requires 3 arguments :
.TP 10
infile
pre-sorted input file
.TP 10
outu
output file containing unique records plus first copy of multiplicates
.TP 10
outd
output file containing the remaining copies of multiplicates
.PP
The order of the `*f` directives must be the same as the sorted order of the
file. ie. the first `*f` defines the major key, the second defines the next
most major key, and so on to the last directive which is the most minor key.
.PP
Similarly, `order` must be as sorted (ascending or descending). 
.PP
If there is only one field it must not exceed 100 characters in length. If
there is more than one field then the TOTAL length of ALL the fields must not
exceed 100 characters.
.SH DIRECTIVES
.nf
*f,beg,len,order
beg ... beginning position of this key field
len ... length of this key field
order . "a" for ascending, "d" for descending
.SH DIAGNOSTICS
error messages written to standard error
