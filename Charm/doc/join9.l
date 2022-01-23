.TH JOIN9 l CHARM x "CSIRO CHARM USERS MANUAL"
.SH NAME
JOIN9 - combines from two to nine files into a single file 
.SH SYNOPSIS
.nf
join9 inf1 inf2 ( ..... infn) outfile
.SH DESCRIPTION
JOIN9 requires from 3 to 10 arguments depending on how many files are
being joined.
.TP 10
inf1
input file 1
.TP 10
inf2
input file 2
.TP 10
infn
input file n .. maximum allowed is 9
.TP 10
outfile
output file containing all the records from files 1 to n in the order
joined.
.PP
Input files may differ in length, in which case the output file will be
equal in length to the longest input file, shorter files will be blank
padded to this length. Empty files on input generate an informative message
without aborting the job. The output file is automatically resequenced but
record order is unchanged.
.SH DIRECTIVES
There are no directives required
.SH DIAGNOSTICS
Errors written to standard error
