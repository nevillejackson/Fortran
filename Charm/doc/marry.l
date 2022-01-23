.TH MARRY l CHARM x "CSIRO CHARM USERS MANUAL"
.SH NAME
MARRY - combines the pre-matched records from two files into a single compound
record on a third file. There is no re-formatting of the records they are
merely concatenated.
.SH SYNOPSIS
.nf
marry infile1 infile2 outfile
.SH DESCRIPTION
Three arguments required :
.TP 10
infile1
primary input file
.TP 10
infile2
secondary input file
.TP 10
outfile
output file with record length equal to the sum of the primary and secondary
input file record lengths. The output file is automatically re-sequenced.
There must be equal numbers of records in the primary and secondary input files
if not the job will abort with an informative message.
.SH DIRECTIVES
There are no directives required.
.SH DIAGNOSTICS
Errors written to standard error
