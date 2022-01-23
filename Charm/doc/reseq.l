.TH RESEQ l CHARM x "CSIRO CHARM USERS MANUAL"
.SH NAME
RESEQ - for re-ordering sequence numbers on a CHARM file
.SH SYNOPSIS
reseq infile outfile
.SH DESCRIPTION
two arguments required :
.TP 10
infile
input file
.TP 10
outfile
re-sequenced output file
.PP
RESEQ does not change the order of the records in the file, just the sequence
numbers of those records. All CHARM programs automatically re-sequence records
but the UNIX `sort` does not so this program is only needed if a job finishes
with a `sort` prior to saving (bagging) a new file or cycle of a file.
.SH DIAGNOSTICS
errors written to standard error
