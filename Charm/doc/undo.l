.TH UNDO l CHARM x "CSIRO CHARM USERS MANUAL"
.SH NAME
UNDO - removes record length and sequence number from the beginning of
each record before copying it across to outfile. (same as Cardim in old
CHARM).
.SH SYNOPSIS
.nf
undo infile outfile
.SH DESCRIPTION
two arguments required :
.TP 10
infile
input file
.TP 10
outfile
output file with first 10 columns removed from each record
.SH DIRECTIVES
no directives required
.SH DIAGNOSTICS
errors written to standard error
