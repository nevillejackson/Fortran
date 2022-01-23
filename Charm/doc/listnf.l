.TH LISTNF l CHARM x "CSIRO CHARM USERS MANUAL"
.SH NAME
LISTNF - provides a straight listing of a file with record length and 
sequence number displayed at the beginning of each record. 
Program LISTNS is provided for listing without record length or sequence 
number, program SHEET is for complex listing with headings,columns etc.
.SH SYNOPSIS
listnf infile
.SH DESCRIPTION
One argument only required for LISTNF, the file to be listed.
No format directive is required, output is one record/line, or for long
records 120 characters/line with each record beginning on a new line,
with record length and sequence number in the first 10 character positions.
Listing is to standard output.
.SH DIAGNOSTICS
errors written to standard error
