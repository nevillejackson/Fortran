.TH CREATE l CHARM x "CSIRO CHARM USERS MANUAL"
.SH NAME
CREATE - converts a file to CHARM format
.SH SYNOPSIS
create infile outfile rl
.SH DESCRIPTION
Create requires 3 arguments : 
.TP 10
infile
Input file.
.TP 10
outfile
"Created" output file.
.TP 10
rl
Record length of "created" file (i3 format).
.PP
.nh
CREATE puts record length and sequence number at the beginning of each record in 
the file, this occupies the first 10 character positions but is not part of `rl`
and does not have to be taken into account when "creating" or manipulating a 
file using CHARM. Maximum record length allowed by CHARM programs is 990 
characters. Input is read and "created" one record at a time, if `rl` is less 
than the record length of the input file each record will be truncated 
accordingly, if `rl` is greater the excess will be made up with blank fill.  
.SH DIAGNOSTICS
Errors written to standard error.
