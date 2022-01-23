.TH REFORM l CHARM x "CSIRO CHARM USERS MANUAL"
.SH NAME
REFORM - provides a means of global intra-record re-formatting of a file.
.SH SYNOPSIS
.nf
reform infile outfile

*c,bego,begi,len                   limit = 100 (*c +/or *e)
*e,beg,'       '
.SH DESCRIPTION
REFORM requires 2 arguments :
.TP 10
infile
Input file
.TP 10 
outfile
Re-formatted output file with a record length equal to, less than or greater
than that of the input file, depending upon the re-formatting directives in
the job deck.
.PP
The new record length is deduced from the fields defined in the directives
and the record is initially set to blanks. Thus a record can be extended
with blank fill simply by "emitting" a blank into the required position,
giving the user control over the record length if desired.  
.PP
Only specified fields will be copied to the new record, the default is no
copying (blank field)
.PP
Maximum field length that can be copied with one `*c` is 1000 characters 
(record length limit for all "CHARM" files)
.PP
The field length to be "emitted" is restricted to 10 characters/directive,
therefore multiple `*e` directives will be necessary should a longer field
be required.
.PP
There is a limit of 100 directives/run, either `*c` or `*e` or a mixture
of both.
.PP
Overlapping target fields are allowed and no warning is given when this
occurs as it is assumed to be intentional on the part of the user (the 
latest directive overides earlier ones when this occurs).
.SH DIRECTIVES
.nf
.TP 5
*c
copy specifications
bego ... beginning position on output to copy data to
begi ... beginning position on input to copy data from
len  ... length of data to be copied (input to output) 
.TP 5
*e
emit specifications
beg  ... beginning position on output to emit following
         character string into
\'   \' .. string of characters enclosed in single quotes
         to be `emitted` into output file 
.SH DIAGNOSTICS
error messages written to standard error
