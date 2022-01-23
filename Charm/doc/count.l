.TH COUNT l CHARM x "CSIRO CHARM USERS MANUAL"
.SH NAME
COUNT - counts pre-sorted records from the input file with control changes
on specified fields. Sum and average specified fields with the same control 
changes as for the counting.
.SH SYNOPSIS
.nf
count infile outfilec [outfilecs]

*c,name,beg,len,order
*s,name,beg,len,ndec
*l
*l,name,beg,len,ndec
.nf

*sf,rl,(i3,i6,1x,na10,2i10,3f10.b)
.SH DESCRIPTION
Two arguments required, with an optional third :
.TP 10
infile
input file
.TP 10
outfilec
count output file (mandatory)
.TP 10
outfilecs
count summary output file (optional)
.PP
Records must be sorted as they are to be counted. ie. major count field must
also be major sort field as counts, sums etc. are computed and written out
each time the contents of a specific control field change. Major, intermediate 
and minor summaries may be produced according to sorting order and corresponding
control changes. Counts for fields summed may be less than record count if
there are missing data for some fields.
.PP
If a count summary file IS required a *sf directive giving the necessary format
must be supplied along with the *c and *s directives, if NOT required the 
warning message `no count summary file format ... no file` will be issued.
.PP
Records counted may be listed wholly or partially using the *l directive, the
default is not to list them at all.
.SH DIRECTIVES
.TP 10
*c
control change directive
.TP 10
name
an alphabetic or hollerith name to be used as a heading for the control field.
Maximum length of 10 characters.
.TP 10
beg
the beginning column number of the control field.
.TP 10
len
the length in characters of the control field.
.TP 10
order
either "a" for ascending or "d" for descending.
.TP 10
*s
directive for sums and means
.TP 10
name
a 10 character name used to identify the trait summed
.TP 10
beg
the beginning column number of the data field
.TP 10
len
the length in characters of the data field
.TP 10
ndec
the number of digits after the implied decimal point in the data field.
Sums are printed with this same number of digits after the decimal. Means
are printed with one extra digit and standard deviations with three extra
digits.
.TP 10
*l
a listing directive
.TP 10
name
an alphabetic header for a field being listed
.TP 10
beg
the beginning column of a field within each record to be listed.
.TP 10
len
the length of the field to be listed
.TP 10
ndec
the number of places after the decimal, the point will be inserted
automatically unless ndec is absent or equal to zero.
.PP
If there are no *l directives the default is to list no records or fields 
thereof. A *l alone lists whole records whereas a *l with a field specified
lists that field for each record in the file (a maximum of 10 fields may 
be listed).
.SH SUMMARY FILE
If a summary file is indicated by inclusion of a third argument on the program
call then a format must be supplied for this file. The *sf directive supplies
this format beginning with the record length of the file :
.nf
                rl = 10(n+5)

where `n` equals the number of control change directives.

thus :   *sf,rl,(i3,i6,1x,na10,2i10,3f10.b)

where `b` is the number of digits required after the decimal.

.fi
Giving a record with record length and sequence number in the first 10 columns
(as for all CHARM files), followed by the contents of the `n` control fields,
followed by trait number, record count, count, mean then standard deviation for
that trait.
.PP
If there is more than one field (trait) being summed, there will be one record
for each, identified by a `trait number` corresponding to the sequential order
of the *s directives.
.PP
Counts remain as integers, totals, means and standard deviations will remain
in floating point for maximum accuracy on re-reading.
.PP
Summary files can be read and manipulated by CHARM programs if required.
.SH DIAGNOSTICS
Errors written to standard error
