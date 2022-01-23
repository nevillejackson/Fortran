.TH SELECT l CHARM x "CSIRO CHARM USERS MANUAL"
.SH NAME
SELECT - makes a new file containing only those records `selected`
on the basis of fields within the records
.SH SYNOPSIS
.nf
select infile outinc [outexc]

*i,beg,len,eq,val1,val2,.......,valn
*e,beg,len,eq,val1,val2,.......,valn
*i,beg,len,gt,val
*i,beg,len,lt,val
*i,beg,len,gt,val1,lt,val2
*i,f(beg1,len1,beg2,len2,...,beg5,len5),eq,val1,val2,.....valn
*el,beg,len,eq,val1,val2,......,valn
.SH DESCRIPTION
SELECT requires 2 arguments and has an optional third :
.TP 10
infile
Input file
.TP 10
outinc
Output file containing only "selected" records
.TP 10
outexc
Optional output file containing records NOT "selected"
.PP
Records are selected if they satisify ALL the `*i` directive(s) in the run
AND are not excluded by ANY `*e` directive(s), "selected" records being
written to `outinc`. Records that do not satisify ALL `*i` directive(s) OR
are excluded by ANY `*e` directive are written to `outexc` only if
it is specified in the call to the program.
.PP
There is a limit of 50 directives/run, they can be a mixture of any of the 
types available.
.PP
When using `f( , , , )` for combined fields a maximum of 5 fields may be
specified
.PP
Field length is restricted to 10 characters for both individual and combined
fields.
.PP
A maximum of 10 field values/directive allowed.
.PP
Maximum length of directive allowed is 80 characters.
.PP
Equivalences available ... "eq" "lt" "gt"
     "           "     ... " gt,val1,lt,val2 "
.PP
Records excluded by a particular `*e` directive can be listed to output
simply by using `*el` directive instead.
.SH DIRECTIVES
.nf
.TP 7
include
*i,beg,len,eq,val1,val2,...,valn

Include any record with a field (beginning position
`beg` of length `len`) equal to ANY of the given 
values (val1 to valn).
The value is read as type character and may be of
type character or integer or a mixture of both. Blank
fields etc. do NOT have single quotes around them eg.
    *i,1,3,eq,68E,77-,   ,80L
.TP 7
exclude
*e,beg,len,eq,val1,val2,....valn

As for `*i` but exclude instead of including the record
.TP 7
< or >
*i/*e,beg,len,gt,val

Include/Exclude the record if the specified field is
of a value greater than the given value `val`.
Alternatively `lt` can be used in place of `gt` in
which case the record will be selected for inclusion/
exclusion if it's field has a value less than that
of `val`.
.TP 7
> + <
*i/*e,beg,len,gt,val1,lt,val2

Include/Exclude the record if the specified field is
in the range specified by `val1` and `val2`, ie.
greater then `val1` but less than `val2`.
.TP 7
combined
*i/*e,f(beg1,len1,...,begx,lenx),eq,val1,...,valn

Include/Exclude the record if the combined fields have
a value equal to any of the values (val1...valn) 
specified, the length of each of these values must be   
identical and equal the total length of the combined  
fields being compared (len1+len2+ ... +lenx)
.TP 7
exclude and list
*el

An `l` combined with an exclude to give `*el` forces 
the listing to standard output of all records excluded
by that particular exclude directive.
.SH DIAGNOSTICS
errors written to standard error
