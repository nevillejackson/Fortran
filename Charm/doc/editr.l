.TH EDITR l CHARM x "CSIRO CHARM USERS GUIDE"
.SH NAME
EDITR - edits single records specified by sequence number on a file to produce
a new resequenced file.
.SH SYNOPSIS
.nf
editr infile outfile

*d,ln
*i,ln
*s,ln,params :     *s,ln,r(cn,string)   *s,ln,d(cn,long)    
                   *s,ln,i(cn,string)   *s,ln,b(cn,len)
*l,ln
.SH DESCRIPTION
Both full record deletion and insertion, and "intra-record" editing of field(s)
specified by column numbers are allowed. A given record may be edited only once
in a given run. The file to be edited must be in sequence order and the lines 
must be edited in sequence.
A maximum of 100 directives allowed per run.
.SH DIRECTIVES
.TP 10
*d,ln
delete line number (ie. sequence number) `ln` followed by optional insert.
Records to be optionally inserted must be the same length as those in the
existing file and records exceeding 80 characters in length are entered
80 char/line beginning in column one. Inserted records are given sequence 
numbers following on from the record after which they were inserted or 
beginning with the number of the record they have replaced.
.TP 10
*i,ln
insert after line number `ln`. Records to be inserted follow, on multiple lines
if record length greater than 80 characters, as for delete.
.TP 10
*s,ln,params
intra-line editing on `ln` according to standard parameters as follows :
.nf

r(cn,string)   replace character beginning in  
               column number `cn` with `string` of 
               same length

d(cn,long)     delete characters beginning in 
               column number `cn` of length `long`.   
               Left shift remainder of record and  
               pack end with blanks.
i(cn,string)   insert characters from `string` 
               beginning in column number `cn`.
               Right shift remainder of record and
               lose trailing characters.

b(cn,len)      replaces `len` characters with blanks
               beginning col `cn`.
note : (1) up to 10 `params` posible per directive. eg.

*s,ln,r(cn,string),r(cn,string),b(cn,len),i(cn,string)

       (2) substitution parameters must deal with   
columns in order (col.1 to end of record) across a 
record being intra-line edited.
.fi
.TP 10
*l,ln
list record number `ln` to output file as a character string (8a10 format)
with sequence number.
.SH DIAGNOSTICS
Errors written to standard error
