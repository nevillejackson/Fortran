.TH MATCH l CHARM x "CSIRO CHARM USERS MANUAL"
.SH NAME
MATCH - compares specified data field(s) on two input files (primary and
secondary inputs) and generates four output files (primary match, secondary
match, primary non-match and secondary non-match).
.SH SYNOPSIS
.nf
match pi si pm sm pnm snm

*m,begpi,len,begsi,order                         limit = 50
*seqchk                                          limit = 1
*offset                                          limit = 1
.SH DESCRIPTION
.nf

.fi
MATCH requires 6 arguments :
.TP 10
pi
primary input file ... sorted according to `order` as specified on the
*m directive(s) and according to the key priority as indicated by the
sequence of *m directive(s). Must not contain any duplicates on the
major match key if using *seqchk or *offset.
.TP 10
si
secondary input file ... sorted as for `pi`
.TP 10
pm
primary matched output file ... `pi` records common to both `pi` and `si`
when compared on the key field(s) as specified by the `*m` directive(s)
.TP 10
sm
secondary matched output file ... `si` records common to both `pi` and `si`
when compared on the key field(s) as specified by the `*m` derective(s)
.TP 10
pnm
primary non-matched file ... records on `pi` but not on `si`
.TP 10
snm
secondary non-matched file ... records on `si` but not on `pi`
.SH DIRECTIVES
.nf
.TP 10
*m,begpi,len,begsi,order
begpi ... beginning column no. of the key field on 
          a primary record
len   ... length in characters of the key field on  
          both primary and secondary records
begsi ... beginning column no. of the key field on 
          a secondary record
order ... either `a` or `d` for ascending or des-
          cending sorted order for this key
NOTE :    One or more `*m` directives may be used 
          for a single MATCH run. The order of              
          these directives determines the order  
          (major,intermediate,minor etc.) of the  
          keys for matching and must correspond to  
          the sorted order of the two input files.  
          This order need not be the same as the  
          order of the key fields across a record.    
          Fields of `*m` directives may vary in    
          length but must not total more than 100  
          characters for a run.
.fi
.TP 10
*seqchk
Inclusion of this directive causes sequence checking of both primary and
secondary input files, run aborts on error, listing record no. and unit
no. (`pi`=unit10, `si`=unit11). Omission allows matching without sequence
checking, thus unsorted files can be compared or files can be compared
on fields other than those on which they were sorted ... unless an unchecked
MATCH is specifically required it is advisable to make a habit of always
including *seqchk in a MATCH run.
.TP 10
*offset
Without this directive records are MATCHED one-for-one, hence duplicates on
one file not matched by duplicates on the other file come out as unmatched
records.
With *offset, all duplicates on the secondary file are considered MATCHED by
a single occurence of a matching record on the primary file, and the single
occurence is written out multiple times so that two files of MATCHED records
will contain equal numbers of records paired one-for-one, even though `pi`
contained fewer records than `si`. Thus, with *offset, all records on the
primary file are considered to be potential master records. *Offset enforces
*seqchk whether explicitly included or not. Duplicates on the primary input
are not allowed, if they occur the job will abort, giving the record no. of
the primary file duplicate.
.SH DIAGNOSTICS
errors written to standard error
