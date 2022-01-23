.TH RECODE l CHARM x "CSIRO CHARM USERS MANUAL"
.SH NAME
RECODE - provides a "selective re-formatting" capability by combining the
features of SELECT and REFORM in one program. Used for altering specific
records or blocks of records in an existing file, also for setting up data
for input to SHEET.
.SH SYNOPSIS
.nf
recode infile outfile dum1 dum2

*all,bego,begi,len
*all,bego,` string `
*code,bego,begi,len
*code,bego,` string `
*i,beg,len,eq,val1,val2,......valn
*e,beg,len,eq,val1,val2,......valn
*i,beg,len,gt,val
*i,beg,len,lt,val
*i,beg,len,gt,val1,lt,val2
*i,f(beg1,len1,...,begx,lenx),eq,val1,...,valn
.SH DESCRIPTION
Recode requires 4 arguments :
.TP 10
infile
input file
.TP 10
outfile
"Recoded" output file, containing only those records specifically "selected"
and re-formatted by RECODE.
.TP 10
dum1/dum2
Records not specifically "selected" in any of the re-formatting blocks of
the RECODE will be written to "scratch file" dum1 or dum2 depending on 
whether there is an odd (dum1) or even (dum2) number of directive blocks
executed in the RECODE. The program automatically deletes these files on
successful completion of a run.
.PP
RECODE begins with the `*all` global re-formatting directives that precede
ANY `*code` directives and apply to EVERY record re-formatted by the program.
These `*all` directives may only be at the beginning before ANY `*code`
directives, if any occur later in the job deck the run will abort. There is 
a limit of 50 `*all` directives/run.
.PP
Records are "selected" from the input file according to SELECT directives
and re-formatted according to directives similar to those of REFORM. The 
new records are written to `outfile`. The directives are partitioned into
blocks (there is a limit of 50 blocks/run), each beginning with one or 
more `*code` directives specifying the re-formatting required for that block,
followed by one or more SELECT directives indicating the records to be re-
formatted in that block. A block is terminated by the first `*code` of the next
block, or an end of directives marker (eoi). Limit = 50 `*code` directives/
block.
.PP
Operationally RECODE processes one directive block at a time. All the records
on `infile` initially (`dum1` or `dum2` later in the job) that satisify the
block "selection" directives are re-formatted according to the `*all` and
the block's `*code` directives, then written to `outfile`. Rejected records are 
written to `dum1` (initially) to be input for the next block (rejected records 
will then be written to `dum2` ie. `dum1` and `dum2` alternate with each
block processed).
.PP
Record length of `outfile` is determined by the `*all` and the first block
of `*code` directives. If in later blocks a longer record length is indicated
the re-formatting requiring greater record length will fail and abort the job
.PP
Counts of records re-formatted by each block are printed to standard output
as each block is processed.
.PP
When all records from `infile` have been re-formatted RECODE terminates whether 
or not there are more directive blocks in the job deck. Should the directives
end without all records having been re-formatted these "unselected" records
(on dum1 or dum2) will be lost. A total count of records written on `outfile`
is printed to standard output on program completion.
.SH DIRECTIVES
.nf
.TP 10
*all,bego,begi,len
global copying

.fi
ALL records re-formatted to have the data field specified (`begi`,`len`) 
copied to the new record, beginning position `bego`.
.TP 10
.nf
*all,bego,` string `
global emit

.fi
ALL records re-formatted to have the quoted string "emitted" in the new record,
beginning at position `bego`.
.TP 10
.nf
*code,bego,begi,len
block copying

.fi
As for the copy `*c` in REFORM, copies field (`begi`,`len`) to new record 
beginning position `bego`.
.TP 10
.nf
*code,bego,` string `
block emit

.fi
As for the emit `*c` in REFORM, copies the quoted string to the new
record beginning position `bego`.
.TP 10
.nf
*i/*e
block selection

.fi
Include and exclude directives as for SELECT with a limit of 50 directives 
/block.
.SH DIAGNOSTICS
errors written to standard error
