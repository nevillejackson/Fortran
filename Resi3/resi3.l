.TH RESI3 l ANPROD
.SH NAME
resi3 - combined selection index with restrictions
.SH SYNOPSIS
.RS 5
\fBresi3 libraryfile <fin >fout\fR
.RE
.SH DESCRIPTION
.I Resi3
is a CSIRO program to compute selection indices which 
combine information from
.TP 10
.BI (i)
individual phenotypic values,
.TP 10
.BI (ii)
half sib family means,
.TP 10
.BI (iii)
full sib family means, and
.TP 10
.BI (iv)
progeny test means.
.PP
There can be multiple traits, and the set of traits for
each of the above four types of values or means need not
be the same, and can be an empty set.
Genetic gains for any individual trait can be restricted
to zero.
Economic weights for individual traits can be specified
as relative economic weights or as desired gains.
.PP
.I Resi3
computes three types of index 
.TP 10
.BI (i)
all traits unrestricted combined index
.TP 10
.BI (ii)
selected traits unrestricted combined index
.TP 10
.BI (iii)
selected traits restricted combined index
.PP
.I Resi3
requires two input files
.TP 10
.BI (i)
a
.I libraryfile
containing genetic & phenotypic parameters describing 
variation in the population for the traits in the index
.TP 10
.BI (ii)
the standard input file (STDIN) containing parameters
describing the type(s) of index to be constructed
.PP
.I Resi3
writes its results to standard output (STDOUT). Results
reported include
.TP 10
.BI (i)
index B coefficients
.TP 10
.BI (ii)
genetic covariance between each trait & the index
.TP 10
.BI (iii)
correlated change in each trait on selection for the index
.TP 10
.BI (iv)
genetic correlation between each trait & the index
.TP 10
.BI (v)
genetic gain in aggregate genotype on selection for index
.TP 10
.BI (vi)
covariace & correlation of aggregate genotype with index
.TP 10
.BI (vii)
variance of index & of aggregate genotype
.SH LIBRARY FILE
.I Resi3
requires a separate file of population parameters. This facilitates
building a library of useful parameter estimates which can be
re_used for future jobs. There can be many more traits in the 
library file than are invoked for a particular index.
.PP
Each record in the library file contains the following
parameters in free format (ie blanks between fields)
.TP 10
.BI trait A no
trait number for trait A
.TP 10
.BI trait B no
trait number for trait B
.TP 10
.BI Rp or SDp
phenotypic correlation between traits A & B (or phenotypic
standard deviation if traits A & B are the same trait)
.TP 10
.BI Rg or hsq
genetic correlation between traits A & B (or heritability
if traits A & B are the same trait)
.TP 10
.BI Rch or chsq
common environmental correlation between traits A & B for half sibs
(or common environmental portion of variance if traits A & B
are the same trait)
.TP 10
.BI Rcf or cfsq
common environmental correlation between traits A & B for full sibs
(or common environmental portion of variance if traits A & B 
are the same trait)
.PP
Trait numbers must be unique, within a library file. They need bear no
relation to trait numbers in the index. If common environmental
parameters are not available enter zeroes.
.SH INPUT FILE (STDIN)
Parameters on standard input describe the type(s) of index to be constructed.
There are five types of parameter record as follows. All fields are in
free format
.SH Record type 1:
One record always required sets job parameters as follows
.TP 10
.BI njobs
number of jobs in this shellscript - determines how many sets of records
type 2-5 folow
.TP 10
.BI liop
list option - -1=skimpy 0=brief 1=verbose(debugging only)
.TP 10
.BI miop
population parameters input option - 1=library file free format ascii
 - no other option available yet
.TP 10
.BI ltr
number of traits in library file
.SH Record type 2:
Job heading up to 100 characters enclosed in double quotes.
One record per job required.
.SH Record type 3:
Index size & no of different indices.
One record per job required structured as follows
.TP 10
.BI ntr
number of traits in aggregate genotype & in index as individual
values. Although some traits may have zero economic weight
or may be excluded from the index, they should still be counted
here, as this parameter sets the number of records of type 5 to 
follow.
Traits included here & on record type 5, but not in the index,
will have their genetic change & correlation with
the index reported.
.TP 10
.BI new
number of sets of economic weights to be included on records of
type 5 to follow
.TP 10
.BI ndg
number of sets of desired gains to be included on records of type 5 to follow
.TP 10
.BI nsr
number of sets of delta_G restrictions to be included on
records of type 5 to follow
.SH Record type 4:
Family sizes as follows - if no family data enter zero(s)
.TP 10
.BI nh
number per half sib family.
.TP 10
.BI nf
number per full sib family.
.TP 10
.BI no
number per progeny test group
.SH Record type 5:
Trait definitions. 'ntr' records required. Each contains the
following fields
.TP 18
.BI libno(j)
number of trait j in library file
.TP 18
.BI name(j)
name of trait j in double quotes. Max 10 characters plus quotes.
.TP 18
.BI ini(j)
Is trait j  in index as individual value? - 1=yes 0=no
.TP 18
.BI inh(j)
Is trait j in index as half sib mean? - 1=yes 0=no
.TP 18
.BI inf(j)
Is trait j in index as full sib mean? - 1=yes 0=no
.TP 18
.BI ino(j)
Is trait j in index as progeny test mean? - 1=yes 0=no
.TP 18
.BI a(i,j),i=1,new
economic weight for trait j - 
.B new 
fields, one for each set of economic weights. If new=0 omit.
.TP 18
.BI dg(i,j),i=1,ndg
desired gain for trait j - 
.B ndg 
fields, one for each set of desired gains. If ndg=0 omit.
.TP 18
.BI gres(i,j),i=1,nsr
restriction of genetic gain to zero for trait j. 1=trait j allowed to
change, 0=trait j constrained to zero genetic change - 
.B nsr 
fields, one for each delta_G restriction. If nsr=0 omit.
.SH EXAMPLES
.RS 5
\fBresi3 ex1.lib <ex1 >&ex1.out &\fR
.sp
.RE
.SH SEE ALSO
resi2(1),seld(l)
.SH AUTHORS
N.Jackson,C.A.Dean & P.P.Cotterill.
Copyright 1987 by C.S.I.R.O.
