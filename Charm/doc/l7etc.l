.TH "L7ETC" l CHARM x "CSIRO CHARM USERS GUIDE"
.SH NAME
L7ETC - works out L7,LM,LW on lambing groups.
.SH SYNOPSIS
l7etc infile outfile
.sp
The program requires one directive - a read format
.sp
-  reads damtag,day of birth, last col. of lamb's eartag, 2nd col.
T/B, 1st col. LR, LB & dam's record no.
.sp
Selection Flock lambing groups:-
     (10x,a7,25x,a3,6x,a1,1x,i1,3x,i1,20x,i1,7x,i1)
.sp
X-breeding Flock lambing groups:-
     (10x,a7,31x,a3,6x,a1,1x,i1,3x,i1,18x,i1,3x,i1)
.SH DESCRIPTION
Exclude lamb unknowns & work out by hand.
Assumes only records with lambs born= 1,6 are fed in & file must be sorted x
damtag x record no.
The program writes dam's eartag, LB,L7,LM,LW   -  (a7,4i1)
.nf
      One record per dam
.fi
.sp
outfile must first be created for subsequent use with other CHARM programs.
The user should then offset match with the infile on eartag & dam's record no.
