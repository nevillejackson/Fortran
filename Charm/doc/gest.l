.TH "GEST" l CHARM x "CSIRO CHARM USERS GUIDE"
.SH NAME
GEST - computes Gestation Length, the Service Day & to which service
conceived for sheep data.
.SH SYNOPSIS
gest infile outfile
.sp
The program requires one directive - a
read format  -  reads damtag,service days 1,2,3, lambing day, no.times served
& dam's record no.
.sp
Selection Flock lambing groups:-
     (10x,a7,15x,3i3,1x,i3,31x,i1,9x,i1)
.sp
X-breeding Flock lambing groups:-
     (10x,a7,21x,3i3,1x,i3,29x,i1,5x,i1)
.SH DESCRIPTION
Assumes only records with lambs born= 1,6 are fed in & lambing day & service
day 1 must be known.
The program writes eartag, gestation period (-100 days), the service day, 
to which service conceived & dam's record no. according to format:- 
.nf
    (a7,i2,i3,2i1)    
.fi
.sp
outfile must first be created for subsequent use with other CHARM programs.
The user should then match with the infile on eartag & dam's record no.
