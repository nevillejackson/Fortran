.TH "GESTCOW" l CHARM x "CSIRO CHARM USERS GUIDE"
.SH NAME
GEST - computes Gestation Length, the Service Day & to which service
conceived for cattle data.
.SH SYNOPSIS
gestcow infile outfile
.sp
The program requires one directive - a
read format  -  reads damtag,service days 1,2,3, calving day, no.times served
& dam's record no.
.sp
Cattle mating & calving groups:-
     (10x,a6,16x,3i3,1x,i3,29x,i1,11x,i1)
.SH DESCRIPTION
Assumes only records with calves born= 1,3 are fed in & calving day & service
day 1 must be known.
The program writes eartag, gestation period, the service day, to which service
conceived & dam's record no. according to format:- 
.nf
     (a6,2i3,2i1)      
.fi
.sp
outfile must first be created for subsequent use with other CHARM programs.
The user should then match with the infile on eartag & dam's record no.
