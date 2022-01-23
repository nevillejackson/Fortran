.TH "WEATHER" l CHARM x "CSIRO CHARM USERS GUIDE"
.SH NAME
WEATHER - calculates no. of miles of wind & amount of rain since previous
observation.
.SH SYNOPSIS
weather infile outfile
.sp
(read format)
.SH DESCRIPTION
The program requires one directive - a read format - reads year,day,time,wind
reading & rain gauge reading from verified file.
.nf
                     e.g. (10x,a9,4x,i5.5,i3.3)
.fi
.sp
The program writes  year,day,time,wind & rain as (a9,i5.5,i3.3) 
.sp
outfile must first be created for subsequent use with other CHARM programs.
