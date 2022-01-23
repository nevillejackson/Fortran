.TH "SDAYS" l CHARM x "CSIRO CHARM USERS GUIDE"
.SH NAME
SDAYS - converts service day records as entered & verified from date to day of
year.
.SH SYNOPSIS
sdays infile outfile
.sp
The program requires two directives - 
.nf
       1)   year as I2  e.g. 87 in cols.1-2
       2)   a read format to read dam's eartag, no.times served & service 
             dates 1,2 & 3 as day,month,year.
              e.g. (10x,a7,i1,6i2)     
.fi
.SH DESCRIPTION
The program writes dam's eartag, no.times served, sd1, sd2 & sd3 - 
                (a7,i1,3i3)
.sp
      Ewes not served should have been entered & verified with no.times
served =0 & the service dates blank.
.sp
outfile must first be created for subsequent use with other CHARM programs.
