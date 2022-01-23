.TH "RANDIEQ" l CHARM x "CSIRO CHARM USERS GUIDE"
.SH NAME
RANDIEQ - allocates animals randomly to 'n' groups with restriction on no.
of animals per group.
.SH SYNOPSIS
randieq infile outfile
.sp
The program requires two directives - 
.nf
       1)   nsheep,len,ngps,maxno
       2)   gpcodes            
.fi
.SH DESCRIPTION
Program requires no. of animals to be known & only one batch of data can
be handled in a run.
The program writes eartag & group allocated to.                 
.sp
Outfile must first be created for subsequent use with other CHARM programs.
.SH DIRECTIVES
.nf
  1)   nsheep,len,ngps,maxno      (4i5)
         -no. of sheep to be allocated to a group
         -length of header to be read & written  (e.g.eartag =7)
         -no. of groups animals are to be allocated to
         -max.no. of animals to be allocated to any one group
           The onus is on the user to ensure that 'nsheep' does not
           exceed 'ngps*maxno'
.sp
  2)   gpcodes            (20i5)
         - codes for groups to be 1 digit if less than 10,
                                  2 digits if less than 100
