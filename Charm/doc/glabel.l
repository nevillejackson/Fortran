.TH "GLABEL" l CHARM x "CSIRO CHARM USERS GUIDE"
.SH NAME
GLABEL - prints labels with 2 fixed lines of print & 1 variable line plus a 
10 x 10 reference grid.
.SH SYNOPSIS
glabel infile outfile1 outfile2
.sp
The program requires five directives - 
.nf
       1)   read format for data
       2)   fixed heading 1 format  
       3)   fixed heading 2 format
       4)   variable format for data
       5)   grid heading
.fi
.SH DESCRIPTION
3 arguments required:
.sp
infile      input file
.sp
outfile1    printed labels
.sp
outfile2    reference grid
.sp
Label paper must be set exactly, in order to get printing centred on each label.
Some trial & error may be required.
Set label paper on the '11' line of printer, then move paper back 6mm (1/4 inch)
.SH DIRECTIVES
.nf
read format for data     (drop,tag,sex,breed)
.sp
heading 1 format         ('0',5('     26 characters        '))
.sp
heading 2 format         (' ',5('     26 characters        '))
.sp
write format for data    ('0',5(drop,tag,sex,breed)/) 
                                 - 26 characters
.fi
.sp
grid heading                data identifier     8a10
.SH EXAMPLE 
.nf
glabel ab60 labs grid <<eoi
(10x,a3,a4,18x,a1,30x,a4)
('0',5('   ab60 1986 drop         '))
(' ',5('   tail fat samples       '))
('0',5(6x,a3,1x,a4,5x,a1,6x)/)
ab60 1986 drop lambs
eoi
