.TH "YELDI" l CHARM x "CSIRO CHARM USERS GUIDE"
.SH NAME
YELDI - computes yield & fibre diameter from sample weights & millivolt       
readings in duplicate.
.SH SYNOPSIS
yeldi infile outfile1 outfile2
.sp
.nf
1) ihead                                           (8a10)
2) nbag                                            (i2)
3) itare,istnwt                                    (2i1)
4) iform                                           (variable)
5) itray,vno,vwt                      optional     (a1,i3,f5.3)   
6) ibagno,resw,regw,resx,regw,imachw,imachx,itop   (a2,(2(f4.2,f5.2),3i2)
.fi
.SH DESCRIPTION
The fifth directive is only required when itare =1.
.sp
Bag no. must be on the records & the input file should be pre-sorted    
into bag no. x eartag order      
.sp
Records are written to outfile1 as itag,iy,jy,idiam
.nf                       
                     with format  (a7,i3.3,i8.8,i3.3)
        where:- itag .. eartag
                iy   .. 3 digit rounded yield for storage
                jy   .. 8 digit yield for subsequent calculation of clean
                         wool weight
                idiam .. 3 digit rounded diameter for storage
This file is for subsequent calculation of cww. It must first be created for
subsequent use with other CHARM programs.
.sp
Outfile2 lists the yield & diameter results for trays A & B test
specimens side by side, calculates the difference & flags:-
            yields less than 40%
            yields greater than 88%
            yields differing by more than 10% between A & B
            diameters less than 12 microns
            diameters greater than 50 microns
            diameters differing by more than 4 microns between A & B
This file is to be printed (fpr <outfile2 |lpr) for filing with the
data sheets.
.fi
.sp
If one of the two results has a value beyond the upper or lower limit, the
program takes the remaining single value instead of the average of the two.
.sp
If both recordings are beyond the limits or if the difference between the two
is greater than allowable, the values are averaged but still flagged. In some
cases editing may be necessary.

.SH DIRECTIVES
1) ihead  ... a general heading,max. 80 cols.
.sp
.nf
2) nbag   ... no. of bags or batches of data e.g. 3 bags  cols.1-2= 3
                                                 11 bags  cols.1-2=11
.sp
3) itare,istnwt  ... tared vessel option   col.1=1 tared
                                                =0 not tared  (usual)
                 ... station weight option col.2=1 stn wt. taken  (usual)
                                                =0 no stn greasy wt.
.sp
4) iform  ... variable format for reading input file
              4 alternatives depending on itare & istnwt
.sp
               a) itare=0, istnwt=1  (usual)
                   reads itag,w3,w4,w1,w2,amv,x1,x2,bmv,ibagno
                    (10x,a7,2f3.1,2(f3.2,f4.3,f3.0,a2))
.sp
               b) itare=0, istnwt=0
                   reads itag,w5,w2,amv,x5,x2,bmv,ibagno
                    (10x,a7,2(f3.2,f4.3,f4.0,a2))
.sp
               c) itare=1, istnwt=1
                   reads ivno,itag,w3,w4,a1,a2,amv,b1,b2,bmv,ibagno
                    (10x,i3,a7,2f3.2,2(2f5.3,f4.0,a2))
.sp
               d) itare=1, istnwt=0
                   reads itag,a5,a2,amv,b5,b2,bmv,vno,ibagno
                    (10x,a7,2(2f5.3,f4.0),i3,a2)
.sp
              where the parameters are:-
               ivno  ..  vessel no.
               itag  ..  eartag
               w3    ..  whole sample station greasy wt.
               w4    ..  whole sample laboratory greasy wt.
               a1    ..  test specimen greasy wt. & vessel tray A
               a2    ..  test specimen clean wt. & vessel   "   "
               b1    ..  test specimen greasy wt. & vessel  "   B
               b2    ..  test specimen clean wt. & vessel   "   "
               amv   ..  millivolt reading tray A
               bmv   ..  millivolt reading tray B
               a5    ..  test specimen greasy wt. & vessel tray A - when
                           no stn. greasy wt. is recorded
               b5    ..  test specimen greasy wt. & vessel tray B - when
                           no stn. greasy wt. is recorded
               w1    ..  test specimen greasy wt. tray A
               w2    ..  test specimen clean wt. tray A
               x1    ..  test specimen greasy wt. tray B
               x2    ..  test specimen clean wt. tray B
.sp
5) itray, vno, vwt  ...   only when itare=1
                          col.1     = tray A or B  (a1)
                          cols. 2-5 = vessel no.   (i3)
                          cols.6-10 = vessel tare wt. (f5.3)
.sp
6) ibagno,resw,regw,resx,regx,imachw,imachx,itop
              (a2,2(f4.2,f5.2),3i2)
        There must be one type 6) record for each bag
            ibagno   ..  bag identifier
            resw     ..  residual tray A
            regw     ..  regain tray A
            resx     ..  residual tray B
            regx     ..  regain tray B
            imachw   ..  machine tray A  1=F14404, -1=F15492
            imachx   ..  machine tray B  1=F14404, -1=F15492
            itop     ..  top or raw wool standard  1=tops, =1=raw wool
