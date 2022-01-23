.TH "LAMTAB" l CHARM x "CSIRO CHARM USERS GUIDE"
.SH NAME
LAMTAB - produces lambing tables from selection & x-breeding flock lambing     
groups.
.SH SYNOPSIS
.nf
f77 -v -f -lskym -o lamtab /dev1g/grp2/CHARMFTN5/lamtab.o lamtab.f
lamtab infile outfile1 outfile2 outfile3
.sp
 where:-  outfile1 is the summary file giving nos. of ewes present 
                   lambing & % dry
          outfile2 is the summary file giving epl, %el/epl, %lb/el,
                   %lb/epl, %lm/lb, %lm/epl summed over the most 
                   frequently cycling variable.
          outfile3 is the file of main tables.
.sp
Parameter cards:-
 1)              iprop,isr
 2)              nways,islaut
 3),4),5)        one for each control variable, therefore depends on nways
 6)              ihead
 7) 8) 9) 10)    khead 
11) 12) 13) 14)  lhead
.fi
.SH DESCRIPTION
Many runs are reqired to get all lambing tables as experiments have different
requirements. 2 copies of each table should be printed - one copy to remain 
here,the other to go to the appropriate R.S.
.sp
Program LAMTAB requires a subroutine "lamread.f" which needs to be edited for
each run. The file CHARMFTN5/lamtab.read should be copied to the user's 
/JOBS directory as "lamread.f"
JOBDEKS/jblamtab should also be copied to the user's /JOBS directory.
.sp
  Data must be sorted in the manner in which the table is required
      e.g. exp x line x mating group
.SH DIRECTIVES
Options for read format to be inserted in "lamread.f"
.sp
.nf
c--exp x line x drop (sel)
    1 read(1,500,end=99)kg(1),kg(3),kg(2),lunk,tb1,tb2,lr1,lr2,lb,no
  500 format(10x,a2,6x,a2,1x,a2,28x,a1,2i1,3x,2i1,19x,i1,7x,i1)
c--exp x drop (sel)
    1 read(1,500,end=99)kg(1),kg(2),lunk,tb1,tb2,lr1,lr2,lb,no
  500 format(10x,a2,6x,a2,31x,a1,2i1,3x,2i1,19x,i1,7x,i1)
c--exp x line x group (sel)
  500 format(10x,8x,a2,1x,2a2,26x,a1,2i1,3x,2i1,19x,i1,7x,i1)
c--exp x group x flock
    1 read(1,500,end=99)kg(1),kg(3),kg(2),lunk,tb1,tb2,lr1,lr2,lb,no
  500 format(10x,7x,a1,a2,3x,a2,26x,a1,2i1,3x,2i1,19x,i1,7x,i1)
c--exp x group (sel)
    1 read(1,500,end=99)kg(2),kg(1),lunk,tb1,tb2,lr1,lr2,lb,no
  500 format(10x,8x,a2,3x,a2,26x,a1,2i1,3x,2i1,19x,i1,7x,i1)
c--exp x group x breed (xb)
    1 read(1,500,end=99)kg(3),kg(1),kg(2),lunk,tb1,tb2,lr1,lr2,lb,no
  500 format(10x,7x,a2,5x,a2,3x,a2,26x,a1,2i1,3x,2i1,17x,i1,3x,i1)
c--exp x brr x bre x group (xb)
    1 read(1,500,end=99)kg(4),kg(3),kg(2),kg(1),lunk,tb1,tb2,lr1,lr2,
     1lb,no
  500 format(10x,7x,a2,3x,a1,1x,a2,4x,a1,26x,a1,2i1,3x,2i1,17x,i1,3x,i1)
.fi
.PP
Options for parameter card no.1
.nf
  1)  'iprop'   a3   cols.1-3 
        a) ard - % not calculated for smallest subgroup
                 trips.,quads,quins lumped as multiples
.sp
        b) lon - % calculated for all subgroups
                 trips,quads,quins lumped as multiples
.sp
        c) ab9 - % calculated for all subgroups
                 trips,quads,quins treated separately
.sp
      'isr'   a3   cols.4-6
        a) nsr - if no service recordings
.sp
        b) blank - service section will be presented
.fi
.PP
Options for parameter card no.2
.nf
  2)  'nways'  i5   cols.1-5
        no. of variables to be controlled on e.g. 3 for exp. x line x group
        max. is 3 for iprop=ab9, 4 for the rest
.sp
      'islaut' i5   cols.6-10
        a) 1 if there are animals slaughtered  (i.e. 1 in col.10)
.sp
        b) blank otherwise
.fi
.PP
Options for parameter cards 3,4,5.
.nf
  3),4),5)  no. of cards here depends on 'nways'. There must be one card 
for each variable to be controlled on.
For each variable,four lines of print are allowed, each six characters 
plus one blank.These are printed under each other as headings on control 
fields.
     e.g.  cols.1-7   top line                   drop
                8-14  2nd line                   of
               15-21  3rd line     exp   line    ewe   
               22-28  4th line              
.sp
card(3) must be the name of the most frequently cycling way  e.g.drop of ewe
card(4) must be the name of the 2nd way                      e.g.line
card(5) must be the name of the least frequently cycling way e.g.exp
.bp
Examples :- 
                     expt
                     line
       drop   of     ewe
       type   of     ewe
                     flock
                     strain
       mating group
                     breed
              geno   type
       type   of     ram
.fi
.PP
Parameter card (6)
.nf
   6)   ihead   8a10    variable heading
               e.g. Longford Lambing 1987
.fi
.PP
Options for paramter cards 7-14.
.nf
   7),8),9),10)      khead    4(7a8/)
        fixed headings for columns on ewe side of tables
        four cards each punched up to col.56
.sp
   11),12),13),14)   lhead    4(6a10)/)
        fixed headings for columns on lamb side of tables
        four cards each punched to col.56 if 'iprop=ab9'
                             or to col.48 otherwise
.sp
Examples of cards 7-14 :-
c----- lb greater than 3
:         no.of ewes                                : 
:    al     :     dry      :         lambed         : 
:   dbl     :              :                        : 
:ej unk epl :ab nsr sr  tot:unk   s  t tr qd qn tot : fa
          no. of lambs                             :  
                                                   : exp
                                                   : rej
te unk sm  sf  tm  tf trm trf qdm qdf qnm qnf total:  
.sp
c---- lb less than or equal to 3
:         no.of ewes                                   :  
:      al      :       dry        :     lambed         :  
:     dbl      :                  :                    :  
: ej  unk  epl : ab  nsr sr   tot :unk   s   t   m tot :  
          no. of lambs                  : 
                                        : 
                                        :  exp
 fate  unk  sm  sf  tm  tf  mm  mf total:  rej
.fi
.bp
.SH PERCENTAGE ARRAY - P
.sp
.nf
P(1)     % dry/epl
.sp
P(2)     % s/epl    i.e. % of ewes producing 1 lamb/epl
P(3)     % t/epl    i.e. % of ewes producing 2 lambs/epl
P(4)     % m/epl    (or tr/epl if iprop=ab9)
P(5)     % qd/epl
P(6)     % qn/epl
.sp
P(7)     % lm/lb for singles
P(8)     % lm/lb for twins
P(9)     % lm/lb for triplets/multiples
P(10)    % lm/lb for quads
P(11)    % lm/lb for quins
.sp
P(12)    % el/epl
P(13)    % lb/el
P(14)    % lb/epl
P(15)    % lm/lb
P(16)    % lm/epl
.sp
iprop = ard  generates 1,7,8,9,14,15,16
iprop = ab9     "      all percentages 1-16
iprop = lon     "      all except 5,6,10,11
.fi
.bp
.SH EWE SIDE
.sp
.nf
ID(M,1)     total no. of ewes mated
     2)     no. of ewes absent lambing
     3)     no. of ewes present lambing
.sp
     4)     no. of ewes aborted
     5)     no. of ewes served     {if services  }     dry ewes
     6)     no. of ewes not served {were recorded}      "   "
     7)     total                                       "   "
.sp
     8)     no. of ewes with unknown no. of lambs      wet ewes
     9)     no. of ewes with 1 lamb                     "   "
    10)     no. of ewes with 2 lambs                    "   "
    11)     no. of ewes with >2 lambs (3 for ab9)       "   "
    12)     no. of ewes with 4 lambs                    "   "
    13)     no. of ewes with 5 lambs                    "   "
    14)     total                                       "   "
.fi
.SH LAMB SIDE
.nf
                                                            |KL(M,J)
     I =  1  2  3  4  5   6   7  8  9  10  11  12   13      |
         unk sm sf tm tf trm trf qm qf qnm quf tot  e.r.    |
J=1 nnm                                                     |
J=2 mm                                                      |
J=3 pm                                                      |
J=4 sl                                                      |
---------------------------------------------------------------------
  JL(M,I)                                                   | TL
.sp
1st subscript (M) refers to level of table
   1 = most divided  e.g. 3 way
   2 = summed over most frequently cycling variable  e.g. 2 way
   3 = summed over 1st & 2nd most frequently cycling variable  e.g. 1 way
.fi
