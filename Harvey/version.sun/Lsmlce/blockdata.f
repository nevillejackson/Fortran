      block data 
      include 'com8'
      include 'com9'
      common /stdio/lin,lout,lmess
      data lin,lout,lmess /5,6,6/
      data blank,zero,nine,minus,plus
     + /' ','0','9','-','+'/
c-----
c-----dimensions defined for com9          orig value
c-----  m5000 - lhm*(lhm+1)/2 + lhm*rhm    5000
c-----  m4000 - random cl with blup        4000
c-----  m1000 - major cl with nab=4        1000
c-----   m300 - subclasses                  300
c-----   m200 - cl for which const fitted   200
c-----   m100 - sum of ncln, sum of ncl     100
c-----    m99 - lhm                          99
c-----    m91 - lines of anova               91
c-----    m90 - npr                          90
c-----    m81 - cl in me or ne with poly     81
c-----    m50 - nne + nnea                   50
c-----    m40 - me or ne with indiv regn     40
c-----    m35 - rhm                          35
c-----    m30 - n2f                          30
c-----    m20 - nme + nmea                   20
c-----    m13 - me or ne with poly           13
c-----    m10 - control columns              10
c-----
c----- note - arrays in com1,com1p,com2,com3 must be adjusted 
c-----        if these are changed
c-----      - also local arrays in polyab.f, suafmm.f, vector.f
c-----
      data m5000,m4000,m1000,m300,m200,m100,m99,m91,m90,m81
     +     ,m50,m40,m35,m30,m20,m13,m10
     +    /60100,4000,1000,300,200,100,200,91,90,81
     +     ,50,40,200,30,20,13,10/
c-----
      end
