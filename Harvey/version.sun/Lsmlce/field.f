      subroutine field (icod,l,ic,l7,j)                                 
c     -------------------------------------------                       
c     combines digits from ic array                                     
c     ----------------------------------------------------              
      character ic*(*)
      include 'com8'
      character jc*1
c-----extracts field ic(l7:l), converts to integer, and
c-----                       puts integer result in icod
c-----  j=ncds=current card no on input
c-----  j=0 on output -> ok
c-----  j=1 on output -> last char of field blank or + or -
c-----                   or any char of field non-numeric
c-----                   and not + or - or blank.
      len=l-l7+1
      ncds=j                                                            
      j=0                                                               
      icod=0                                                            
      if=0                                                              
      do 3 l6=l7,l                                                      
      jc=ic(l6:l6)
      if(jc.ge.zero.and.jc.le.nine) go to 1
      if(jc.eq.minus.or.jc.eq.plus) go to 2
      if(jc.ne.blank) go to 4
    2 if(l6.eq.l) go to 4
      if (if.eq.0) go to 3                                              
      write (6,1003) l6,ncds                                            
 1003 format (1h ,17hblank card column,i4,12h on card no.,i6,22h has bee
     1n set to zero./' or minus or plus following a digit.')                                                   
      ic(l6:l6)=zero                                                          
    1 if=1                                                              
    3 continue                                                          
      icod=ixa(ic(l7:l))
      return                                                            
    4 j=1                                                               
      return                                                            
      end                                                               
