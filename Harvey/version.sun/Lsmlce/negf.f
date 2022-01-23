      subroutine negf (t,beg,lfd,lit,i)                                 
c     -------------------------------------------                       
c     subroutine used to check for fortran type negative numbers        
c     ----------------------------------------------------              
      include 'com8'
      real*8 t                                                          
      integer beg                                                       
      character lit*(*)
      dimension beg(*),lfd(*)                                
      m=beg(i)                                                          
      m1=m+lfd(i)-1                                                     
    1 if (lit(m:m).ne.blank) go to 5                                       
      if (m.eq.m1) go to 10                                             
      m=m+1                                                             
      go to 1                                                           
    5 if (lit(m:m).ne.minus) go to 8                                       
      lit(m:m)=blank                                                       
      t=-1.                                                             
      go to 10                                                          
    8 t=1.                                                              
   10 continue                                                          
      return                                                            
      end                                                               
