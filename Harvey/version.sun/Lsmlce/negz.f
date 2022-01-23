      subroutine negz  (t,beg,lfd,lit,i)                                
c     -------------------------------------------                       
c     subroutine used to check for 1620 type negative numbers           
c     -------------------------------------------                       
      real*8 t                                                          
      integer beg                                                       
      character lit*(*)
      dimension beg(*),lfd(*)                                
      m=beg(i)+lfd(i)-1                                                 
      t=1.0                                                             
      if(kovp29(lit,m)) 1,2,2
    1 t=-1.0
    2 return                                                            
      end                                                               
