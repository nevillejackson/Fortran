      subroutine diagmy(v,a,n,b)                                        
c-----pre and post multiply a matrix by a diagonalized vector)          
      implicit double precision (a-h,o-z)
      dimension v(1),a(20,1),b(20,1)                                    
      do 1 i=1,n                                                        
      do 1 j=1,n                                                        
    1 b(i,j)=v(i)*a(i,j)*v(j)                                           
      return                                                            
      end                                                               
