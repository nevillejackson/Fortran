      double precision function quadf(v,a,n)                                             
c----- quadratic form v'Av
      implicit double precision (a-h,o-z)
      dimension v(1),a(20,1)                                            
      q=0.0                                                             
      do 1 i=1,n                                                        
      do 1 j=1,n                                                        
    1 q=q+v(i)*a(i,j)*v(j)                                              
      quadf=q                                                           
      return                                                            
      end                                                               
