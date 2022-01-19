      double precision function hermf(v,w,a,n)                                           
c----- hermetian form v'Aw
      implicit double precision (a-h,o-z)
      dimension v(1),w(1),a(20,1)                                       
      h=0.0                                                             
      do 1 i=1,n                                                        
      do 1 j=1,n                                                        
    1 h=h+v(i)*a(i,j)*w(j)                                              
      hermf=h                                                           
      return                                                            
      end                                                               
