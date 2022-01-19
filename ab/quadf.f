      FUNCTION QUADF(V,A,N)                                                               
      DIMENSION V(1),A(20,1)                                                              
      Q=0.0                                                                               
      DO 1 I=1,N                                                                          
      DO 1 J=1,N                                                                          
    1 Q=Q+V(I)*A(I,J)*V(J)                                                                
      QUADF=Q                                                                             
      RETURN                                                                              
      END                                                                                 
