      FUNCTION HERMF(V,W,A,N)                                                             
      DIMENSION V(1),W(1),A(20,1)                                                         
      H=0.0                                                                               
      DO 1 I=1,N                                                                          
      DO 1 J=1,N                                                                          
    1 H=H+V(I)*A(I,J)*W(J)                                                                
      HERMF=H                                                                             
      RETURN                                                                              
      END                                                                                 
