      SUBROUTINE DIAGMY(V,A,N,B)                                                          
C-----PRE AND POST MULTIPLY A MATRIX BY A DIAGONALIZED VECTOR)                            
      DIMENSION V(1),A(20,1),B(20,1)                                                      
      DO 1 I=1,N                                                                          
      DO 1 J=1,N                                                                          
    1 B(I,J)=V(I)*A(I,J)*V(J)                                                             
      RETURN                                                                              
      END                                                                                 
