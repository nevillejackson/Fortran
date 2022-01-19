      SUBROUTINE SQUP(X,N)                                                                
      DIMENSION X(50,50)                                                                  
C     SQUARES UP A LOWER TRIANGULAR MATRIX                                                
      N1=N-1                                                                              
      DO 1 I=1,N1                                                                         
      I1=I+1                                                                              
      DO 1 J=I1,N                                                                         
    1 X(I,J)=X(J,I)                                                                       
      RETURN                                                                              
      END                                                                                 
