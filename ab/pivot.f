      SUBROUTINE PIVOT (X,N,M)                                                            
      DIMENSION X(50,50)                                                                  
C     DOES A SINGLE PIVOT STEP ON THE DIAGONAL                                            
      IF(ABSF(X(M,M)).LE.0.000001)5,11                                                    
   11 DO 1 I=1,N                                                                          
      DO 1 J=1,N                                                                          
      IF(I.EQ.M.OR.J.EQ.M)1,8                                                             
    8 X(I,J)=X(I,J)-X(I,M)*X(M,J)/X(M,M)                                                  
    1 CONTINUE                                                                            
      DO 2 I=1,N                                                                          
      IF(I.EQ.M)2,9                                                                       
    9 X(I,M)=-X(I,M)/X(M,M)                                                               
    2 CONTINUE                                                                            
      DO 3 J=1,N                                                                          
      IF(J.EQ.M)3,10                                                                      
   10 X(M,J)=X(M,J)/X(M,M)                                                                
    3 CONTINUE                                                                            
      X(M,M)=1.0/X(M,M)                                                                   
      RETURN                                                                              
    5 PRINT6,M                                                                            
    6 FORMAT(16H0PIVOT STEP ZERO I3)                                                      
      RETURN                                                                              
      END                                                                                 
