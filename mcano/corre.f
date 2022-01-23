      SUBROUTINE CORRE (N,M,IO,X,XBAR,STD,RX,R,B,D,T)                   
      DIMENSION X(1),XBAR(1),STD(1),RX(1),R(1),B(1),D(1),T(1)           
C                                                                       
C     INITIALIZATION                                                    
C                                                                       
      DO 100 J=1,M                                                      
      B(J)=0.0                                                          
  100 T(J)=0.0                                                          
      K=(M*M+M)/2                                                       
      DO 102 I=1,K                                                      
  102 R(I)=0.0                                                          
      FN=N                                                              
      L=0                                                               
C                                                                       
      IF(IO) 260,127,105                                                
C                                                                       
C     READ STATISTICS INSTEAD OF DATA (IO=-1)
C                                                                       
  260 CALL STATIN (M,N,XBAR,STD,RX,R,B)                                 
      RETURN                                                            
C                                                                       
C     DATA ARE ALREADY IN CORE (IO=1)
C                                                                       
  105 DO 108 J=1,M                                                      
      DO 107 I=1,N                                                      
      L=L+1                                                             
  107 T(J)=T(J)+X(L)                                                    
      XBAR(J)=T(J)                                                      
  108 T(J)=T(J)/FN                                                      
C                                                                       
      DO 115 I=1,N                                                      
      JK=0                                                              
      L=I-N                                                             
      DO 110 J=1,M                                                      
      L=L+N                                                             
      D(J)=X(L)-T(J)                                                    
  110 B(J)=B(J)+D(J)                                                    
      DO 115 J=1,M                                                      
      DO 115 K=1,J                                                      
      JK=JK+1                                                           
  115 R(JK)=R(JK)+D(J)*D(K)                                             
      GO TO 205                                                         
C                                                                       
C     READ OBSERVATIONS AND CALCULATE TEMPORARY                         
C     MEANS FROM THESE DATA IN T(J)       .......(IO=0)
C                                                                       
  127 IF(N-M) 130, 130, 135                                             
  130 KK=N                                                              
      GO TO 137                                                         
  135 KK=M                                                              
  137 DO 140 I=1,KK                                                     
      CALL DATA (M,D)                                                   
      DO 140 J=1,M                                                      
      T(J)=T(J)+D(J)                                                    
      L=L+1                                                             
  140 RX(L)=D(J)                                                        
      FKK=KK                                                            
      DO 150 J=1,M                                                      
      XBAR(J)=T(J)                                                      
  150 T(J)=T(J)/FKK                                                     
C                                                                       
C     CALCULATE SUMS OF CROSS-PRODUCTS OF DEVIATIONS                    
C     FROM TEMPORARY MEANS FOR M OBSERVATIONS                           
C                                                                       
      L=0                                                               
      DO 180 I=1,KK                                                     
      JK=0                                                              
      DO 170 J=1,M                                                      
      L=L+1                                                             
  170 D(J)=RX(L)-T(J)                                                   
      DO 180 J=1,M                                                      
      B(J)=B(J)+D(J)                                                    
      DO 180 K=1,J                                                      
      JK=JK+1                                                           
  180 R(JK)=R(JK)+D(J)*D(K)                                             
C                                                                       
      IF(N-KK) 205, 205, 185                                            
C                                                                       
C     READ THE REST OF OBSERVATIONS ONE AT A TIME, SUM                  
C     THE OBSERVATION, AND CALCULATE SUMS OF CROSS-                     
C     PRODUCTS OF DEVIATIONS FROM TEMPORARY MEANS                       
C                                                                       
  185 KK=N-KK                                                           
      DO 200 I=1,KK                                                     
      JK=0                                                              
      CALL DATA (M,D)                                                   
      DO 190 J=1,M                                                      
      XBAR(J)=XBAR(J)+D(J)                                              
      D(J)=D(J)-T(J)                                                    
  190 B(J)=B(J)+D(J)                                                    
      DO 200 J=1,M                                                      
      DO 200 K=1,J                                                      
      JK=JK+1                                                           
  200 R(JK)=R(JK)+D(J)*D(K)                                             
C                                                                       
C     CALCULATE MEANS                                                   
C                                                                       
  205 JK=0                                                              
      DO 210 J=1,M                                                      
      XBAR(J)=XBAR(J)/FN                                                
C                                                                       
C     ADJUST SUMS OF CROSS-PRODUCTS OF DEVIATIONS                       
C     FROM TEMPORARY MEANS                                              
C                                                                       
      DO 210 K=1,J                                                      
      JK=JK+1                                                           
  210 R(JK)=R(JK)-B(J)*B(K)/FN                                          
C                                                                       
C     CALCULATE CORRELATION COEFFICIENTS                                
C                                                                       
      JK=0                                                              
      DO 220 J=1,M                                                      
      JK=JK+J                                                           
  220 STD(J)= SQRT( ABS(R(JK)))                                         
      DO 230 J=1,M                                                      
      DO 230 K=J,M                                                      
      JK=J+(K*K-K)/2                                                    
      L=M*(J-1)+K                                                       
      RX(L)=R(JK)                                                       
      L=M*(K-1)+J                                                       
      RX(L)=R(JK)                                                       
      IF(STD(J)*STD(K)) 225, 222, 225                                   
  222 R(JK)=0.0                                                         
      GO TO 230                                                         
  225 R(JK)=R(JK)/(STD(J)*STD(K))                                       
  230 CONTINUE                                                          
C                                                                       
C     CALCULATE STANDARD DEVIATIONS                                     
C                                                                       
      FN=SQRT(FN-1.0)                                                   
      DO 240 J=1,M                                                      
  240 STD(J)=STD(J)/FN                                                  
C                                                                       
C     COPY THE DIAGONAL OF THE MATRIX OF SUMS OF CROSS-PRODUCTS OF      
C     DEVIATIONS FROM MEANS.                                            
C                                                                       
      L=-M                                                              
      DO 250 I=1,M                                                      
      L=L+M+1                                                           
  250 B(I)=RX(L)                                                        
      RETURN                                                            
      END                                                               
