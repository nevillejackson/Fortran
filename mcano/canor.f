      SUBROUTINE CANOR(N,MP,MQ,RR,ROOTS,WLAM,CANR,CHISQ,NDF,COEFR,      
     1                       COEFL,R)                                   
      DIMENSION RR(1),ROOTS(1),WLAM(1),CANR(1),CHISQ(1),NDF(1),COEFR(1),
     1          COEFL(1),R(1)                                           
      M=MP+MQ                                                           
      N1=0                                                              
      DO 105 I=1,M                                                      
      DO 105 J=1,M                                                      
      IF(I-J) 102,103,103                                               
  102 L=I+(J*J-J)/2                                                     
      GO TO 104                                                         
  103 L=J+(I*I-I)/2                                                     
  104 N1=N1+1                                                           
  105 R(N1)=RR(L)                                                       
      L=MP                                                              
      DO 108 J=2,MP                                                     
      N1=M*(J-1)                                                        
      DO 108 I=1,MP                                                     
      L=L+1                                                             
      N1=N1+1                                                           
  108 R(L)=R(N1)                                                        
      N2=MP+1                                                           
      L=0                                                               
      DO 110 J=N2,M                                                     
      N1=M*(J-1)                                                        
      DO 110 I=1,MP                                                     
      L=L+1                                                             
      N1=N1+1                                                           
  110 COEFL(L)=R(N1)                                                    
      L=0                                                               
      DO 120 J=N2,M                                                     
      N1=M*(J-1)+MP                                                     
      DO 120 I=N2,M                                                     
      L=L+1                                                             
      N1=N1+1                                                           
  120 COEFR(L)=R(N1)                                                    
      L=MP*MP+1                                                         
      K=L+MP                                                            
      CALL MINV(R,MP,DET,R(L),R(K))                                     
C-----PRINT DET (R11)                                                   
      PRINT 330,DET                                                     
  330 FORMAT(12H0DET(R11) = ,E15.5)                                     
      DO 140 I=1,MP                                                     
      N2=0                                                              
      DO 130 J=1,MQ                                                     
      N1=I-MP                                                           
      ROOTS(J)=0.0                                                      
      DO 130 K=1,MP                                                     
      N1=N1+MP                                                          
      N2=N2+1                                                           
  130 ROOTS(J)=ROOTS(J)+R(N1)*COEFL(N2)                                 
      L=I-MP                                                            
      DO 140 J=1,MQ                                                     
      L=L+MP                                                            
  140 R(L)=ROOTS(J)                                                     
      L=MP*MQ                                                           
      N3=L+1                                                            
      DO 160 J=1,MQ                                                     
      N1=0                                                              
      DO 160 I=1,MQ                                                     
      N2=MP*(J-1)                                                       
      SUM=0.0                                                           
      DO 150 K=1,MP                                                     
      N1=N1+1                                                           
      N2=N2+1                                                           
  150 SUM=SUM+COEFL(N1)*R(N2)                                           
      L=L+1                                                             
  160 R(L)=SUM                                                          
      L=L+1                                                             
      CALL NROOT(MQ,R(N3),COEFR,ROOTS,R(L))                             
      DO 210 I=1,MQ                                                     
      IF(ROOTS(I)) 220,220,165                                          
  165 CANR(I)=SQRT(ROOTS(I))                                            
      WLAM(I)=1.0                                                       
      DO 170 J=I,MQ                                                     
  170 WLAM(I)=WLAM(I)*(1.0-ROOTS(J))                                    
      FN=N                                                              
      FMP=MP                                                            
      FMQ=MQ                                                            
  175 CHISQ(I)=-(FN-0.5*(FMP+FMQ+1.0))*ALOG(WLAM(I))                    
      N1=I-1                                                            
      NDF(I)=(MP-N1)*(MQ-N1)                                            
      N1=MQ*(I-1)                                                       
      N2=MQ*(I-1)+L-1                                                   
      DO 180 J=1,MQ                                                     
      N1=N1+1                                                           
      N2=N2+1                                                           
  180 COEFR(N1)=R(N2)                                                   
      DO 200 J=1,MP                                                     
      N1=J-MP                                                           
      N2=MQ*(I-1)                                                       
      K=MP*(I-1)+J                                                      
      COEFL(K)=0.0                                                      
      DO 190 JJ=1,MQ                                                    
      N1=N1+MP                                                          
      N2=N2+1                                                           
  190 COEFL(K)=COEFL(K)+R(N1)*COEFR(N2)                                 
  200 COEFL(K)=COEFL(K)/CANR(I)                                         
  210 CONTINUE                                                          
  220 RETURN                                                            
      END                                                               
