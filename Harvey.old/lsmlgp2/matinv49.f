*TEXT                                                                                     
      SUBROUTINE MATINV(ARRAY,NBRC,NERC,NLHM)                             150001          
C     SUBROUTINE FOR INVERSION OF SYMMETRICAL HALF-STORED MATRIX          150002          
C     ----------------------------------------------------                150003          
      DIMENSION ARRAY(1)                                                  150004          
C-----                                                                                    
C-----THIS SECTION CHECKS FOR CONFOUNDING                                                 
C-----                                                                                    
      EPS=1.0E-06                                                                         
C-----FIND SMALLEST DIAGONAL ELEMENT                                                      
      K3=NLHM*(NBRC-1)-NBRC*(NBRC-3)/2                                                    
      DMIN=ARRAY(K3)                                                                      
      IF(NBRC.EQ.NERC)GO TO 37                                                            
      NBRC2=NBRC+1                                                                        
      DO 1 L=NBRC2,NERC                                                                   
      K3=NLHM*(L-1)-L*(L-3)/2                                                             
      IF(DMIN-ARRAY(K3)) 1,1,2                                                            
    2 DMIN=ARRAY(K3)                                                                      
    1 CONTINUE                                                                            
C-----CHECK MIN DIAG ELEMENT POSITIVE                                                     
   37 IF(DMIN) 3,4,4                                                                      
    3 WRITE(6,1000) DMIN                                                                  
 1000 FORMAT(55H0SMALLEST DIAGONAL ELEMENT OF MATRIX TO BE INVERTED IS ,                  
     1  E18.8/21H0EXECUTION TERMINATED)                                                   
      STOP                                                                                
    4 CONTINUE                                                                            
C-----LOOP PICKS PIVOTAL ELEMENT                                                          
      DMIN=DMIN*EPS                                                                       
    5 DO 28 L=NBRC,NERC                                                                   
      K3=NLHM*(L-1)-L*(L-3)/2                                             150006          
      IF(ABS(ARRAY(K3))-DMIN) 22,22,23                                                    
C-----BRANCH WHEN SIGNIFICANCE LOST                                                       
   22 WRITE(6,1001) L                                                                     
 1001 FORMAT(12H0ROW NUMBER ,I3,46H IS CONFOUNDED WITH ONE OR MORE PRECE                  
     1DING ROWS)                                                                          
      WRITE(6,1002)L                                                                      
 1002 FORMAT(41H0THE CONFOUNDING IS SUCH THAT ROW NUMBER ,I3,67H IS EQUA                  
     1L TO THE FOLLOWING LINEAR COMBINATION OF THE PRECEDING ROWS)                        
      WRITE(6,1003)                                                                       
 1003 FORMAT(25H0     ROW     COEFFICIENT)                                                
C-----SET PIVOTAL ELEMENT  AND COLUMN TO ZERO                                             
      ARRAY(K3)=-0.0                                                                      
      L2=L+1                                                                              
      IF(L2.GT.NERC)GO TO 53                                                              
      DO 7 I=L2,NERC                                                                      
      K1=K3+I-L                                                                           
    7 ARRAY(K1)=-0.0                                                                      
C-----PRINT OUT PIVOTAL ROW AND SET IT TO ZERO                                            
   53 L2=L-1                                                                              
      DO 9 J=1,L2                                                                         
      K5=NLHM*(J-1)-J*(J-3)/2+L-J                                                         
      WRITE(6,1004) J,ARRAY(K5)                                                           
    9 ARRAY(K5)=-0.0                                                                      
 1004 FORMAT(1H ,I8,E16.8)                                                                
C-----NEXT L                                                                              
      GO TO 28                                                                            
C-----BRANCH WHEN SIGNIFICANCE NOT LOST                                                   
   23 CONTINUE                                                                            
C-----                                                                                    
C-----END OF SECTION WHICH CHECKS CONFOUNDING                                             
C-----                                                                                    
      RECIP=1./ARRAY(K3)                                                  150007          
      ARRAY(K3)=-RECIP                                                    150008          
C-----LOOP PICKS ROW                                                                      
      DO 20 I=NBRC,NERC                                                   150009          
      K11=NLHM*(I-1)-I*(I-3)/2                                            150010          
      IF (I-L) 6,20,8                                                     150011          
    6 K1=K11+L-I                                                          150012          
      GO TO 10                                                            150013          
    8 K1=K3+I-L                                                           150014          
   10 R=RECIP*ARRAY(K1)                                                   150015          
C-----LOOP PICKS COLUMN                                                                   
      DO 18 J=I,NERC                                                      150016          
      K4=K11+J-I                                                          150017          
      IF (J-L) 12,18,14                                                   150018          
   12 K5=NLHM*(J-1)-J*(J-3)/2+L-J                                         150019          
      GO TO 16                                                            150020          
   14 K5=K3+J-L                                                           150021          
   16 ARRAY(K4)=ARRAY(K4)-R*ARRAY(K5)                                     150022          
   18 CONTINUE                                                            150023          
      ARRAY(K1)=R                                                         150024          
   20 CONTINUE                                                            150025          
   28 CONTINUE                                                                            
      DO 32 I=NBRC,NERC                                                   150026          
      DO 32 J=I,NERC                                                      150027          
      K1=NLHM*(I-1)-I*(I-3)/2+J-I                                         150028          
      ARRAY(K1)=-ARRAY(K1)                                                150029          
   32 CONTINUE                                                            150030          
      RETURN                                                              150031          
      END                                                                 150032          
*ENDTEXT                                                                                  
