      FUNCTION INTFAC(NFAC,NTY,NT,NWAY)                                                   
      DIMENSION N(20)                                                                     
      KNT=0 $ I=0 $ K=0                                                                   
      NTMAX=NCR(NFAC,NTY)                                                                 
    1 I=I+1 $ IF(I.GT.NTY)GO TO 4                                                         
    2 K=K+1 $ IF(K.GT.NFAC)GO TO 4                                                        
      N1=NFAC-K $ N2=NTY-I                                                                
      N(K)=NCR(N1,N2)                                                                     
      KNT=KNT+N(K) $ IF(KNT.GT.NTMAX)GO TO 4                                              
      IF(NT.LE.KNT)3,2                                                                    
    3 KNT=KNT-N(K)                                                                        
      IF(I.LT.NWAY)GO TO 1                                                                
      INTFAC=K $ RETURN                                                                   
    4 PRINT 5,NFAC,NTY,NT,NWAY,I,K,KNT                                                    
    5 FORMAT(63H0  ERROR IN INTFAC - PARAMETER VALUES (NFAC, NTY, NT, NW                  
     .AY) ARE,4I5,//,47H   VALUES OF I, K, KNT WHEN ERROR DETECTED WERE,                  
     .3I5)                                                                                
      STOP                                                                                
      END                                                                                 
      FUNCTION NCR(N1,N2)                                                                 
      IF(N2.GT.0)GO TO 1                                                                  
      NCR=1 $ RETURN                                                                      
    1 XD=N2+1 $ XN=N1-N2 $ T=1.0                                                          
    2 XD=XD-1 $ XN=XN+1 $ T=XN/XD*T $ NTS=XD+0.1 $ IF(NTS.EQ.1)3,2                        
    3 NCR=T+0.5                                                                           
      RETURN                                                                              
      END                                                                                 
