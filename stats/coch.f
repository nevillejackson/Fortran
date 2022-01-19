      FUNCTION COCH(DF,V,K,M,KP)                                                          
      DIMENSION DF(1),V(1)                                                                
      KP=0 $ COCH=VMAX=VSUM=TOT=0.0                                                       
      DO 10 I=1,K                                                                         
      IF(V(I).GT.VMAX)VMAX=V(I)                                                           
      TOT=TOT+DF(I)                                                                       
   10 VSUM=VSUM+V(I)                                                                      
      IF(VMAX.LT.0.000001)1,2                                                             
    1 KP=1 $ RETURN                                                                       
    2 COCH=VMAX/VSUM $ M=TOT/K $ RETURN                                                   
      END                                                                                 
