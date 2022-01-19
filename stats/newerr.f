      SUBROUTINE NEWERR(SSCP,LV,NW,NERR)                                                  
      DIMENSION SSCP(LV,LV, 9)                                                            
      DO 1 L=1,LV                                                                         
      DO 1 LL=1,LV                                                                        
    1 SSCP(L,LL,NW)=SSCP(L,LL,NERR)                                                       
      RETURN                                                                              
      END                                                                                 
