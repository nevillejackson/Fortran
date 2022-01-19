      FUNCTION LCARDS(LRECW)                                                              
C-----WRITTEN N.J. 1977                                                                   
C-----RETURNS NO OF CARDS FOR A RECORD OF LENGTH LRECW WORDS                              
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      K=MACHCD                                                                            
    1 IF(MOD(LRECW,K)) 5,6,7                                                              
    5 STOP                                                                                
    6 LCARDS=LRECW/K                                                                      
      GO TO 8                                                                             
    7 LCARDS=LRECW/K+1                                                                    
    8 RETURN                                                                              
      END                                                                                 
