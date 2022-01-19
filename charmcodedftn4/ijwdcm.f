      FUNCTION IJWDCM(IWD,JWD)                                                            
C-----WRITTEN N.J. 1978                                                                   
C-----NOT MACHINE DEPENDENT BUT MAY BE MORE EFFICIENTLY DONE USING                        
C----- FUNCTIONS AVAILABLE ON PARTICULAR MACHINES                                         
C-----COMPARES TWO WORDS IWD AND JWD AT EVERY CHARACTER POSITION                          
C----- FROM LEFT TO RIGHT                                                                 
C-----RETURNS -1 IWD.LT.JWD                                                               
C-----         0 IWD.EQ.JWD                                                               
C-----        +1 IWD.GT.JWD                                                               
C-----                                                                                    
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      DO 1 K=1,MACHC                                                                      
      IF(IJCHCM(IN(IWD,K),IN(JWD,K))) 2,1,3                                               
    1 CONTINUE                                                                            
      IJWDCM=0                                                                            
      GO TO 4                                                                             
    2 IJWDCM=-1                                                                           
      GO TO 4                                                                             
    3 IJWDCM=+1                                                                           
    4 RETURN                                                                              
      END                                                                                 
