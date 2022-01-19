      SUBROUTINE LENGET(LREC,LRECW)                                                       
C-----WRITTEN N.J. 1977                                                                   
C-----READS REC LENGTH IN CHARS , CONVERTS TO WORDS AND RETURNS BOTH                      
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      READ (LC,5) LREC                                                                    
    5 FORMAT(16I5)                                                                        
      LRECW=LWORDS(LREC)                                                                  
      WRITE(LP,6) LREC,LRECW                                                              
    6 FORMAT(17H RECORD LENGTH = ,I5,14H CHARACTERS = ,I5,6H WORDS)                       
      RETURN                                                                              
      END                                                                                 
