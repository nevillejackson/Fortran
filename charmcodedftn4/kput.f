      SUBROUTINE KPUT(K)                                                                  
C-----WRITTEN N.J. 1977                                                                   
C-----PRINT RECORD COUNTS                                                                 
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      WRITE(LP,1) K                                                                       
    1 FORMAT(16H RECORD COUNT = ,I8)                                                      
      RETURN                                                                              
      END                                                                                 
