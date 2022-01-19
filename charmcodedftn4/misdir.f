      SUBROUTINE MISDIR(MESS,IBOM)                                                        
C---- WRITTEN R.E. 1977                                                                   
C---- USED BY PROGRAM SHEET TO CHECK FOR REQUIRED DIRECTIVES                              
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      DIMENSION MESS(1)                                                                   
      IBOM=1                                                                              
      WRITE(LP,1) MESS                                                                    
    1 FORMAT(1H ,A5,18H DIRECTIVE MISSING)                                                
      RETURN                                                                              
      END                                                                                 
