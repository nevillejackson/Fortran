      SUBROUTINE EOFR(LUN,KOUNT)                                                          
C     WRITTEN N.J. 1977                                                                   
C-----PRINTS ENDFILE MESSAGE AND RECORD COUNTS ON OUTPUT                                  
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      WRITE(LP,1) LUN,KOUNT                                                               
    1 FORMAT(18H EOF READ ON UNIT ,I3,20H  -- RECORD COUNT = ,I8)                         
      RETURN                                                                              
      ENTRY EOFW                                                                          
      WRITE(LP,2) LUN,KOUNT                                                               
    2 FORMAT(21H EOF WRITTEN ON UNIT  I3,20H  -- RECORD COUNT = ,I8)                      
      RETURN                                                                              
      END                                                                                 
