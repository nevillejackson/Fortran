      SUBROUTINE WRDPOS(NWD,IPOS,ICOL)                                                    
C-----WRITTEN R.E. 1977                                                                   
C-----CONVERTS COLUMN NO ICOL TO WORD NO NWD AND CHAR NO WITHIN WORD IPOS                 
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      NWD=ICOL/MACHC                                                                      
      IPOS=ICOL-NWD*MACHC                                                                 
      IF(IPOS)1,2,3                                                                       
    1 CALL LOGIC(6HWRDPOS)                                                                
      STOP                                                                                
    2 IPOS=MACHC                                                                          
      RETURN                                                                              
    3 NWD=NWD+1                                                                           
      RETURN                                                                              
      END                                                                                 
