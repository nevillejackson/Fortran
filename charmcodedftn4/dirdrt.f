      SUBROUTINE DIRDRT(LUNI,LUNO,KARD,IOF)                                               
      DIMENSION KARD(1)                                                                   
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
C---- WRITTEN R.E. 1977                                                                   
C---- READ AND WRITE DIRECTIVE. RETURNS IOF=1 IF EOF FOUND.                               
      IOF=0                                                                               
      CALL READCD(LUNI,KARD,IFLAG,MACHCD)                                                 
      IF(IFLAG) 10,5,999                                                                  
    5 CALL WRITLN(LUNO,KARD,0,MACHCD)                                                     
      RETURN                                                                              
   10 IOF=1                                                                               
      RETURN                                                                              
  999 CALL LOGIC(6HDIRDRT)                                                                
      RETURN                                                                              
      END                                                                                 
