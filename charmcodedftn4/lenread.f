      SUBROUTINE LNREAD(LREC,LRECW,LUN)                                                   
C --- WRITTEN R.E. 1977                                                                   
C --- ROUTINE TO READ RECORD LENGTH IN CHARACTERS FROM DATA FILE LUN,                     
C --- CONVERT TO WORDS AND RETURN BOTH.                                                   
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      CALL READUN(LUN,LREC,IFLAG,1)                                                       
      IF(IFLAG) 4,2,999                                                                   
    2 LREC=INUM(LREC)                                                                     
      LRECW=LWORDS(LREC)                                                                  
      GO TO 3                                                                             
    4 LREC=0                                                                              
      LRECW=0                                                                             
    3 CONTINUE                                                                            
      WRITE(LP,6) LUN,LREC,LRECW                                                          
    6 FORMAT(11H FILE TAPE ,I3,26H     RECORD LENGTH READ = ,I5,14H CHAR                  
     .ACTERS = ,I5,6H WORDS)                                                              
      RETURN                                                                              
      ENTRY LNWRIT                                                                        
C --- WRITE RECORD LENGTH IN CHARACTERS AS FIRST RECORD ON FILE LUN,                      
C --- CONVERT TO WORDS AND RETURN BOTH.                                                   
      LRECW=LWORDS(LREC)                                                                  
      WRITE(LP,7) LUN,LREC,LRECW                                                          
    7 FORMAT(11H FILE TAPE ,I3,29H     RECORD LENGTH WRITTEN = ,I5,14H C                  
     .HARACTERS = ,I5,6H WORDS)                                                           
      CALL WRITUN(LUN,IHOL(LREC),1)                                                       
      RETURN                                                                              
  999 CALL LOGIC(6HLNREAD)                                                                
      RETURN                                                                              
      END                                                                                 
