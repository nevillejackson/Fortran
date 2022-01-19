      FUNCTION LWORDS(LREC)                                                               
C-----WRITTEN N.J. 1977                                                                   
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
C-----RETURNS NO OF WORDS FOR A RECORD OF LENGTH LREC CHARS                               
C-----MACHC CHARACTERS PER WORD                                                           
      K=MACHC                                                                             
    1 IF(MOD(LREC,K)) 5,6,7                                                               
    5 CALL JOBEND                                                                         
    6 LWORDS=LREC/K                                                                       
      GO TO 8                                                                             
    7 LWORDS=LREC/K+1                                                                     
    8 RETURN                                                                              
      END                                                                                 
