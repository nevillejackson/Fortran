      SUBROUTINE LZSTRP(WORD,IBEG,ILEN)                                                   
C-----WRITTEN N.J. 1977                                                                   
C-----STRIPS LEADING ZEROS FROM FIELD IBEG,ILEN RIGHT JUST IN WORD                        
      INTEGER WORD(1)                                                                     
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
      JL=IBEG+ILEN-2                                                                      
      DO 6 J=IBEG,JL                                                                      
      IF(IJCHCM(IN(WORD,J),IN(KONST(28),MACHC))) 7,6,7                                    
    6 CALL OUT(WORD,J,IN(KONST(46),MACHC))                                                
    7 RETURN                                                                              
      END                                                                                 
