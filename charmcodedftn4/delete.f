      SUBROUTINE DELETE(LIT,LCOL,LENG,LREC)                                               
C-----WRITTEN N.J. 1977                                                                   
C-----DELETES LENG CHARS BEG AT COL LCOL FROM RECORD LIT OF LENGTH LREC CHARS             
C-----LEFT SHIFTS TRAILING CHARS AND PADS END OF STRING WITH BLANKS                       
      DIMENSION LIT(1)                                                                    
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
      IF(LENG) 4,4,5                                                                      
    5 IEND=LREC-LENG                                                                      
      IF(LCOL-IEND)3,3,4                                                                  
    3 DO 1 I=LCOL,IEND                                                                    
    1 CALL OUT(LIT,I,IN(LIT,I+LENG))                                                      
      IBEG=IEND+1                                                                         
      DO 2 I=IBEG,LREC                                                                    
    2 CALL OUT(LIT,I,IN(KONST(46),MACHC))                                                 
    4 RETURN                                                                              
      END                                                                                 
