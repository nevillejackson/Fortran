      SUBROUTINE CLEARC(CARD,I,LREC)                                                      
C                                                                                         
      DIMENSION CARD( 10 )                                                                
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
C                                                                                         
C     CLEARC                            WRITTEN 1974 AUTHOR J.N.                          
C     ------                                                                              
C                                                                                         
C     CALL CLEARC(CARD,I,LREC)                                                            
C                                                                                         
C     ROUTINE TO BLANK FILL A RECORD FROM CARD(I) TO CARD(LREC) . ON                      
C     EXIT I IS UNCHANGED                                                                 
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     CARD    LREC COLUMN CARD                                                            
C     I       POINTS TO WHERE CLEARING SHOULD START                                       
C     LREC    RECORD LENGTH IN CHARS                                                      
C                                                                                         
      IF(I.GT.LREC) RETURN                                                                
C                                                                                         
      DO 100 J=I,LREC                                                                     
  100 CALL OUT(CARD,J,IN(KONST(46),MACHC))                                                
C                                                                                         
      RETURN                                                                              
C                                                                                         
      END                                                                                 
