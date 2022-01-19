      FUNCTION JRIGHT(WORD)                                                               
C-----WRITTEN N.J. 1977                                                                   
C-----RETURNS RIGHT JUST BLANK FILLED FORM OF WORD WHICH REMAINS                          
C----- UNCHANGED - IE LEFT JUST BLANK FILLED                                              
      INTEGER WORD                                                                        
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
      KOPY=WORD                                                                           
      KL=MACHC-1                                                                          
      DO 1 I=1,MACHC                                                                      
      IF(LETTER(IN(KOPY,MACHC))) 2,3,3                                                    
    2 DO 4 K=1,KL                                                                         
      L=MACHC-K+1                                                                         
    4 CALL OUT(KOPY,L,IN(KOPY,L-1))                                                       
      CALL OUT(KOPY,1,IN(KONST(46),MACHC))                                                
    1 CONTINUE                                                                            
    3 JRIGHT=KOPY                                                                         
      RETURN                                                                              
      END                                                                                 
