      SUBROUTINE MCH(NCH,MST,LEN)                                                         
C-----USED FOR BUILDING MULTI-CHARACTER STRINGS                                           
C-----ADDS CHAR NO NCH FROM /CONST/ ONTO POSITION INDEX OF STRING MST()                   
C-----OF LENGTH LEN CHARS , PADDED TO LENX CHARS                                          
C-----UPDATES INDEX                                                                       
      DIMENSION MST(1)                                                                    
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
      IF(INDEX.LT.1.OR.INDEX.GT.LEN) GO TO 3                                              
      CALL OUT(MST,INDEX,IN(KONST(NCH),MACHC))                                            
      INDEX=INDEX+LEAN                                                                    
    3 RETURN                                                                              
C-----                                                                                    
C-----ENTRY POINTS MCHR , MCHL INITIALIZE INDEX ,                                         
C-----  SET UPDATING DIRECTION FOR R OR L JUST WORD ,                                     
C-----  AND FILL ENTIRE WORD MWD WITH FILL CHAR NO NCH FROM /CONST/                       
C-----                                                                                    
      ENTRY MCHR                                                                          
      LENX=LWORDS(LEN)*MACHC                                                              
      INDEX=LENX                                                                          
      LEAN=-1                                                                             
    2 DO 1 I=1,LENX                                                                       
    1 CALL OUT(MST,I,IN(KONST(NCH),MACHC))                                                
      RETURN                                                                              
C-----                                                                                    
      ENTRY MCHL                                                                          
      LENX=LWORDS(LEN)*MACHC                                                              
      INDEX=1                                                                             
      LEAN=1                                                                              
      GO TO 2                                                                             
      END                                                                                 
