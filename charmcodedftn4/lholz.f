      SUBROUTINE LHOLZ(INT,ITM,LEN)                                                       
      DIMENSION ITM(1)                                                                    
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
C-----WRITTEN N.J. 1978                                                                   
C-----CONVERTS AN INTEGER VARIABLE TO A MULTI-WORD CHAR STRING ITM() OF                   
C-----LENGTH LEN                                                                          
C-----RIGHT JUSTIFIED , ZERO FILLED IN ITM()                                              
      L=1                                                                                 
      CALL CONVI(INT,ITM,L)                                                               
      L=L-1                                                                               
      LEADER=LEN-L                                                                        
      IF(LEADER) 999,1,2                                                                  
    2 DO 3 I=1,L                                                                          
      J=L-I+1                                                                             
      K=J+LEADER                                                                          
    3 CALL OUT(ITM,K,IN(ITM,J))                                                           
      DO 4 I=1,LEADER                                                                     
    4 CALL OUT(ITM,I,IN(KONST(28),MACHC))                                                 
    1 RETURN                                                                              
  999 WRITE(LP,998) INT,L,LEN                                                             
  998 FORMAT(28H0LHOLZ CANNOT ENCODE INTEGER,I20,11H OF LENGTH ,I5,                       
     1 22H INTO FIELD OF LENGTH ,I5,11H CHARACTERS)                                       
      CALL JOBEND                                                                         
      RETURN                                                                              
      END                                                                                 
