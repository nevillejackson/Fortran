      SUBROUTINE LHOL(INT,ITM,LEN)                                                        
      DIMENSION ITM(1)                                                                    
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
C-----WRITTEN N.J. 1978                                                                   
C-----CONVERTS AN INTEGER VARIABLE TO A MULTI-WORD CHAR STRING ITM() OF                   
C-----LENGTH LEN                                                                          
C-----RIGHT JUSTIFIED BLANK FILLED IN ITM()                                               
      L=1                                                                                 
      CALL SETBLK(ITM,0,LEN)                                                              
      CALL CONVI(INT,ITM,L)                                                               
      L=L-1                                                                               
      LEADER=LEN-L                                                                        
      IF(LEADER) 999,1,2                                                                  
    2 DO 3 I=1,L                                                                          
      J=L-I+1                                                                             
      K=J+LEADER                                                                          
    3 CALL OUT(ITM,K,IN(ITM,J))                                                           
      CALL SETBLK(ITM,0,LEADER)                                                           
    1 RETURN                                                                              
  999 WRITE(LP,998) INT,L,LEN                                                             
  998 FORMAT(27H0LHOL CANNOT ENCODE INTEGER,I20,11H OF LENGTH ,I5,                        
     1 22H INTO FIELD OF LENGTH ,I5,11H CHARACTERS)                                       
      CALL JOBEND                                                                         
      RETURN                                                                              
      END                                                                                 
