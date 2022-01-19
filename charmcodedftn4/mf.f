      SUBROUTINE MF(IBEG,ILEN,KOUNTF,LIT,LREC,JST,JLEN)                                   
C-----WRITTEN N.J. 1977                                                                   
      DIMENSION IBEG(1),ILEN(1),LIT(1)                                                    
      DIMENSION JST(1)                                                                    
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
C-----EXTRACTS MULTIPLE FIELDS FROM STRING LIT AND MARRIES THEM INTO A                    
C-----STRING WHICH IS RETURNED VIA JST()                                                  
C-----STRING LENGTH IS JLEN CHARS                                                         
      IPOS=0                                                                              
      DO 1 K=1,KOUNTF                                                                     
      IB=IBEG(K)                                                                          
      IE=ILEN(K)+IB-1                                                                     
      IF(IE-LREC) 6,6,7                                                                   
    7 WRITE(LP,8)                                                                         
    8 FORMAT(20H0LREC EXCEEDED IN MF)                                                     
      CALL JOBEND                                                                         
    6 DO 2 I=IB,IE                                                                        
      IPOS=IPOS+1                                                                         
      CALL OUT(JST,IPOS,IN(LIT,I))                                                        
    2 CONTINUE                                                                            
    1 CONTINUE                                                                            
      JLEN=IPOS                                                                           
      RETURN                                                                              
      END                                                                                 
