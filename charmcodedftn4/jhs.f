      SUBROUTINE JHS(JTM,JLEN,J,JNO,KTM,KLEN)                                             
C-----WRITTEN N.J. 1978                                                                   
C-----EXTRACT JTH HOLLERITH STRING FROM JTM() AND                                         
C----- PLACE KLEN CHARS IN KTM()                                                          
C-----                                                                                    
      DIMENSION JTM(1),KTM(1)                                                             
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
      JPOS=1                                                                              
      IEND=0                                                                              
      DO 1 JH=1,JNO                                                                       
C-----INTEGER LENGTH                                                                      
      ITYP=NXST(JTM,JLEN,JPOS,INT,ILEN,0,IEND)                                            
      IF(IEND) 999,3,999                                                                  
    3 IF(ITYP) 999,999,4                                                                  
C-----H                                                                                   
    4 IF(IJCHCM(IN(JTM,JPOS),IN(KONST(9),MACHC)))999,5,999                                
C-----STRING OF LENGTH INT                                                                
    5 JPOS=JPOS+1                                                                         
      IF(JH-J) 7,6,999                                                                    
    6 CALL COPYC(JTM,JPOS,KTM,1,INT)                                                      
      GO TO 8                                                                             
    7 JPOS=JPOS+INT                                                                       
      IF(JPOS-JLEN) 1,1,999                                                               
    1 CONTINUE                                                                            
  999 CALL LOGIC(3HJHS)                                                                   
    8 KLEN=INT                                                                            
      RETURN                                                                              
      END                                                                                 
