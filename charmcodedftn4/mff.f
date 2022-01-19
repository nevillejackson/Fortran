      FUNCTION MFF(IBEG,ILEN,KOUNTF,LIT,LREC)                                             
C-----WRITTEN N.J. 1977                                                                   
      DIMENSION IBEG(1),ILEN(1),LIT(1)                                                    
      COMMON /MACH/ MACHB,MACHC,MACHCD,INCD,LPERRS                                        
C-----EXTRACTS MULTIPLE FIELDS FROM STRING LIT AND MARRIES THEM INTO A                    
C-----STRING WHICH IS RETURNED VIA THE FUNCTION NAME                                      
C-----SINGLE STRING MUST NOT EXCEED MACHC CHARACTERS (1 WORD)                             
C-----AND IS LEFT JUSTIFIED , BLANK FILLED AS RETURNED BY NEXALF OR                       
C-----DECALF                                                                              
      CALL SETBLK(M,0,MACHC)                                                              
      IPOS=0                                                                              
      DO 1 K=1,KOUNTF                                                                     
      IB=IBEG(K)                                                                          
      IE=ILEN(K)+IB-1                                                                     
      IF(IE-LREC) 6,6,7                                                                   
    7 PRINT 8                                                                             
    8 FORMAT(20H0LREC EXCEEDED IN MF)                                                     
      CALL JOBEND                                                                         
    6 DO 2 I=IB,IE                                                                        
      IPOS=IPOS+1                                                                         
      IF(IPOS-MACHC) 3,3,4                                                                
    4 PRINT 5                                                                             
    5 FORMAT(21H0MACHC EXCEEDED IN MF)                                                    
      CALL JOBEND                                                                         
    3 CALL OUT(M,IPOS,IN(LIT,I))                                                          
    2 CONTINUE                                                                            
    1 CONTINUE                                                                            
      MFF=M                                                                               
      RETURN                                                                              
      END                                                                                 
