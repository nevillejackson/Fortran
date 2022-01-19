      SUBROUTINE ORDKEY(IBEG,ILEN,IORD,ISEQ,KOUNTF,LIT,LREC,MKEY)                         
C-----WRITTEN N.J. 1978                                                                   
C-----COPY KOUNTF FIELDS INTO SINGLE STRING MKEY                                          
C-----COMPLEMENT DESC KEYS                                                                
C-----TRANSPOSE ASCII6 KEYS                                                               
C-----                                                                                    
      DIMENSION IBEG(1),ILEN(1),IORD(1),ISEQ(1),LIT(1),MKEY(1)                            
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
C-----                                                                                    
      IPOS=0                                                                              
      DO 1 K=1,KOUNTF                                                                     
      LB=IBEG(K)                                                                          
      LE=ILEN(K)+LB-1                                                                     
      IF(LE-LREC) 6,6,7                                                                   
    7 WRITE(LP,8)                                                                         
    8 FORMAT(24H LREC EXCEEDED IN ORDKEY)                                                 
      CALL JOBEND                                                                         
    6 DO 2 L=LB,LE                                                                        
      IPOS=IPOS+1                                                                         
      KEY=IN(LIT,L)                                                                       
      IF(ISEQ(K)) 999,10,11                                                               
   10 KEY=NEWCSQ(KEY)                                                                     
   11 IF(IORD(K)) 999,12,13                                                               
   13 CALL OUT(KEY,MACHC,COMPL(IN(KEY,MACHC)))                                            
   12 CALL OUT(MKEY,IPOS,IN(KEY,MACHC))                                                   
    2 CONTINUE                                                                            
    1 CONTINUE                                                                            
      RETURN                                                                              
  999 CALL LOGIC(6HORDKEY)                                                                
      RETURN                                                                              
      END                                                                                 
