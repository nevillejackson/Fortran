      SUBROUTINE ORDFLD(IBEG,ILEN,IORD,ISEQ,MC,KC,LIT,LREC,IFIELD,                        
     1   NFIELD)                                                                          
C-----WRITTEN N.J. 1977                                                                   
C-----EXTRACT ONE FIELD FROM LIT , TRANS IF ASCII6 , COMPL IF DESC                        
C-----IFIELD IS FIELD TRANS TO ASCII6 AND DESC IF REQ                                     
C-----NFIELD IS ORIGINAL FIELD IN DISPLAY                                                 
      DIMENSION IBEG(1),ILEN(1),IORD(1),ISEQ(1),LIT(1)                                    
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
      CALL MCHR(1,IFIELD,MACHC)                                                           
      CALL MCHR(46,NFIELD,MACHC)                                                          
      LB=IBEG(MC)                                                                         
      LE=ILEN(MC)+LB-1                                                                    
      IF(LE-LREC) 6,6,7                                                                   
    7 WRITE(LP,8)                                                                         
    8 FORMAT(24H LREC EXCEEDED IN ORDFLD)                                                 
      CALL JOBEND                                                                         
    6 IPOS=MACHC-ILEN(MC)                                                                 
      DO 2 L=LB,LE                                                                        
      IPOS=IPOS+1                                                                         
      KEY=IN(LIT,L)                                                                       
      CALL OUT(NFIELD,IPOS,IN(KEY,MACHC))                                                 
      IF(ISEQ(MC)) 999,10,11                                                              
   10 KEY=NEWCSQ(KEY)                                                                     
   11 IF(IORD(MC)) 999,12,13                                                              
   13 CALL OUT(KEY,MACHC,COMPL(IN(KEY,MACHC)))                                            
   12 CALL OUT(IFIELD,IPOS,IN(KEY,MACHC))                                                 
    2 CONTINUE                                                                            
      RETURN                                                                              
  999 CALL LOGIC(6HORDFLD)                                                                
      RETURN                                                                              
      END                                                                                 
