      SUBROUTINE ORDINL(IBEG,ILEN,IORD,ISEQ,KOUNTF,LIT,LREC,MULTI)                        
C-----WRITTEN N.J. 1977                                                                   
C-----COPY KOUNTF FIELDS INTO STRING MULTI                                                
C-----TRANSPOSE ASCII6 KEYS                                                               
C-----COMPLEMENT DESCENDING KEYS                                                          
      DIMENSION IBEG(1),ILEN(1),IORD(1),ISEQ(1),LIT(1),MULTI(1)                           
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
C-----                                                                                    
      DO 1 K=1,KOUNTF                                                                     
      CALL ORDFLD(IBEG,ILEN,IORD,ISEQ,K,KOUNTF,LIT,LREC,IFIELD,IDUM)                      
      MULTI(K)=IFIELD                                                                     
    1 CONTINUE                                                                            
      RETURN                                                                              
  999 CALL LOGIC(6HORDINL)                                                                
      RETURN                                                                              
      END                                                                                 
