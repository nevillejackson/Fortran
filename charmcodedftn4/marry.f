      PROGRAM MARRY(TAPE10=/1000,TAPE11=/1000,TAPE12=/1000                                
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C-----WRITTEN N.J. 1977                                                                   
C-----REWRITTEN R.E. 1977                                                                 
C-----MARRY MATCHED RECORDS FROM CHARM FILES TAPE10 & 11                                  
C-----INTO SINGLE RECORD WRITTEN ON CHARM FILE TAPE12                                     
      DIMENSION LITP(100),LITS(100)                                                       
      DIMENSION LIT(100)                                                                  
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      CALL DEFINE                                                                         
      WRITE(LP,10)                                                                        
   10 FORMAT(10H0MARRY RUN)                                                               
      REWIND 10                                                                           
      REWIND 11                                                                           
      REWIND 12                                                                           
      CALL LNREAD(LRECP,LRECWP,10)                                                        
      CALL LNREAD(LRECS,LRECWS,11)                                                        
      LREC=LRECP+LRECS                                                                    
      CALL LNWRIT(LREC,LRECW,12)                                                          
      LAST=LRECW+1                                                                        
      LASTP=LRECWP+1                                                                      
      LASTS=LRECWS+1                                                                      
      K10=0                                                                               
      K11=0                                                                               
      K12=0                                                                               
C-----                                                                                    
    2 CALL READUN(10,LITP,IFLAG,LASTP)                                                    
      IF(IFLAG) 3,4,999                                                                   
    4 K10=K10+1                                                                           
      CALL READUN(11,LITS,IFLAG,LASTS)                                                    
      IF(IFLAG) 5,6,999                                                                   
    6 K11=K11+1                                                                           
      INDEX=1                                                                             
C-----COPY PRIMARY FILE RECORD                                                            
      CALL COPYST(LITP,LRECP,LIT,INDEX)                                                   
C-----COPY SECONDARY FILE RECORD                                                          
      CALL COPYST(LITS,LRECS,LIT,INDEX)                                                   
      INDEX=LRECW*MACHC                                                                   
C-----BLANK POSITIONS LREC+1 TO INDEX                                                     
      CALL SETBLK(LIT,LREC,INDEX)                                                         
      INDEX=INDEX+1                                                                       
C-----COPY SEQUENCE NO FROM PRIMARY FILE RECORD                                           
      CALL COPYST(LITP(LASTP),MACHC,LIT,INDEX)                                            
      CALL WRITUN(12,LIT,LAST)                                                            
      K12=K12+1                                                                           
      GO TO 2                                                                             
C-----                                                                                    
    3 CALL EOFR(10,K10)                                                                   
      CALL READUN(11,LITS,IFLAG,LASTS)                                                    
      IF(IFLAG) 11,12,999                                                                 
   11 CALL EOFR(11,K11)                                                                   
      GO TO 20                                                                            
   12 WRITE(LP,13)                                                                        
   13 FORMAT(44H COUNTS ON 10 AND 11 DISAGREE -- CHECK MATCH)                             
      CALL JOBEND                                                                         
C-----                                                                                    
    5 CALL EOFR(11,K11)                                                                   
      CALL READUN(10,LITP,IFLAG,LASTP)                                                    
      IF(IFLAG) 14,12,999                                                                 
   14 CALL EOFR(10,K10)                                                                   
      GO TO 20                                                                            
C-----NORMAL TERMINATION                                                                  
   20 ENDFILE 12                                                                          
      CALL EOFW(12,K12)                                                                   
      REWIND 10                                                                           
      REWIND 11                                                                           
      REWIND 12                                                                           
      STOP                                                                                
  999 CALL LOGIC(5HMARRY)                                                                 
      END                                                                                 
