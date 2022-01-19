      PROGRAM SLICE(TAPE10=/1000,TAPE11=/1000,TAPE12=/1000                                
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C-----WRITTEN N.J. 1977                                                                   
C-----SPLIT A CHARM FILE (TAPE10) AFTER SPECIFIED SEQUENCE NO                             
C-----INTO TWO CHARM FILES ( TAPES 11&12)                                                 
      DIMENSION LIT(100)                                                                  
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      CALL DEFINE                                                                         
      WRITE(LP,10)                                                                        
   10 FORMAT(10H0SLICE RUN)                                                               
      REWIND 10                                                                           
      REWIND 11                                                                           
      REWIND 12                                                                           
      CALL LNREAD(LREC,LRECW,10)                                                          
      CALL LNWRIT(LREC,LRECW,11)                                                          
      CALL LNWRIT(LREC,LRECW,12)                                                          
C-----                                                                                    
      READ(LC,5) LINO                                                                     
    5 FORMAT(16I5)                                                                        
      WRITE(LP,6) LINO                                                                    
    6 FORMAT(23H SPLIT AFTER RECORD NO ,I8)                                               
C-----                                                                                    
      ISW=0                                                                               
      LAST=LRECW+1                                                                        
      K=0                                                                                 
      K2=0                                                                                
    2 CALL READUN(10,LIT,IFLAG,LAST)                                                      
      IF(IFLAG) 3,4,999                                                                   
    4 K=K+1                                                                               
      IF(INUM(LIT(LAST))-LINO) 7,8,9                                                      
    8 ISW=1                                                                               
      WRITE(LP,12)                                                                        
   12 FORMAT(18H SPLIT POINT FOUND)                                                       
      CALL KPUT(K)                                                                        
    7 CALL WRITUN(11,LIT,LAST)                                                            
      IF(ISW) 2,2,11                                                                      
   11 ENDFILE 11                                                                          
      REWIND 11                                                                           
      CALL EOFW(11,K)                                                                     
      GO TO 2                                                                             
    9 CALL WRITUN(12,LIT,LAST)                                                            
      K2=K2+1                                                                             
      GO TO 2                                                                             
    3 CALL EOFR(10,K)                                                                     
      ENDFILE 12                                                                          
      REWIND 12                                                                           
      CALL EOFW(12,K2)                                                                    
      REWIND 10                                                                           
      STOP                                                                                
  999 CALL LOGIC(5HSLICE)                                                                 
      END                                                                                 
