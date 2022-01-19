      PROGRAM SEQNC(TAPE10=/1000,TAPE11=/1000                                             
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C-----WRITTEN N.J. 1977                                                                   
C-----UNFORMATTED TO UNFORMATTED COPY . SEQUENCING REORDERED                              
      DIMENSION LIT(100)                                                                  
      COMMON/MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                    
      CALL DEFINE                                                                         
      WRITE(LP,10)                                                                        
   10 FORMAT(13H0SEQUENCE RUN)                                                            
      REWIND 10                                                                           
      REWIND 11                                                                           
      CALL LNREAD(LREC,LRECW,10)                                                          
      CALL LNWRIT(LREC,LRECW,11)                                                          
C-----                                                                                    
      LAST=LRECW+1                                                                        
      K=0                                                                                 
    2 CALL READUN(10,LIT,IFLAG,LAST)                                                      
      IF(IFLAG) 3,4,999                                                                   
    4 K=K+1                                                                               
      LIT(LAST)=IHOL(K)                                                                   
      CALL WRITUN(11,LIT,LAST)                                                            
      GO TO 2                                                                             
    3 CALL EOFR(10,K)                                                                     
      ENDFILE 11                                                                          
      REWIND 11                                                                           
      CALL EOFW(11,K)                                                                     
      REWIND 10                                                                           
      STOP                                                                                
  999 CALL LOGIC(5HSEQNC)                                                                 
      END                                                                                 
