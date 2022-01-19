      PROGRAM POSTSRT(TAPE10=/1000,TAPE11=/1000,TAPE12=/1000                              
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C----                                                                                     
C---- WRITTEN R.E. MARCH, 1983.                                                           
C----                                                                                     
C---- RE-SYNTHESISES CHARM FILE AFTER SORT/MERGE UNDER NOS.                               
C---- USES HEADER RECORD RETAINED FROM PRESRT VIA TAPE12.                                 
C----                                                                                     
      DIMENSION LIT(100)                                                                  
      COMMON/MACH/MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                     
      CALL DEFINE                                                                         
      WRITE(LP,10)                                                                        
   10 FORMAT(12H0POSTSRT RUN)                                                             
      REWIND 10                                                                           
      REWIND 11                                                                           
      REWIND 12                                                                           
      CALL LNREAD(LREC,LRECW,12)                                                          
      CALL LNWRIT(LREC,LRECW,11)                                                          
      LAST=LRECW+1                                                                        
      K10=0                                                                               
      K11=0                                                                               
    2 CALL READUN(10,LIT,IFLAG,LAST)                                                      
      IF(IFLAG)3,4,999                                                                    
    4 K10=K10+1                                                                           
      CALL WRITUN(11,LIT,LAST)                                                            
      K11=K11+1                                                                           
      GO TO 2                                                                             
    3 CALL EOFR(10,K10)                                                                   
      ENDFILE 11                                                                          
      CALL EOFW(11,K11)                                                                   
      REWIND 10                                                                           
      REWIND 11                                                                           
      REWIND 12                                                                           
      STOP                                                                                
  999 CALL LOGIC(7HPOSTSRT)                                                               
      END                                                                                 
