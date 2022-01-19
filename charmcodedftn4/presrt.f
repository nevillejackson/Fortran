      PROGRAM PRESRT(TAPE10=/1000,TAPE11=/1000,TAPE12=/1000                               
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C----                                                                                     
C---- WRITTEN R.E. MARCH, 1983.                                                           
C----                                                                                     
C---- PROGRAM TO PREPARE A CHARM FILE FOR INPUT TO SORT/MERGE UNDER NOS.                  
C---- REQUIRED WHENEVER THE HEADER RECORD NEEDS TO BE REMOVED FROM THE FILE               
C---- BUT RETAINED FOR LATER USE.                                                         
C---- THE CHARM FILE IS ON TAPE10, THE UN-CHARMED FILE IS WRITTEN ON                      
C---- TAPE11 AND THE RECORD LENGTH HEADER RECORD IS SAVED ON TAPE12.                      
C----                                                                                     
      DIMENSION LIT(100)                                                                  
      COMMON/MACH/MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                     
      CALL DEFINE                                                                         
      WRITE(LP,10)                                                                        
   10 FORMAT(11H0PRESRT RUN)                                                              
      REWIND 10                                                                           
      REWIND 11                                                                           
      REWIND 12                                                                           
      CALL LNREAD(LREC,LRECW,10)                                                          
      CALL LNWRIT(LREC,LRECW,12)                                                          
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
  999 CALL LOGIC(6HPRESRT)                                                                
      END                                                                                 
