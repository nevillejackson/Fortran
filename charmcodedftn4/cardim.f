      PROGRAM CARDIM(TAPE10=/1000,TAPE11=/1000                                            
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C-----WRITTEN N.J. 1977                                                                   
C-----CONVERTS CHARM FILE (TAPE10) TO CARDIMAGE FILE (TAPE11)                             
C-----SEQUENCE NUMBERS NOT WRITTEN ON TAPE11                                              
      DIMENSION LIT(100)                                                                  
      DIMENSION IFM(8)                                                                    
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
      CALL DEFINE                                                                         
      WRITE(LP,10)                                                                        
   10 FORMAT(11H0CARDIM RUN)                                                              
      REWIND 10                                                                           
      REWIND 11                                                                           
      CALL LNREAD(LREC,LRECW,10)                                                          
      CALL FMGET(IFM)                                                                     
C-----                                                                                    
      LAST=LRECW+1                                                                        
      K=0                                                                                 
    2 CALL READUN(10,LIT,IFLAG,LAST)                                                      
      IF(IFLAG) 3,4,999                                                                   
    4 K=K+1                                                                               
      WRITE(11,IFM)(LIT(I),I=1,LRECW)                                                     
      GO TO 2                                                                             
C-----                                                                                    
    3 CALL EOFR(10,K)                                                                     
      ENDFILE 11                                                                          
      CALL EOFW(11,K)                                                                     
      REWIND 10                                                                           
      REWIND 11                                                                           
      STOP                                                                                
  999 CALL LOGIC(6HCARDIM)                                                                
      END                                                                                 
