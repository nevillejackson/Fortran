      PROGRAM JOIN(TAPE10=/1000,TAPE11=/1000,TAPE12=/1000                                 
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C-----WRITTEN N.J. 1977                                                                   
C-----JOIN 2 CHAREM FILES (TAPES 10 & 11) INTO ONE CHARM FILE (TAPE12)                    
C-----NO SORTING OR RESEQUENCING                                                          
      DIMENSION LIT(100)                                                                  
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      CALL DEFINE                                                                         
      WRITE(LP,10)                                                                        
   10 FORMAT(9H0JOIN RUN)                                                                 
      REWIND 10                                                                           
      REWIND 11                                                                           
      REWIND 12                                                                           
      LMAX=1                                                                              
      LMIN=10000                                                                          
      CALL LNREAD(LREC,LRECW,10)                                                          
      IF(LREC.GT.LMAX)LMAX=LREC                                                           
      IF(LREC.LT.LMIN)LMIN=LREC                                                           
      CALL LNREAD(MREC,MRECW,11)                                                          
      IF(MREC.GT.LMAX)LMAX=MREC                                                           
      IF(MREC.LT.LMIN)LMIN=MREC                                                           
      IF(LMAX.NE.LMIN) WRITE(LP,11)                                                       
   11 FORMAT(90H0*** WARNING ***  RECORD LENGTHS DIFFER  - SHORT RECORDS                  
     . BLANK FILLED TO LENGTH OF LONGEST)                                                 
      CALL LNWRIT(LMAX,LMAXW,12)                                                          
C-----                                                                                    
      LAST=LRECW+1                                                                        
      MAST=MRECW+1                                                                        
      LOUT=LMAXW+1                                                                        
      LONG=LMAXW*MACHC                                                                    
      K=0                                                                                 
      K3=0                                                                                
    2 CALL READUN(10,LIT,IFLAG,LAST)                                                      
      IF(IFLAG) 3,4,999                                                                   
    4 K=K+1                                                                               
      IF(LREC-LMAX)20,21,999                                                              
   20 LIT(LOUT)=LIT(LAST)                                                                 
      CALL SETBLK(LIT,LREC,LONG)                                                          
   21 CALL WRITUN(12,LIT,LOUT)                                                            
      K3=K3+1                                                                             
      GO TO 2                                                                             
    3 CALLEOFR(10,K)                                                                      
      K2=0                                                                                
   33 CALL READUN(11,LIT,IFLAG,MAST)                                                      
      IF(IFLAG) 13,14,999                                                                 
   14 K2=K2+1                                                                             
      IF(MREC-LMAX)22,23,999                                                              
   22 LIT(LOUT)=LIT(MAST)                                                                 
      CALL SETBLK(LIT,MREC,LONG)                                                          
   23 CALL WRITUN(12,LIT,LOUT)                                                            
      K3=K3+1                                                                             
      GO TO 33                                                                            
   13 CALL EOFR(11,K2)                                                                    
      ENDFILE 12                                                                          
      REWIND 12                                                                           
      CALL EOFW(12,K3)                                                                    
      REWIND 11                                                                           
      REWIND 10                                                                           
      STOP                                                                                
  999 CALL LOGIC(4HJOIN)                                                                  
      END                                                                                 
