      PROGRAM JOIN5(TAPE10=/1000,TAPE11=/1000,TAPE12=/1000                                
     + ,TAPE13=/1000,TAPE14=/1000,TAPE15=/1000                                            
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C-----WRITTEN N.J. 1977                                                                   
C-----JOIN 5 CHARM FILES (TAPES 10-14) INTO ONE (TAPE15)                                  
C-----RECORD LENGTHS MUST BE SAME ON ALL JOINED FILES                                     
      DIMENSION LIT(100)                                                                  
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      CALL DEFINE                                                                         
      WRITE(LP,10)                                                                        
   10 FORMAT(10H0JOIN5 RUN)                                                               
      REWIND 10                                                                           
      REWIND 11                                                                           
      REWIND 12                                                                           
      REWIND 13                                                                           
      REWIND 14                                                                           
      REWIND 15                                                                           
      LMAX=1                                                                              
      LMIN=1000                                                                           
      CALL LNREAD(LREC,LRECW,10)                                                          
      IF(LREC.GT.LMAX)LMAX=LREC                                                           
      IF(LREC.LT.LMIN)LMIN=LREC                                                           
      CALL LNREAD(L11,L11W,11)                                                            
      IF(L11.GT.LMAX)LMAX=L11                                                             
      IF(L11.LT.LMIN)LMIN=L11                                                             
      CALL LNREAD(L12,L12W,12)                                                            
      IF(L12.GT.LMAX)LMAX=L12                                                             
      IF(L12.LT.LMIN)LMIN=L12                                                             
      CALL LNREAD(L13,L13W,13)                                                            
      IF(L13.GT.LMAX)LMAX=L13                                                             
      IF(L13.LT.LMIN)LMIN=L13                                                             
      CALL LNREAD(L14,L14W,14)                                                            
      IF(L14.GT.LMAX)LMAX=L14                                                             
      IF(L14.LT.LMIN)LMIN=L14                                                             
      IF(LMAX.NE.LMIN) WRITE(LP,11)                                                       
   11 FORMAT(90H0*** WARNING ***  RECORD LENGTHS DIFFER  - SHORT RECORDS                  
     . BLANK FILLED TO LENGTH OF LONGEST)                                                 
      CALL LNWRIT(LMAX,LMAXW,15)                                                          
      LAST=LRECW+1                                                                        
      LOUT=LMAXW+1                                                                        
      L11WS=L11W+1                                                                        
      L12WS=L12W+1                                                                        
      L13WS=L13W+1                                                                        
      L14WS=L14W+1                                                                        
      LONG=LMAXW*MACHC                                                                    
C-----INITIALIZE                                                                          
      K10=0                                                                               
      K11=0                                                                               
      K12=0                                                                               
      K13=0                                                                               
      K14=0                                                                               
      K15=0                                                                               
      LYN=1                                                                               
C-----READ                                                                                
    1 CALL READUN(10,LIT,IFLAG,LAST)                                                      
      IF(IFLAG) 31,41,999                                                                 
   41 K10=K10+1                                                                           
      IF(LREC-LMAX)20,50,999                                                              
   20 LIT(LOUT)=LIT(LAST)                                                                 
      CALL SETBLK(LIT,LREC,LONG)                                                          
      GO TO 50                                                                            
    2 CALL READUN(11,LIT,IFLAG,L11WS)                                                     
      IF(IFLAG) 32,42,999                                                                 
   42 K11=K11+1                                                                           
      IF(L11-LMAX)21,50,999                                                               
   21 LIT(LOUT)=LIT(L11WS)                                                                
      CALL SETBLK(LIT,L11,LONG)                                                           
      GO TO 50                                                                            
    3 CALL READUN(12,LIT,IFLAG,L12WS)                                                     
      IF(IFLAG) 33,43,999                                                                 
   43 K12=K12+1                                                                           
      IF(L12-LMAX) 22,50,999                                                              
   22 LIT(LOUT)=LIT(L12WS)                                                                
      CALL SETBLK(LIT,L12,LONG)                                                           
      GO TO 50                                                                            
    4 CALL READUN(13,LIT,IFLAG,L13WS)                                                     
      IF(IFLAG) 34,44,999                                                                 
   44 K13=K13+1                                                                           
      IF(L13-LMAX)23,50,999                                                               
   23 LIT(LOUT)=LIT(L13WS)                                                                
      CALL SETBLK(LIT,L13,LONG)                                                           
      GO TO 50                                                                            
    5 CALL READUN(14,LIT,IFLAG,L14WS)                                                     
      IF(IFLAG) 35,45,999                                                                 
   45 K14=K14+1                                                                           
      IF(L14-LMAX)24,50,999                                                               
   24 LIT(LOUT)=LIT(L14WS)                                                                
      CALL SETBLK(LIT,L14,LONG)                                                           
C-----WRITE                                                                               
   50 CALL WRITUN(15,LIT,LOUT)                                                            
      K15=K15+1                                                                           
      GO TO (1,2,3,4,5),LYN                                                               
C-----ENDS                                                                                
   31 CALL EOFR(10,K10)                                                                   
      REWIND 10                                                                           
      LYN=LYN+1                                                                           
      GO TO 2                                                                             
   32 CALL EOFR(11,K11)                                                                   
      REWIND 11                                                                           
      LYN=LYN+1                                                                           
      GO TO 3                                                                             
   33 CALL EOFR(12,K12)                                                                   
      REWIND 12                                                                           
      LYN=LYN+1                                                                           
      GO TO 4                                                                             
   34 CALL EOFR(13,K13)                                                                   
      REWIND 13                                                                           
      LYN=LYN+1                                                                           
      GO TO 5                                                                             
   35 CALL EOFR(14,K14)                                                                   
      REWIND 14                                                                           
      ENDFILE 15                                                                          
      REWIND 15                                                                           
      CALL EOFW(15,K15)                                                                   
      STOP                                                                                
  999 CALL LOGIC(5HJOIN5)                                                                 
      END                                                                                 
