      SUBROUTINE RCARD(IN,K,L,IC,KD,IFLAG)                                                
      DIMENSION IC(1),IBUF(1000)                                                          
      COMMON /CMBLK5/ LREC,LRECW,LAST                                                     
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      IF(IN-5) 5,6,5                                                                      
    6 CALL READCD(IN,IBUF,IFLAG,LRECW)                                                    
      IF(IFLAG) 2,1,2                                                                     
    5 CALL READUN(IN,IBUF,IFLAG,LAST)                                                     
      IF(IFLAG) 2,1,2                                                                     
    1 CALL COPYC(IBUF,1,IC,K,LREC)                                                        
    2 RETURN                                                                              
      ENTRY FCARD                                                                         
C-----FIRST CARD ENTRY                                                                    
      IF(IN-5) 3,4,3                                                                      
    4 LREC=MACHCC                                                                         
      LRECW=MACHCD                                                                        
      LAST=MACHCD                                                                         
      IFLAG=0                                                                             
      RETURN                                                                              
    3 CALL LNREAD(LREC,LRECW,IN)                                                          
      LAST=LRECW+1                                                                        
      IFLAG=0                                                                             
      RETURN                                                                              
      ENTRY WCARD                                                                         
C-----WRITE A CARD                                                                        
      IF(IN-5) 7,8,7                                                                      
    8 CALL WRITLN(6,IC,0,LRECW)                                                           
      GO TO 9                                                                             
    7 CALL PRNTSI(6,IBUF(LAST),IBUF,LRECW,0)                                              
    9 RETURN                                                                              
      END                                                                                 
