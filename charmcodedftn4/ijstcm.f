      FUNCTION IJSTCM(IST,JST,LEN)                                                        
C-----WRITTEN N.J. 1978                                                                   
C-----COMPARES TWO STRINGS BOTH STARTING AT ARRAY POSITION 1 AND                          
C-----BOTH OF LENGTH LEN CHARS                                                            
C-----RETURNS -1 IST.LT.JST                                                               
C-----         0 IST.EQ.JST                                                               
C-----        +1 IST.GT.JST                                                               
C-----                                                                                    
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      DIMENSION IST(1),JST(1)                                                             
C-----WHOLE WORDS                                                                         
      L=LWORDS(LEN)-1                                                                     
      IF(L) 999,4,5                                                                       
    5 DO 1 K=1,L                                                                          
      IF(IJWDCM(IST(K),JST(K))) 2,1,3                                                     
    1 CONTINUE                                                                            
    4 KF=L*MACHC+1                                                                        
      L=LEN-L*MACHC                                                                       
      KL=KF+L-1                                                                           
      IF(L) 999,6,7                                                                       
C-----PART WORD TRAILING                                                                  
    7 DO 8 K=KF,KL                                                                        
      IF(IJCHCM(IN(IST,K),IN(JST,K))) 2,8,3                                               
    8 CONTINUE                                                                            
    6 IJSTCM=0                                                                            
      GO TO 9                                                                             
    2 IJSTCM=-1                                                                           
      GO TO 9                                                                             
    3 IJSTCM=+1                                                                           
    9 RETURN                                                                              
  999 CALL LOGIC(6HIJSTCM)                                                                
      RETURN                                                                              
      END                                                                                 
