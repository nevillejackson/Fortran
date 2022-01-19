      FUNCTION INDTRG(I,N,KORD)                                                           
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON / TWO / ISSW(15,10)                                                          
C---- WRITTEN R.E. 1977                                                                   
C---- USED BY PROGRAM SHEET                                                               
C---- CHECKS ACTIVITY STATUS OF TRIGGER OPERATIVE TYPE I                                  
C---- RETURNS 0 IF INACTIVE, 1 IF ACTIVE                                                  
C---- ORDINAL POSITION OF ACTIVE OPERATIVE IS RETURNED VIA KORD                           
C---- N IS THE NUMBER OF OPERATIVES OF TYPE I                                             
      INDTRG=0                                                                            
      KORD=0                                                                              
      IF(N)50,50,20                                                                       
   20 DO 30 K=1,N                                                                         
      IF(ISSW(I,K)-1)30,40,40                                                             
   30 CONTINUE                                                                            
      GO TO 50                                                                            
   40 INDTRG=1                                                                            
      KORD=K                                                                              
   50 RETURN                                                                              
      END                                                                                 
