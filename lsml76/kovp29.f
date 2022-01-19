      FUNCTION KOVP29(KARD,M)                                                             
      DIMENSION KARD(1),KTAB(64),LTAB(64)                                                 
C-----CONVERTS CHAR POSITION M IN STRING KARD() FROM OVERPUNCHED DIGIT                    
C-----TO PLAIN DIGIT , AND RETURNS OVERPUNCHING VIA FUNCTION NAME.                        
C-----RETURNS ZERO IF NO OVERPUNCHING.                                                    
C-----029 PUNCH CODE ASSUMED                                                              
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
      DATA KTAB/0,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,0,0,0                  
     1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,-1,-1,0,1,-1,-1,0,0,0,1,0,1,-1,0,0,0,                  
     2 1,1,0,0,1,0,0,0,-1,-1/                                                             
      DATA LTAB/1,29,30,31,32,33,34,35,36,37,29,30,31,32,33,34,35,36,37,                  
     1 20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,45,46,61,41                  
     2,57,57,49,45,46,47,49,49,1,1,52,53,54,28,46,57,58,28,60,61,62,53,                   
     3 45/                                                                                
      L=IN(KARD,M)                                                                        
      KOVP29=KTAB(L+1)                                                                    
      N=LTAB(L+1)                                                                         
      CALL OUT(KARD,M,IN(KONST(N),MACHC))                                                 
      RETURN                                                                              
      END                                                                                 
