      FUNCTION IN( M,I )                                                                  
C                                                                                         
      DIMENSION M( 500 )                                                                  
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
C                                                                                         
C     IN                                WRITTEN1973 AUTHOR J.N.                           
C     --                                                                                  
C                                                                                         
C     K = IN( M, I )                                                                      
C                                                                                         
C     ROUTINE TO RETURN THE I"TH CHARACTER FROM ARRAY "M"                                 
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     M  CHARACTER ARRAY                                                                  
C     I  POINTS TO REQUIRED CHARACTER                                                     
C                                                                                         
      IW=(I-1)/MACHC+1                                                                    
      IC=-(MACHC-1-MOD(I-1,MACHC))*MACHB                                                  
C                                                                                         
      IN=AND(COMPL(MASK(MACHB*(MACHC-1))),SHIFT(M(IW),IC))                                
C                                                                                         
      RETURN                                                                              
      END                                                                                 
