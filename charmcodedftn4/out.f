      SUBROUTINE OUT( M, I, ICH )                                                         
C                                                                                         
      DIMENSION M( 500 )                                                                  
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
C                                                                                         
C     OUT                               WRITTEN 1973 AUTHOR J.N.                          
C     ---                                                                                 
C                                                                                         
C     CALL OUT( M, I, ICH )                                                               
C                                                                                         
C     ROUTINE TO INSERT THE CHARACTER "ICH" IN THE I"TH CHARACTER                         
C     POSITION IN ARRAY "M"                                                               
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     M    CHARACTER ARRAY                                                                
C     I    POINTS TO CHARACTER IN 'M'                                                     
C     ICH  CHARACTER RIGHT JUSTIFIED                                                      
C                                                                                         
C                                                                                         
      IW=(I-1)/MACHC+1                                                                    
      IC=(MACHC-1-MOD(I-1,MACHC))*MACHB                                                   
C                                                                                         
      M(IW)=OR(AND(M(IW),SHIFT(MASK(MACHB*(MACHC-1)),IC)),SHIFT(ICH,IC))                  
C                                                                                         
      RETURN                                                                              
C                                                                                         
      END                                                                                 
