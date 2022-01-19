      SUBROUTINE CLRBIT( MASKK, NBIT )                                                    
C                                                                                         
      DIMENSION MASKK( 1 )                                                                
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
C                                                                                         
C     CLRBIT                            WRITTEN  8/8/75   AUTHOR  J.N.                    
C     ------                                                                              
C                                                                                         
C     CALL CLRBIT( MASK, NBIT )                                                           
C                                                                                         
C     SETS BIT NR 'NBIT' TO ZERO                                                          
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     MASK   WORD HOLDING BIT TO BE CLEARED                                               
C     NBIT   BIT NUMBER                                                                   
C                                                                                         
      IW=NBIT/(MACHB*MACHC)+1                                                             
      NB=MOD(NBIT,MACHB*MACHC)                                                            
      MASKK(IW)=AND(MASKK(IW),SHIFT(MASK(MACHB*MACHC-1),MACHB*MACHC-1-NB                  
     1 ))                                                                                 
      RETURN                                                                              
C                                                                                         
      END                                                                                 
