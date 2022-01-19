      SUBROUTINE SETBIT( MASK,NBIT )                                                      
C                                                                                         
      DIMENSION MASK( 1 )                                                                 
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
C                                                                                         
C     SETBIT                            WRITTEN 1974 AUTHOR J.N.                          
C     ------                                                                              
C                                                                                         
C     CALL SETBIT( MASK, NBIT )                                                           
C                                                                                         
C     ROUTINE TO SET BIT NO 'NBIT' IN VARIABLE 'MASK' TO A ONE                            
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     MASK    VARIABLE CONTAINING BIT MASK                                                
C     NBIT    NUMBER OF BIT IN 'MASK' TO BE SET                                           
C                                                                                         
      NB=MOD(NBIT,MACHB*MACHC)                                                            
      IW=NBIT/(MACHB*MACHC)+1                                                             
      MASK(IW)=OR(MASK(IW),SHIFT(1,MACHB*MACHC-1-NB))                                     
      RETURN                                                                              
C                                                                                         
      END                                                                                 
