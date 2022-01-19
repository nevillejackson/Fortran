      FUNCTION ISBIT( MASK, IBIT )                                                        
      DIMENSION MASK( 1 )                                                                 
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
C                                                                                         
C     ISBIT                             WRITTEN 1974 AUTHOR J.N.                          
C     -----                                                                               
C                                                                                         
C     K = ISBIT( MASK, IBIT )                                                             
C                                                                                         
C     ROUTINE TO RETURN A ONE IF BIT NUMBER IBIT IN MASK IS SET                           
C     ELSE RETURNS ZERO                                                                   
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     MASK    VARIABLE CONTAINING BIT MASK                                                
C     IBIT    NUMBER OF BIT OT BE INSERTED                                                
C                                                                                         
      IW=IBIT/(MACHB*MACHC)+1                                                             
      NB=MOD(IBIT,MACHB*MACHC)                                                            
      ISBIT=AND(1,SHIFT(MASK(IW),-(MACHB*MACHC-1-NB)))                                    
      RETURN                                                                              
C                                                                                         
      END                                                                                 
