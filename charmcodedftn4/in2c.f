      FUNCTION IN2C( ARRAY, I )                                                           
C                                                                                         
      DIMENSION ARRAY( 1 )                                                                
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      K = 0                                                                               
      CALL OUT( K, MACHC-1, IN( ARRAY,I ) )                                               
      CALL OUT( K, MACHC  , IN( ARRAY,I+1 ) )                                             
      IN2C = K                                                                            
      RETURN                                                                              
C                                                                                         
      END                                                                                 
