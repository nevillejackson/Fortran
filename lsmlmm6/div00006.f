*TEXT                                                                                     
      REAL FUNCTION DIV*8(X)                                            DK020010          
      REAL*8 X                                                          DK020020          
      DIV=X                                                             DK020030          
      IF (X.NE.0) RETURN                                                DK020040          
      DIV=1.0                                                           DK020050          
      RETURN                                                            DK020060          
      END                                                               DK020070          
*ENDTEXT                                                                                  
