      FUNCTION FXIHOL(I,NDEC)                                                             
C-----WRITTEN N.J. 1977                                                                   
C-----MACHINE DEPENDENT ROUTINE -- CYBER 76 VERSION                                       
C-----CONVERTS INTEGER HOLLERITH WORD TO FLOATING WORD                                    
      DIMENSION IFM(8)                                                                    
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
    1 FORMAT(2H(F,I2,1H.,I2,1H))                                                          
      ENCODE(8,1,IFM) MACHC,NDEC                                                          
      DECODE(MACHC,IFM,I) FXIHOL                                                          
      RETURN                                                                              
      END                                                                                 
