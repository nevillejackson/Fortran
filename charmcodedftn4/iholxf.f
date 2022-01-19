      FUNCTION IHOLXF(F,NDEC)                                                             
C-----WRITTEN N.J. 1977                                                                   
C-----MACHINE DEPENDENT ROUTINE -- CYBER 76 VERSION                                       
C-----CONVERTS FROM FLOATING WORD TO INTEGER HOLLERITH WORD WITH POINT                    
C-----MAX FIELD WIDTH IS MACHC CODED COLS INCL PT.                                        
      DIMENSION IFM(8)                                                                    
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
    1 FORMAT(2H(F,I2,1H.,I2,1H))                                                          
      ENCODE(8,1,IFM) MACHC,NDEC                                                          
      ENCODE(MACHC,IFM,IHOLXF)F                                                           
      RETURN                                                                              
      END                                                                                 
