      SUBROUTINE SPLIT(IWD,IEL)                                                           
C-----WRITTEN R.E. 1977                                                                   
C-----MSCHINE DEPENDENT ROUTINE - CYBER 76 VERSION                                        
      DIMENSION IEL(10)                                                                   
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      DECODE(MACHC,1,IWD) (IEL(I),I=1,MACHC)                                              
    1 FORMAT(10A1)                                                                        
      RETURN                                                                              
      ENTRY PACK                                                                          
      ENCODE(MACHC,1,IWD) (IEL(I),I=1,MACHC)                                              
      RETURN                                                                              
      END                                                                                 
