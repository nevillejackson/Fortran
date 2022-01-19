      SUBROUTINE FMGET(IFM)                                                               
C-----WRITTEN N.J. 1977                                                                   
C-----READS VARIABLE FORMAT CARD                                                          
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      DIMENSION IFM(1)                                                                    
      CALL READCD(LC,IFM,IFLAG,MACHCD)                                                    
      IF(IFLAG) 2,1,2                                                                     
    2 CALL LOGIC(5HFMGET)                                                                 
    1 CALL WRITLN(LP,IFM,0,MACHCD)                                                        
      RETURN                                                                              
      END                                                                                 
