      FUNCTION BART(DF,VAR,NV,CON)                                                        
      DIMENSION DF(1),VAR(1)                                                              
      SF=0.0 $ SFS=0.0 $ SFL=0.0 $ SRF=0.0                                                
      DO 10 I=1,NV                                                                        
      SL=ALOG10(VAR(I))                                                                   
      SF=SF+DF(I)                                                                         
      SRF=SRF+1.0/DF(I)                                                                   
      SFS=SFS+DF(I)*VAR(I)                                                                
      SFL=SFL+DF(I)*SL                                                                    
   10 CONTINUE                                                                            
      SBAR=SFS/SF                                                                         
      XM=(SF*ALOG10(SBAR)-SFL)*CON                                                        
      C=(SRF-1.0/SF)/(3.0*(NV-1))+1.0                                                     
      BART=XM/C                                                                           
      RETURN                                                                              
      END                                                                                 
