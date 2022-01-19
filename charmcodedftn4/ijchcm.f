      FUNCTION IJCHCM(ICH,JCH)                                                            
C-----WRITTEN N.J. 1978                                                                   
C-----MACHINE DEPENDENT ROUTINE -- CYBER 76 VERSION                                       
C-----WILL WORK ON ANY MACHINE WITH MACHC.GT.1                                            
C-----COMPARES TWO CHARACTERS STORED RIGHT JUSTIFIED ZERO FILLED IN                       
C----- ICH AND JCH                                                                        
C-----RETURNS -1 ICH.LT.JCH                                                               
C-----         0 ICH.EQ.JCH                                                               
C-----        +1 ICH.GT.JCH                                                               
C-----USES COLLATING SEQUENCE DEFINED BY THE MACHINE CODES FOR EACH CHARACTER             
C-----TO CHANGE COLL SEQ CHANGE THE CODES BEFORE CALLING                                  
C-----                                                                                    
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      IF(IN(ICH,MACHC)-IN(JCH,MACHC)) 1,2,3                                               
    1 IJCHCM=-1                                                                           
      GO TO 4                                                                             
    2 IJCHCM=0                                                                            
      GO TO 4                                                                             
    3 IJCHCM=+1                                                                           
    4 RETURN                                                                              
      END                                                                                 
