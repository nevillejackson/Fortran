      FUNCTION NOD(ITM,LDIR,N)                                                            
C-----WRITTEN N.J. 1977                                                                   
C-----CONVERT DIRECTIVE MNEMONICS TO NUMERIC OP CODES                                     
      DIMENSION LDIR(1)                                                                   
      NOD=0                                                                               
      DO 1 I=1,N                                                                          
      IF(ITM-LDIR(I)) 1,2,1                                                               
    1 CONTINUE                                                                            
      CALL LOGIC(3HNOD)                                                                   
    2 NOD=I                                                                               
      RETURN                                                                              
      END                                                                                 
