*TEXT                                                                                     
      FUNCTION DIVF(ENUM,DEN)                                           DVF20001          
      IF(DEN) 1,2,1                                                     DVF20002          
    2 DIVF=0.0                                                          DVF20003          
      RETURN                                                            DVF20004          
    1 DIVF=ENUM/DEN                                                     DVF20005          
      RETURN                                                            DVF20006          
      END                                                               DVF20007          
*ENDTEXT                                                                                  
