*TEXT                                                                                     
      FUNCTION CORF(COV,VA,VB)                                          CRF20001          
      ABLE=VA*VB                                                        CRF20002          
      IF(ABLE) 1,1,2                                                    CRF20003          
    1 CORF=0.0                                                          CRF20004          
      RETURN                                                            CRF20005          
    2 CORF=COV/SQRT(ABLE)                                               CRF20006          
      RETURN                                                            CRF20007          
      END                                                               CRF20008          
*ENDTEXT                                                                                  
