      SUBROUTINE DATAVC(FMT,X,LV)                                                         
      DIMENSION X(1),FMT(12)                                                              
      READ(1,FMT) (X(L1),L1=1,LV)                                                         
      RETURN                                                                              
      END                                                                                 
