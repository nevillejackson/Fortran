      SUBROUTINE ATOIA(IC,K,L)                                                            
      DIMENSION IC(1),ID(10)                                                              
      DATA(ID(I),I=1,10)/1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9/                         
      DO 1 J=K,L                                                                          
      DO 2 I=1,10                                                                         
      IF(IC(J)-ID(I)) 2,3,2                                                               
    3 IC(J)=I-1                                                                           
      GO TO 1                                                                             
    2 CONTINUE                                                                            
    1 CONTINUE                                                                            
      RETURN                                                                              
      END                                                                                 
