      SUBROUTINE DATA(M,D)                                              
      DIMENSION D(1)                                                    
      NCR=5                                                             
      NLP=6                                                             
    2 FORMAT(1H ,12F10.4)                                               
      READ(NCR,*) (D(I),I=1,M)                                          
      WRITE(NLP,2) (D(I),I=1,M)                                         
      RETURN                                                            
      END                                                               
