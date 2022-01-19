      SUBROUTINE RASTER(IFM,LP)                                                           
      DIMENSION IFM(8)                                                                    
C-----WRITTEN N.J. 1982                                                                   
C-----PRINTS RASTER FOR PROGRAM LIST                                                      
      WRITE(LP,1)                                                                         
    1 FORMAT(1H0,9H  SEQ.NO.,5X,9X,1H1,9X,1H2,9X,1H3,9X,1H4,9X,1H5                        
     1 ,9X,1H6,9X,1H7,9X,1H8,9X,1H9,9X,1H0,9X,1H1,9X,1H2/                                 
     2 1H ,14X,12(10H1234567890)/)                                                        
      RETURN                                                                              
      END                                                                                 
