      FUNCTION FXI(I,NDEC)                                                                
C-----WRITTEN N.J. 1977                                                                   
C-----CONVERTS AN INTEGER WORD TO FLOATING PT INSERTING DECOMAL POINT                     
C----- BEFORE NDEC DIGITS                                                                 
      X=I                                                                                 
      FXI=X/(10**NDEC)                                                                    
      RETURN                                                                              
      END                                                                                 
