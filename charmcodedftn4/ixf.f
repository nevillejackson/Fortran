      FUNCTION IXF(A,NDEC)                                                                
C-----WRITTEN N.J. 1977                                                                   
C-----CONVERTS A FLOATING PT WORD TO AN INTEGER RETAINING NDEC DIGITS                     
C-----AFTER DECIMAL                                                                       
      F=1.0                                                                               
      IF(A.LT.0.0)F=-F                                                                    
      IXF=A*(10.**NDEC)+0.5*F                                                             
      RETURN                                                                              
      END                                                                                 
