      SUBROUTINE LOC(I,J,IR,N,M,MS)                                     
      IX=I                                                              
      JX=J                                                              
      IRX=MS+1                                                          
      GO TO (10,20,30,40),IRX                                           
   10 IRX=N*(JX-1)+IX                                                   
      GO TO 36                                                          
   20 IF(IX-JX) 22,24,24                                                
   22 IRX=IX+(JX*JX-JX)/2                                               
      GO TO 36                                                          
   24 IRX=JX+(IX*IX-IX)/2                                               
      GO TO 36                                                          
   30 IRX=0                                                             
      IF(IX-JX) 36,32,36                                                
   32 IRX=IX                                                            
      GO TO 36                                                          
   40 IF(IX-JX) 42,46,44                                                
   42 IRX=1+IX+JX*(JX-3)/2                                              
      GO TO 36                                                          
   44 IRX=1+JX+IX*(IX-3)/2                                              
      GO TO 36                                                          
   46 IRX=0                                                             
   36 IR=IRX                                                            
      RETURN                                                            
      END                                                               
