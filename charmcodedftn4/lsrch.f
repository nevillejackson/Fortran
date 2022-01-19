      FUNCTION LSRCH(N,KA,KARG,LIB,LAB,NI,ISIZE,M)                                        
C-----WRITTEN N.J. 1966 FOR IBM 1620                                                      
C-----MUCH MODIFIED                                                                       
C-----FINDS POSITION TO INSERT ITEM IN SORTED LIST BY BINARY SEARCH METHOD                
C-----IF MORE THAN ONE ITEM EQ KARG , LY IS POSITION OF LAST ITEM                         
C-----                                                                                    
C-----CHARACTER STRING VERSION                                                            
C-----LIB,LAB ARE BEG AND ENDING CHAR POSNS                                               
C-----FOR KEY SORTED ON -- ONLY ONE SORT KEY ALLOWED                                      
C-----                                                                                    
      DIMENSION KARG(NI),KA(ISIZE,NI)                                                     
C-----                                                                                    
C-----M%0 -- KARG ALREADY IN LIST,LY RETURNED AS POSITION OF ITEM                         
C-----WHICH EQUALS KARG                                                                   
C-----                                                                                    
C-----M%1 KARG NOT IN LIST,LY RETURNED AS POSITION TO INSERT KARG                         
C-----                                                                                    
      DO 22 L=LIB,LAB                                                                     
      IF(IN(KARG,L)-IN(KA,L)) 1,22,3                                                      
   22 CONTINUE                                                                            
   19 LY=1                                                                                
      GO TO 15                                                                            
    1 LY=1                                                                                
C-----NO DUPLICATE                                                                        
    2 M=1                                                                                 
      GO TO 16                                                                            
    3 DO 23 L=LIB,LAB                                                                     
      IF(IN(KARG,L)-IN(KA,(N-1)*ISIZE+L)) 5,23,4                                          
   23 CONTINUE                                                                            
   20 LY=N                                                                                
      GO TO 15                                                                            
    4 LY=N+1                                                                              
      GO TO 2                                                                             
    5 LX=1                                                                                
    6 IF(LX-N) 7,8,8                                                                      
    7 LX=2*LX                                                                             
      GO TO 6                                                                             
    8 LY=LX                                                                               
    9 IF(LY-N) 10,10,11                                                                   
   10 DO 24 L=LIB,LAB                                                                     
      IF(IN(KARG,L)-IN(KA,(LY-1)*ISIZE+L)) 11,24,12                                       
   24 CONTINUE                                                                            
C-----DUPLICATES                                                                          
   15 M=0                                                                                 
C-----CHECK THAT LY IS POSITION OF LAST DUPLICATE RECORD                                  
   32 LY=LY+1                                                                             
      DO 31 L=LIB,LAB                                                                     
      IF(IN(KARG,L)-IN(KA,(LY-1)*ISIZE+L)) 33,31,33                                       
   31 CONTINUE                                                                            
      GO TO 32                                                                            
   33 LY=LY-1                                                                             
   16 LSRCH=LY                                                                            
      RETURN                                                                              
   11 LY=LY-LX/2                                                                          
      GO TO 13                                                                            
   12 LY=LY+LX/2                                                                          
   13 IF(LX-1) 17,17,14                                                                   
   14 LX=LX/2                                                                             
      GO TO 9                                                                             
   17 DO 25 L=LIB,LAB                                                                     
      IF(IN(KARG,L)-IN(KA,(LY-1)*ISIZE+L)) 2,25,18                                        
   25 CONTINUE                                                                            
      GO TO 2                                                                             
   18 LY=LY+1                                                                             
      GO TO 2                                                                             
      END                                                                                 
