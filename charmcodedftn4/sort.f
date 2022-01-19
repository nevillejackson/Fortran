      SUBROUTINE SORT(LIST,ISIZE,NI,LIB,LAB,KARD,N)                                       
C-----WRITTEN N.J. 1966 FOR IBM 1620                                                      
C-----MUCH MODIFIED                                                                       
C-----INSERTS KARD INTO LIST AT POS OBTAINED FROM LSRCH                                   
C-----                                                                                    
C-----CHARACTER STRING VERSION                                                            
C-----LIB,LAB ARE BEG AND ENDING CHAR POSNS                                               
C-----FOR KEY SORTED ON -- ONLY ONE SORT KEY ALLOWED                                      
C-----                                                                                    
C-----   N= NO OF ITEMS ALREADY IN LIST , SET TO N+1 ON RETURN                            
C-----  NI=RECORD LENGTH                                                                  
      DIMENSION KARD(NI),LIST(ISIZE,NI)                                                   
      LY=1                                                                                
      IF(N) 162,162,74                                                                    
  162 N=1                                                                                 
      GO TO 62                                                                            
   74 LY=LSRCH(N,LIST,KARD,LIB,LAB,NI,ISIZE,M)                                            
C-----FORCE DUPLICATES TO BE INSERTED AFTER POSITION OF LAST DUPLICATE                    
      IF(M) 50,51,50                                                                      
   51 LY=LY+1                                                                             
   50 N=N+1                                                                               
C-----NN IS VACANT ROW                                                                    
      NN=N-1                                                                              
      IF(LY-N) 64,64,62                                                                   
   64 NP1=NN+1                                                                            
   61 DO 60 J=1,NI                                                                        
   60 LIST(NP1,J)=LIST(NN,J)                                                              
      NP1=NP1-1                                                                           
C-----NN IS ROW BEFORE VACANT ROW                                                         
      NN=NN-1                                                                             
C-----IF VACANT ROW IS LY , FILL IT FROM KARD , IF NOT KEEP SHIFTING DIWN                 
      IF(LY-NN) 61,61,62                                                                  
C-----LY IS VACANT ROW HERE                                                               
   62 DO 63 J=1,NI                                                                        
   63 LIST(LY,J)=KARD(J)                                                                  
      RETURN                                                                              
      END                                                                                 
