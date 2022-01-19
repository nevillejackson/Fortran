      SUBROUTINE LOGIC( HOLL )                                                            
C                                                                                         
C-----MACHINE DEPENDENT ROUTINE -- CYBER 76 VERSION                                       
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      INTEGER HOLL(2)                                                                     
C                                                                                         
C     LOGIC                              WRITTEN 1974 AUTHOR JN                           
C     -----                                                                               
C                                                                                         
C     CALL LOGIC( HOLL )                                                                  
C                                                                                         
C     USED BY CHARM ROUTINES WHEN THEY DISCOVER A LOGIC ERROR                             
C     PRINTS A MESSAGE AND ABORTS THE JOB                                                 
C                                                                                         
C     PARAMETER                                                                           
C                                                                                         
C     HOLL()   CHAR STRING                                                                
C                                                                                         
C                                                                                         
      WRITE(LP,1001) HOLL                                                                 
      CALL JOBEND                                                                         
C                                                                                         
      RETURN                                                                              
C                                                                                         
 1001 FORMAT(45H LOGIC ERROR OCCURRED - PROGRAM FAULTY - SEND,                            
     1 26H THIS LISTING TO ROD EVANS,/1H ,2A10)                                           
C                                                                                         
      END                                                                                 
