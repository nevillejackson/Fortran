      SUBROUTINE SHELL( X,N,KEY )                                                         
      DIMENSION X(1),KEY(1)                                                               
      INTEGER X,TEMP                                                                      
C                                                                                         
C     SHESORT                           WRITTEN 1970 AUTHOR JABP                          
C     -------                           MODIFIED 1973                                     
C                                                                                         
C     CALL SHELL( X, N, KEY )                                                             
C                                                                                         
C     SHELL, MODIFIED FRANK AND LAZARUS, CACM 3,20 (1960)                                 
C     TO MAKE KEY TO ORIGINAL ORDER, USE NEGATIVE VALUE OF N                              
C     TO SORT INTEGERS, USE    INTEGER X, TEMP                                            
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     X    (INTEGER) LIST OF NUMBERS                                                      
C     N    LENGTH OF 'N'X'                                                                
C     KEY  KEY LIST                                                                       
C                                                                                         
      NO=N                                                                                
      MO=NO                                                                               
      ASSIGN 260 TO KEEPER                                                                
      IF(.NOT.(NO.LT.0)) GOTO 90                                                          
      NO=-NO                                                                              
      MO=NO                                                                               
      ASSIGN 230 TO KEEPER                                                                
      I=1                                                                                 
71    IF(I.GT.NO) GOTO 79                                                                 
      KEY(I)=I                                                                            
      I=I+1                                                                               
      GOTO 71                                                                             
79    CONTINUE                                                                            
90    IF(.NOT.(MO.LE.15)) GOTO 130                                                        
      IF(.NOT.(MO.GT.1)) GOTO 300                                                         
      MO=2*(MO/4)+1                                                                       
      GOTO 140                                                                            
130   MO=2*(MO/8)+1                                                                       
140   KO=NO-MO                                                                            
      JO=1                                                                                
160   I=JO                                                                                
170   IMO=I+MO                                                                            
      IF(.NOT.(X(I).GT.X(IMO))) GOTO 280                                                  
      TEMP=X(I)                                                                           
      X(I)=X(IMO)                                                                         
      X(IMO)=TEMP                                                                         
      GOTO KEEPER  , ( 260,230 )                                                          
230   KEMP=KEY(I)                                                                         
      KEY(I)=KEY(IMO)                                                                     
      KEY(IMO)=KEMP                                                                       
260   I=I-MO                                                                              
      IF(.NOT.(I.LT.1)) GOTO 170                                                          
280   JO=JO+1                                                                             
      IF(JO.GT.KO) GOTO 90                                                                
      GOTO 160                                                                            
300   RETURN                                                                              
      END                                                                                 
