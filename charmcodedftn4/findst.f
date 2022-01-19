      SUBROUTINE FINDST(STRING,RECORD,IFROM,JFLAG,LREC)                                   
C                                                                                         
C                                                                                         
C     FINDST                            WRITTEN  9/10/75 AUTHOR G.C.                      
C     ------                                                                              
C                                                                                         
C     CALL FINDST(STRING,RECORD,IFROM,JFLAG,LREC)                                         
C                                                                                         
C     THIS ROUTINE SEARCHES FOR A SPECIFIED STRING 'STRING' IN A                          
C     RECORD 'RECORD' OF 'LREC' CHARS. THE SEARCH STARTS FROM THE                         
C     'IFROM' CHARACTER IN THE RECORD, AND 'IFROM' POINTS TO NEXT                         
C     CHARACTER AFTER THE STRING IF IT IS FOUND.                                          
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     STRING ARRAY HOLDING STRING SOUGHT                                                  
C     RECORD ARRAY HOLDING RECORD SEARCHED                                                
C     IFROM  COLUMN OF RECORD FROM WHICH SEARCH BEGINS                                    
C     JFLAG  FLAG WHICH IS                                                                
C            POSITIVE IF STRING FOUND                                                     
C            ZERO IF STRING NOT FOUND                                                     
C     LREC   RECORD LENGTH IN CHARS                                                       
C                                                                                         
C                                                                                         
      DIMENSION RECORD(1),NXTSTG(100),STRING(100)                                         
      INTEGER RECORD,STRING                                                               
C                                                                                         
C                                                                                         
      IF ( IFROM .LE.0 ) IFROM = 1                                                        
C                                                                                         
C                                      CLEAR 'NXTSTG' WITH BLANKS                         
    5 CALL CLEARC ( NXTSTG, 1 )                                                           
C                                      GET NEXT STRING FROM RECORD                        
      CALL NEXT(RECORD,IFROM, NXTSTG,LEN,LREC)                                            
C                                      STRING FOUND?                                      
      IF ( LEN. LE. 0 ) GOTO 20                                                           
C                                      YES, SET JFLAG > 0                                 
C                                      IS IT STRING SOUGHT                                
      IF(MATCHF(NXTSTG,1,STRING,LEN).NE.1) GO TO 5                                        
      JFLAG = IFROM                                                                       
      RETURN                                                                              
C                                                                                         
C                                      'STRING' NOT FOUND IN RECORD                       
20    JFLAG = 0                                                                           
      IFROM = 1                                                                           
      RETURN                                                                              
      END                                                                                 
