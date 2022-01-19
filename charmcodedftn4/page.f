      SUBROUTINE PAGE( STRING, LENGTH, ROW, COL )                                         
C-----MACHINE DEPENDENT ROUTINE - CYBER 76 VERSION                                        
C-----CHECK FORMAT 1001                                                                   
C                                                                                         
      INTEGER ROW,R,COL,C                                                                 
      INTEGER STRING                                                                      
      INTEGER ONE,ZERO                                                                    
C                                                                                         
      DIMENSION STRING(1),IPAGE(14,120)                                                   
      DIMENSION K(120)                                                                    
C                                                                                         
      DATA ONE / 1H1 /                                                                    
      DATA NBLANK / 1H /                                                                  
      DATA ZERO /1H0/                                                                     
C                                                                                         
C                                                                                         
C     PAGE                              WRITTEN 1974 AUTHOR J.N.                          
C     ----                                                                                
C                                                                                         
C     CALL PAGE( STRING, LENGTH, ROW, COL )                                               
C                                                                                         
C     AN AID TO REPORT WRITING.  SETS UP INFORMATION ANYWHERE ON A PAGE                   
C     AND PRINTS IT ON REQUEST.  ROW AND COL ARE USED BOTH TO SPECIFY A                   
C     POSITION ON THE PAGE AND REQUEST PARTICULAR FUNCTIONS                               
C     THE PAGE CONSISTS OF MAXROW LINES (ROWS) EACH MAXCOL COLUMNS WIDE. THE              
C     FIRST COLUMN OF EACH LINE IS USED TO PROVIDE CARRIAGE CONTROL IN                    
C     THE USUAL MANNER.                                                                   
C                                                                                         
C     ROW  COL  MEANING                                                                   
C      0    0   CLEAR PAGE TO ALL BLANKS AND CLEAR LIST OF MARKED ROWS                    
C               SET MAXROWS AND MAXCOL TO STRING(1) AND LENGTH                            
C      0   +VE  PRINT PAGE ON UNIT 'COL' AND THEN CLEAR ALL UNMARKED                      
C               ROWS                                                                      
C     +VE  +VE  INSERT STRING STARTING AT GIVEN 'ROW' AND COLUMN 'COL'                    
C     -VE   0   MARK ROW NUMBER -ROW AS NOT TO BE CLEARED                                 
C      0   -VE  INSERT DOUBLE SPACE CARRIAGE CONTROL IN FIRST COL EACH ROW                
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     STRING OF CHARACTERS TO BE INSERTED ON THE PAGE                                     
C     LENGTH OF ABOVE STRING                                                              
C     ROW    THAT STRING IS TO BE PLACED IN                                               
C     COL    COLUMN AT WHICH STRING STARTS                                                
C                                                                                         
C     VARIABLES                                                                           
C                                                                                         
C     K      LIST CORRESPONDING TO ROW NUMBERS.  K( I ) = 0 IF ROW I                      
C            MAY BE CLEARED,  ELSE =1 IF ROW TO BE PRESERVED.                             
C     IPAGE  HOLDS A WHOLE PAGE                                                           
C                                                                                         
      R = ROW                                                                             
      C = COL                                                                             
C                                       FIND OUT REQUIRED OPTION                          
      IF( R + C ) 700,200,350                                                             
C            R =  -VE  0   ?                                                              
C            C =   0   0   ?                                                              
C                                                                                         
C                                       CLEAR THE PAGE TO ALL BLANKS                      
  200 CONTINUE                                                                            
      MAXROW=STRING(1)                                                                    
      MAXCOL=LENGTH                                                                       
      MAXWRD=LWORDS(MAXCOL)                                                               
      DO 300 I=1,MAXWRD                                                                   
      DO 300 J=1,MAXROW                                                                   
      K( J ) = 0                                                                          
300   IPAGE( I,J ) = NBLANK                                                               
      LASTP=0                                                                             
      GOTO 650                                                                            
350   IF( R .NE. 0 ) GOTO 800                                                             
C                                       PRINT AND CLEAR UNMARKED ROWS                     
400   CALL OUT( IPAGE, 1, IN( ONE,1 ) )                                                   
C                                                                                         
      DO 600 J=1,MAXROW                                                                   
      IF( J .GT. LAST ) GOTO 600                                                          
      WRITE(C,1001)(IPAGE(I,J),I=1,MAXWRD)                                                
C                                       PRESERVE THIS ROW ?                               
      IF( K( J ) .EQ. 1 ) GOTO 600                                                        
C                                                                                         
      DO 500 I=1,MAXWRD                                                                   
500   IPAGE( I,J ) = NBLANK                                                               
C                                                                                         
600   CONTINUE                                                                            
C                                                                                         
  650 LAST=LASTP                                                                          
      CALL OUT( IPAGE, 1, IN( ONE,1 ) )                                                   
C                                                                                         
      GOTO 900                                                                            
C     MARK ROW OR DOUBLE SPACE                                                            
  700 IF(R) 701,702,900                                                                   
C     DOUBLE SPACE                                                                        
  702 DO 703 J=2,MAXROW                                                                   
  703 CALL OUT(IPAGE(1,J),1,IN(ZERO,1))                                                   
      RETURN                                                                              
C                                       MARK ROW AS NOT TO BE CLEARED                     
  701 R=-R                                                                                
      K(  R ) = 1                                                                         
      IF(R.LE.LAST) GO TO 704                                                             
      LAST=R                                                                              
      LASTP=LAST                                                                          
  704 RETURN                                                                              
C                                                                                         
800   L = LENGTH                                                                          
      IF( C + L .GT. MAXCOL+1 ) L = MAXCOL+1 - C                                          
      IF( R .GT. MAXROW ) GOTO 900                                                        
      CALL COPYC(STRING,1,IPAGE(1,R),C,L)                                                 
      IF( R .GT. LAST ) LAST = R                                                          
900   RETURN                                                                              
C                                                                                         
 1001 FORMAT(13A10,A6)                                                                    
1002  FORMAT( 1H1 )                                                                       
C                                                                                         
      END                                                                                 
