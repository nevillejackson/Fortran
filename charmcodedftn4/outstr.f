      SUBROUTINE OUTSTR( LU, STRING, LENGTH, LBLANK,FMT )                                 
C                                                                                         
      DIMENSION STRING( 1 ),FORM( 3 ), FMT( 1 )                                           
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      DATA FORM( 1 ) / 1H(  /                                                             
C                                                                                         
C     WRITES A 'STRING' OF GIVEN 'LENGTH' PRECEDED BY 'LBLANK' BLANKS ON                  
C     UNIT 'LU'.  LBLANKS MAY BE ZERO.  IF LBLANK+LENGTH > 136 THEN                       
C     LBLANK REDUCED TO ONE.  IF STILL TOO LONG LENGTTH IS REDUCED.                       
C     MAKES A FORMAT STATEMENT  ( **X, **A**, 1A** )                                      
C                                       RETURNS THE FORMAT STRING USED                    
C                                       AS THE PARAMETER 'FMT'                            
      LS = LENGTH                                                                         
      LB = LBLANK                                                                         
C                                       CORRECT CRAZY VALUES                              
      IF( LB .LT. 0 ) LB = 0                                                              
      IF( LS .LT. 0 ) LS = 0                                                              
C                                       CHECK FOR NO STRING AND NO                        
C                                       BLANKS                                            
      IF( LB+LS .EQ. 0 ) RETURN                                                           
C                                       CHECK IT FITS ON A LINE 136 LONG                  
      IF( LS+LB .LT. 137 ) GOTO 100                                                       
C                                       USER HAS ASKED FOR OUTPUT OF                      
C                                       MORE THAN 136 CHARACTERS                          
      IF( LB .GT. 1 ) LB = 1                                                              
      IF( LB+LS .LT. 137 ) GOTO 100                                                       
C                                       HAVE TO TRUNCATE THE STRING                       
      LS = 135                                                                            
C                                                                                         
100   J = 2                                                                               
C                                       DON'T SET UP **X IF NOT NEEDED                    
      IF( LB .EQ. 0 ) GOTO 200                                                            
C                                       SET UP NUMBER OF BLANKS IN **X                    
      CALL CONVI( LB, FORM, J )                                                           
      CALL COPYST( 2HX, , 2, FORM, J )                                                    
C                                       IS THERE A STRING?                                
200   IF( LS .NE. 0 ) GOTO 250                                                            
C                                       NO SO BACKSPACE OVER COMMA                        
220   J = J - 1                                                                           
      GOTO 400                                                                            
C                                       FIND NUMBER OF WHOLE WORDS                        
250   N = LS/MACHC                                                                        
C                                       JUMP IF NONE                                      
      IF( N .EQ. 0 ) GOTO 300                                                             
C                                       SET UP 'N'A'MACHC',                               
      CALL CONVI( N, FORM, J )                                                            
      CALL COPYST( 1HA, 1, FORM, J )                                                      
      CALL CONVI( MACHC, FORM, J )                                                        
      CALL COPYST( 1H, , 1, FORM, J )                                                     
C                                       FIND NO. OF CHARS IN FINAL WORD                   
300   M = MOD( LS,MACHC )                                                                 
C                                       JUMP IF NONE                                      
      IF( M .EQ. 0 ) GOTO 220                                                             
      N = N + 1                                                                           
C                                       SET UP 'A'M'                                      
      CALL COPYST( 2H1A, 2, FORM, J )                                                     
      CALL CONVI( M, FORM, J )                                                            
C                                       TERMINATE FORMAT WITH )                           
400   CALL COPYST( 1H), 1, FORM, J )                                                      
      IF( N .EQ. 0 ) GOTO 500                                                             
      WRITE( LU,FORM ) ( STRING( I ), I = 1,N )                                           
      GOTO 600                                                                            
C                                                                                         
500   WRITE( LU,FORM )                                                                    
  600 CALL COPYC(FORM,1,FMT,1,J)                                                          
      RETURN                                                                              
C                                                                                         
      END                                                                                 
