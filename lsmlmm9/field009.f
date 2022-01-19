      SUBROUTINE FIELD(ICOD,L,IC,L7,J)                                                    
C-----                                                                                    
C-----EXTRACTS FIELD FROM POSITIONS L7 TO L OF IC ARRAY ON JTH CARD                       
C-----RETURNS J=1 IF UNITS POSITION OF FIELD IS BLANK                                     
C-----RETURNS J=0 OTHERWISE                                                               
C-----ICOD RETURNS FIELD L7 TO L IN INTEGER FORM (RIGHT JUST AND DECODED)                 
C-----                                                                                    
      DIMENSION IC(480)                                                                   
      DATA IBLK/00000000000000000055B/                                                    
C-----                                                                                    
      J=0                                                                                 
      LEN=L-L7+1                                                                          
C-----EXTRACT FIELD                                                                       
      ICOD=IGETRB(IC,LEN,L7,48)                                                           
C-----CHECK IF LAST 2 OCTAL DIGITS ARE 55B                                                
      IF(AND(COMPL(MASK(54)),ICOD).NE.IBLK) GO TO 4                                       
C-----SET J=1 IF LAST COL BLANK                                                           
      J=1                                                                                 
C-----DECODE WHETHER BLANK OR NOT                                                         
    4 ICOD=INUM(ICOD)                                                                     
C-----BLANK WILL DECODE AS -0                                                             
      RETURN                                                                              
      END                                                                                 
