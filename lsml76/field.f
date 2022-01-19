      SUBROUTINE FIELD (ICOD,L,IC,L7,J)                                 DK090010          
C     -------------------------------------------                       DK090020          
C     COMBINES DIGITS FROM IC ARRAY                                     DK090030          
C     ----------------------------------------------------              DK090040          
      DIMENSION IC(480)                                                 DK090050          
C-----EXTRACTS FIELD FROM IC(L7) TO IC(L) CHARS                                           
C-----CONVERTS TO INTEGER                                                                 
C-----ICOD=INTEGER RESULT                                                                 
C-----J = NCSD = CURRENT CARD NO , ON INPUT                                               
C-----J = 0 OR 1 , ON OUTPUT                                                              
C-----     0 = OK                                                                         
C-----     1 = LAST CHAR OF FIELD BLANK , OR ANY CHAR NON NUMERIC AND                     
C-----            NOT + OR -                                                              
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
      LEN=L-L7+1                                                                          
      NCDS=J                                                            DK090070          
      J=0                                                               DK090080          
      ICOD=0                                                            DK090090          
      IF=0                                                              DK090100          
      DO 3 L6=L7,L                                                      DK090110          
      IF(NMERIC(IN(IC,L6))) 1,2,5                                                         
    5 IF(NUMBER(IN(IC,L6))) 1,4,1                                                         
    2 CONTINUE                                                                            
      IF (L6.EQ.L) GO TO 4                                              DK090130          
      IF (IF.EQ.0) GO TO 3                                              DK090140          
      WRITE (6,1003) L6,NCDS                                            DK090150          
 1003 FORMAT (1H ,17HBLANK CARD COLUMN,I4,12H ON CARD NO.,I6,22H HAS BEEDK090160          
     1N SET TO ZERO.)                                                   DK090170          
      CALL OUT(IC,L6,IN(KONST(28),MACHC))                                                 
    1 IF=1                                                              DK090190          
    3 CONTINUE                                                          DK090210          
      CALL COPYC(IC,L7,ICOD,1,LEN)                                                        
      ICOD=LNUM(ICOD,LEN)                                                                 
      RETURN                                                            DK090220          
    4 J=1                                                               DK090230          
      RETURN                                                            DK090240          
      END                                                               DK090250          
