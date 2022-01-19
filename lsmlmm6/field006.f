*TEXT                                                                                     
      SUBROUTINE FIELD (ICOD,L,IC,L7,J)                                 DK090010          
C     -------------------------------------------                       DK090020          
C     COMBINES DIGITS FROM IC ARRAY                                     DK090030          
C     ----------------------------------------------------              DK090040          
      DIMENSION IC(480)                                                 DK090050          
      DATA IBLK/Z00000040/                                              DK090060          
      NCDS=J                                                            DK090070          
      J=0                                                               DK090080          
      ICOD=0                                                            DK090090          
      IF=0                                                              DK090100          
      DO 3 L6=L7,L                                                      DK090110          
      IF (IC(L6).NE.IBLK) GO TO 1                                       DK090120          
      IF (L6.EQ.L) GO TO 4                                              DK090130          
      IF (IF.EQ.0) GO TO 3                                              DK090140          
      WRITE (6,1003) L6,NCDS                                            DK090150          
 1003 FORMAT (1H ,17HBLANK CARD COLUMN,I4,12H ON CARD NO.,I6,22H HAS BEEDK090160          
     1N SET TO ZERO.)                                                   DK090170          
      IC(L6)=0                                                          DK090180          
    1 IF=1                                                              DK090190          
      ICOD=ICOD*10+IC(L6)                                               DK090200          
    3 CONTINUE                                                          DK090210          
      RETURN                                                            DK090220          
    4 J=1                                                               DK090230          
      RETURN                                                            DK090240          
      END                                                               DK090250          
*ENDTEXT                                                                                  
