*TEXT                                                                                     
      SUBROUTINE FIELD(ICOD,L,IC,L7,J)                                  DEC06000          
C     COMBINES DIGITS FROM IC ARRAY                                     DEC06000          
C     ----------------------------------------------------              DEC06001          
      DIMENSION IC(480)                                                 DEC06003          
      DATA ( IBLK=1H )                                                  M0106004          
      NCDS=J                                                            DEC06005          
      J=0                                                               DEC06006          
      ICOD=0                                                            DEC06007          
      IF=0                                                              DEC06008          
      DO 3 L6=L7,L                                                      DEC06009          
      IF (IC(L6).NE.IBLK) GO TO 1                                       DEC06010          
      IF (L6.EQ.L) GO TO 4                                              DEC06011          
      IF (IF.EQ.0) GO TO 3                                              DEC06012          
      WRITE (6,1003) L6,NCDS                                            DEC06013          
 1003 FORMAT (1H ,17HBLANK CARD COLUMN,I4,12H ON CARD NO.,I6,22H HAS BEEDEC06014          
     1N SET TO ZERO.)                                                   DEC06015          
      IC(L6) = 0                                                        DEC06016          
    1 IF=1                                                              DEC06016          
      ICOD=ICOD*10+IC(L6)                                               DEC06017          
    3 CONTINUE                                                          DEC06018          
      RETURN                                                            DEC06019          
    4 J=1                                                               DEC06020          
      RETURN                                                            DEC06021          
      END                                                               DEC06022          
*ENDTEXT                                                                                  
