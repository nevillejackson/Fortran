*TEXT                                                                                     
      SUBROUTINE FIELD(ICOD,L,IC,L7,J)                                    090001          
C     COMBINES DIGITS FROM IC ARRAY                                       090002          
C     ----------------------------------------------------                090003          
      DIMENSION IC(1)                                                     090004          
      DATA ( IBLK=1H )                                                    090005          
      NCDS=J                                                              090006          
      J=0                                                                 090007          
      ICOD=0                                                              090008          
      IF=0                                                                090009          
      DO 3 L6=L7,L                                                        090010          
      IF (IC(L6).NE.IBLK) GO TO 1                                         090011          
      IF (L6.EQ.L) GO TO 4                                                090012          
      IF (IF.EQ.0) GO TO 3                                                090013          
      WRITE (6,1003) L6,NCDS                                              090014          
 1003 FORMAT (1H ,17HBLANK CARD COLUMN,I4,12H ON CARD NO.,I6,22H HAS BEE  090015          
     1N SET TO ZERO.)                                                     090016          
      IC(L6) = 0                                                          090017          
    1 IF=1                                                                090018          
      ICOD=ICOD*10+IC(L6)                                                 090019          
    3 CONTINUE                                                            090020          
      RETURN                                                              090021          
    4 J=1                                                                 090022          
      RETURN                                                              090023          
      END                                                                 090024          
*ENDTEXT                                                                                  
