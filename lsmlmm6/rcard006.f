*TEXT                                                                                     
      SUBROUTINE RCARD (IN,K,L,IC,JDUM,*)                               DK070010          
      DIMENSION IC(480)                                                 DK070020          
      REAL*8 BUF8(10)                                                   DK070030          
      LOGICAL*1 BUF1(80),CHR1(4)                                        DK070040          
      EQUIVALENCE (BUF8(1),BUF1(1))                                     DK070050          
      INTEGER*4 CHR4,F0                                                 DK070060          
      DATA F0/Z000000F0/                                                DK070070          
      EQUIVALENCE (CHR4,CHR1(1))                                        DK070080          
      CHR4=0                                                            DK070090          
      READ(IN,900,END=901) BUF8                                         DK070100          
  900 FORMAT(10A8)                                                      DK070110          
      IF(L.LE.80) GO TO 899                                             DK070120          
      DO 897 IDO=K,L                                                    DK070130          
      JDO=IDO-((JDUM-1)*80)                                             DK070140          
      CHR1(4)=BUF1(JDO)                                                 DK070150          
      IC(IDO)=CHR4                                                      DK070160          
      IF(IC(IDO).LT.F0) GO TO 897                                       DK070170          
      IC(IDO)=IC(IDO)-F0                                                DK070180          
  897 CONTINUE                                                          DK070190          
      GO TO 902                                                         DK070200          
  899 DO 903 IDO=K,L                                                    DK070210          
      CHR1(4)=BUF1(IDO)                                                 DK070220          
      IC(IDO)=CHR4                                                      DK070230          
      IF(IC(IDO).LT.F0) GO TO 903                                       DK070240          
      IC(IDO)= IC(IDO) -F0                                              DK070250          
  903 CONTINUE                                                          DK070260          
      GO TO 902                                                         DK070270          
  901 RETURN 1                                                          DK070280          
  902 RETURN                                                            DK070290          
      END                                                               DK070300          
*ENDTEXT                                                                                  
