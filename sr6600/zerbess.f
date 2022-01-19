      SUBROUTINE ZERBESS(X,BESS,RNEUM)                                  ZEROB002          
      DIMENSION T(3)                                                    ZEROB003          
      IF(X-1.E-28) 280,280,30                                           ZEROB004          
30    L=2*IFIX(ABS(X))+11                                               ZEROB005          
      RNEUM=0                                                           ZEROB006          
      T(3)=RNEUM                                                        ZEROB007          
      BESS=T(3)                                                         ZEROB008          
      T(2)=1.E-20                                                       ZEROB009          
      MULT=-1                                                           ZEROB010          
      XINV=2./X                                                         ZEROB011          
80    IFLAG=0                                                           ZEROB012          
90    L=L-1                                                             ZEROB013          
      TERM=T(3)                                                         ZEROB014          
      T(1)=XINV*FLOAT(L+1)*T(2)-T(3)                                    ZEROB015          
      T(3)=T(2)                                                         ZEROB016          
      T(2)=T(1)                                                         ZEROB017          
      IF(-IFLAG) 80,150,280                                             ZEROB018          
150   IFLAG=1                                                           ZEROB019          
      MULT=-MULT                                                        ZEROB020          
      BESS=BESS+2.*TERM                                                 ZEROB021          
      RNEUM=RNEUM+4.*TERM*FLOAT(MULT)/FLOAT(L+2)                        ZEROB022          
      IF(L) 200,200,90                                                  ZEROB023          
200   BESS=BESS+T(1)                                                    ZEROB024          
      IF(MULT) 220,280,230                                              ZEROB025          
220   RNEUM=FLOAT(MULT)*RNEUM                                           ZEROB026          
230   RNEUM=RNEUM/BESS                                                  ZEROB027          
      BESS=T(1)/BESS                                                    ZEROB028          
250   RNEUM=BESS*ALOG(X)+RNEUM                                          ZEROB029          
      RNEUM=.6366197724*(RNEUM-.1159315157*BESS)                        ZEROB030          
      GOTO 310                                                          ZEROB031          
280   BESS=1                                                            ZEROB032          
      RNEUM=0                                                           ZEROB033          
      GOTO 250                                                          ZEROB034          
310   RETURN                                                            ZEROB035          
      END                                                               ZEROB036          
