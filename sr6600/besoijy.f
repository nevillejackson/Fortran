      SUBROUTINE BESOIJY(X,RJ0,RJ1,Y0,Y1)                               BESSO002          
      DIMENSION T(3)                                                    BESSO003          
      XINV=2./X                                                         BESSO004          
      CONST=.6366197724                                                 BESSO005          
      IF(ABS(X)-1.E-28) 310,310,50                                      BESSO006          
50    L=2*IFIX(ABS(X))+11                                               BESSO007          
      Y0=0.                                                             BESSO008          
      T(3)=Y0                                                           BESSO009          
      RJ0=T(3)                                                          BESSO010          
      T(2)=1.E-20                                                       BESSO011          
      MULT=-1                                                           BESSO012          
90    IFLAG=0                                                           BESSO013          
100   L=L-1                                                             BESSO014          
      TERM=T(3)                                                         BESSO015          
      T(1)=XINV*FLOAT(L+1)*T(2)-T(3)                                    BESSO016          
      T(3)=T(2)                                                         BESSO017          
      T(2)=T(1)                                                         BESSO018          
      IF(-IFLAG) 90,160,310                                             BESSO019          
160   IFLAG=1                                                           BESSO020          
      MULT=-MULT                                                        BESSO021          
      RJ0=RJ0+2.*TERM                                                   BESSO022          
      Y0=Y0+4.*TERM*FLOAT(MULT)/FLOAT(L+2)                              BESSO023          
      IF(L) 210,210,100                                                 BESSO024          
120   ACOSH=ALOG(X+SQRT((X-1.)*(X+1.)))                                 ACOSH013          
      GOTO 150                                                          ACOSH014          
140   ACOSH=ALOG(2.*X)                                                  ACOSH015          
150   RETURN                                                            ACOSH016          
      END                                                                                 
