      SUBROUTINE VIETA(A,Y,MTYPE)                                       VIETA002          
      DIMENSION A(4),B(3),Y(3)                                          VIETA003          
C     EXPLICIT SOLUTION OF THE GENERAL CUBIC EQUATION                   VIETA004          
      B(1)=A(2)/A(1)                                                    VIETA005          
      B10V3=B(1)/3.0                                                    VIETA006          
      B(2)=A(3)/A(1)                                                    VIETA007          
      B(3)=A(4)/A(1)                                                    VIETA008          
      ALF=B(2)-B(1)*B10V3                                               VIETA009          
      BET=2.0*B10V3**3-B(2)*B10V3+B(3)                                  VIETA010          
      BET0V2=BET/2.0                                                    VIETA011          
      ALF0V3=ALF/3.0                                                    VIETA012          
      CUA0V3=ALF0V3**3                                                  VIETA013          
      SQB0V2=BET0V2**2                                                  VIETA014          
      DEL=SQB0V2+CUA0V3                                                 VIETA015          
      IF(DEL) 340,140,250                                               VIETA016          
140   MTYPE=2                                                           VIETA017          
      GAM=SQRT(-ALF0V3)                                                 VIETA018          
      IF(BET) 210,210,170                                               VIETA019          
170   Y(1)=-2.0*GAM-B10V3                                               VIETA020          
      Y(2)=GAM-B10V3                                                    VIETA021          
      Y(3)=Y(2)                                                         VIETA022          
      GOTO 450                                                          VIETA023          
210   Y(1)=2.0*GAM-B10V3                                                VIETA024          
      Y(2)=-GAM-B10V3                                                   VIETA025          
      Y(3)=Y(2)                                                         VIETA026          
      GOTO 450                                                          VIETA027          
250   MTYPE=3                                                           VIETA028          
      EPS=SQRT(DEL)                                                     VIETA029          
      TAU=-BET0V2                                                       VIETA030          
      R=CUBERT(TAU+EPS)                                                 VIETA031          
      S=CUBERT(TAU-EPS)                                                 VIETA032          
      Y(1)=R+S-B10V3                                                    VIETA033          
      Y(2)=-(R+S)/2.0-B10V3                                             VIETA034          
      Y(3)=0.86602540*(R-S)                                             VIETA035          
      GOTO 450                                                          VIETA036          
340   MTYPE=1                                                           VIETA037          
      QUOT=SQB0V2/CUA0V3                                                VIETA038          
      ROOT=SQRT(-QUOT)                                                  VIETA039          
      IF(BET) 400,380,380                                               VIETA040          
380   PHI=(1.5707963+ASIN(ROOT))/3.0                                    VIETA041          
      GOTO 410                                                          VIETA042          
400   PHI=ACOS(ROOT)/3.0                                                VIETA043          
410   FACT=2.0*SQRT(-ALF0V3)                                            VIETA044          
      Y(1)=FACT*COS(PHI)-B10V3                                          VIETA045          
      Y(2)=FACT*COS(PHI+2.0943951)-B10V3                                VIETA046          
      Y(3)=FACT*COS(PHI+4.1887902)-B10V3                                VIETA047          
450   CONTINUE                                                          VIETA048          
C     THIS IS THE LAST CARD OF VIETA SUBROUTINE                         VIETA049          
      RETURN                                                            VIETA050          
      END                                                               VIETA051          
