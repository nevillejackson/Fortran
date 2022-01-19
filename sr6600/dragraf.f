      SUBROUTINE DRAGRAF(X,Y,N,SCX,SCY,DS,L,M,LUN)                      DRAWG002          
      DIMENSION X(1),Y(1),D(4),U(4),FL(4),XX(4),YY(4),DL(4)             DRAWG003          
C Q4 CSIR DRAGRAF                                                       DRAWG004          
C AUTHOR - G. SHEARING,CSIRO DIVISION OF COMPUTING RESEARCH             DRAWG005          
C REVISED MAY, 1970                                                     DRAWG006          
      I=2 $ II=2                                                        DRAWG007          
      IF(.NOT.(L.EQ.1H )) GOTO 60                                       DRAWG008          
      II=N                                                              DRAWG009          
60    A=DS*SCX*SCY                                                      DRAWG010          
      SCY2=SCY**2                                                       DRAWG011          
      CALL PLOT(X(1),Y(1),3,LUN)                                        DRAWG012          
      CALL TEXT(L,0,M,LUN)                                              DRAWG013          
      T=X(1)                                                            DRAWG014          
      G=(Y(2)-Y(1))/(X(2)-X(1))                                         DRAWG015          
120   T=A/SQRT(SCY2+(SCX*G)**2)+T                                       DRAWG016          
      IF(T-X(II)) 190,140,140                                           DRAWG017          
140   T=X(II)                                                           DRAWG018          
      CALL PLOT(T,Y(II),4,LUN)                                          DRAWG019          
      I=II                                                              DRAWG020          
      CALL TEXT(L,0,M,LUN)                                              DRAWG021          
      II=II+1                                                           DRAWG022          
190   IF(T-X(2)) 600,600,200                                            DRAWG023          
200   IF(T-X(N)) 210,880,880                                            DRAWG024          
210   IF(T-X(N-1)) 220,220,710                                          DRAWG025          
220   IF(T-X(I)) 250,260,230                                            DRAWG026          
230   I=I+1 $ GOTO 220                                                  DRAWG027          
250   I=I-1                                                             DRAWG028          
260   J=1                                                               DRAWG029          
261   IF(J.GT.4) GOTO 269                                               DRAWG030          
      ISUB=I-2+J                                                        DRAWG031          
      XX(J)=X(ISUB)                                                     DRAWG032          
      YY(J)=Y(ISUB)                                                     DRAWG033          
      J=J+1                                                             DRAWG034          
      GOTO 261                                                          DRAWG035          
269   CONTINUE                                                          DRAWG036          
300   J=1                                                               DRAWG037          
301   IF(J.GT.4) GOTO 309                                               DRAWG038          
      U(J)=T-XX(J)                                                      DRAWG039          
      J=J+1                                                             DRAWG040          
      GOTO 301                                                          DRAWG041          
309   CONTINUE                                                          DRAWG042          
      V01=XX(1)-XX(2)                                                   DRAWG043          
      V02=XX(1)-XX(3)                                                   DRAWG044          
      V03=XX(1)-XX(4)                                                   DRAWG045          
      V12=XX(2)-XX(3)                                                   DRAWG046          
      V13=XX(2)-XX(4)                                                   DRAWG047          
      V23=XX(3)-XX(4)                                                   DRAWG048          
      D(1)=1./(V01*V02*V03)                                             DRAWG049          
      D(2)=-1./(V01*V12*V13)                                            DRAWG050          
      D(3)=1./(V02*V12*V23)                                             DRAWG051          
      D(4)=-1./(V03*V13*V23)                                            DRAWG052          
      DL(1)=U(2)*(U(3)+U(4))+U(3)*U(4)                                  DRAWG053          
      DL(2)=U(3)*(U(4)+U(1))+U(4)*U(1)                                  DRAWG054          
      DL(3)=U(4)*(U(1)+U(2))+U(1)*U(2)                                  DRAWG055          
      DL(4)=U(1)*(U(2)+U(3))+U(2)*U(3)                                  DRAWG056          
      J=1                                                               DRAWG057          
461   IF(J.GT.4) GOTO 469                                               DRAWG058          
      Z=D(J)                                                            DRAWG059          
      K=1                                                               DRAWG060          
481   IF(K.GT.4) GOTO 489                                               DRAWG061          
      IF(K.EQ.J) GOTO 510                                               DRAWG062          
      Z=Z*U(K)                                                          DRAWG063          
510   CONTINUE                                                          DRAWG064          
      K=K+1                                                             DRAWG065          
      GOTO 481                                                          DRAWG066          
489   CONTINUE                                                          DRAWG067          
      FL(J)=Z                                                           DRAWG068          
      J=J+1                                                             DRAWG069          
      GOTO 461                                                          DRAWG070          
469   CONTINUE                                                          DRAWG071          
      F=0. $ G=0.                                                       DRAWG072          
      J=1                                                               DRAWG073          
551   IF(J.GT.4) GOTO 559                                               DRAWG074          
      F=F+FL(J)*YY(J)                                                   DRAWG075          
      G=G+DL(J)*D(J)*YY(J)                                              DRAWG076          
      J=J+1                                                             DRAWG077          
      GOTO 551                                                          DRAWG078          
559   CONTINUE                                                          DRAWG079          
      CALL PLOT(T,F,4,LUN)                                              DRAWG080          
      GOTO 120                                                          DRAWG081          
600   X2=X(1)                                                           DRAWG082          
      XX(2)=X2                                                          DRAWG083          
      X1=X(2)                                                           DRAWG084          
      XX(3)=X1                                                          DRAWG085          
      X0=X(3)                                                           DRAWG086          
      XX(4)=X0                                                          DRAWG087          
      XX(1)=2.*X2-X1                                                    DRAWG088          
      YY(4)=Y(3)                                                        DRAWG089          
      F0=YY(4)                                                          DRAWG090          
      YY(3)=Y(2)                                                        DRAWG091          
      F1=YY(3)                                                          DRAWG092          
      YY(2)=Y(1)                                                        DRAWG093          
      F2=YY(2)                                                          DRAWG094          
      V=1. $ GOTO 820                                                   DRAWG095          
690   YY(1)=F3 $ GOTO 300                                               DRAWG096          
710   X2=X(N)                                                           DRAWG097          
      XX(3)=X2                                                          DRAWG098          
      X1=X(N-1)                                                         DRAWG099          
      XX(2)=X1                                                          DRAWG100          
      X0=X(N-2)                                                         DRAWG101          
      XX(1)=X0                                                          DRAWG102          
      XX(4)=2.*X2-X1                                                    DRAWG103          
      YY(1)=Y(N-2)                                                      DRAWG104          
      F0=YY(1)                                                          DRAWG105          
      YY(2)=Y(N-1)                                                      DRAWG106          
      F1=YY(2)                                                          DRAWG107          
      YY(3)=Y(N)                                                        DRAWG108          
      F2=YY(3)                                                          DRAWG109          
      V=2. $ GOTO 820                                                   DRAWG110          
800   YY(4)=F3 $ GOTO 300                                               DRAWG111          
820   X12=X1-X2                                                         DRAWG112          
      X01=X0-X1                                                         DRAWG113          
      X02=X0-X2                                                         DRAWG114          
      F12=F1-F2                                                         DRAWG115          
      F3=F2-F12+2.*X12*(X12*(F0-F1)-F12*X01)/(X01*X02)                  DRAWG116          
      KQZ001=V                                                          DRAWG117          
      IF(KQZ001.LT.1) KQZ001=1                                          DRAWG118          
      IF(KQZ001.GT.2) KQZ001=2                                          DRAWG119          
      GOTO(690,800),KQZ001                                              DRAWG120          
880   CALL PLOT(X(N),Y(N),4,LUN)                                        DRAWG121          
      CALL TEXT(L,0,M,LUN)                                              DRAWG122          
      CALL PLOT(X(1),Y(1),3,LUN)                                        DRAWG123          
      RETURN                                                            DRAWG124          
      END                                                               DRAWG125          
