      SUBROUTINE DRACURV(X,Y,D,N,SCX,SCY,DS,L,M,LUN)                    DRAWC002          
      DIMENSION X(1),Y(1),D(1),U(3),V(3)                                DRAWC003          
C Q4 CSIR DRAWCURV                                                      DRAWC004          
C AUTHOR - G. SHEARING,CSIRO DIVISION OF COMPUTING RESEARCH             DRAWC005          
C REVISED MAY,1970                                                      DRAWC006          
      RSX=1./SCX $ RSY=1./SCY                                           DRAWC007          
      RSX2=RSX**2 $ RSY2=RSY**2                                         DRAWC008          
      CALL PLOT(X(1),Y(1),3,LUN)                                        DRAWC009          
      CALL TEXT(L,0,M,LUN)                                              DRAWC010          
      J=2                                                               DRAWC011          
81    IF(J.GT.N) GOTO 89                                                DRAWC012          
      D(J)=SQRT(RSX2*(X(J)-X(J-1))**2+RSY2*(Y(J)-Y(J-1))**2)            DRAWC013          
      J=J+1                                                             DRAWC014          
      GOTO 81                                                           DRAWC015          
89    CONTINUE                                                          DRAWC016          
      S=0. $ I=1                                                        DRAWC017          
120   S=S+DS $ SS=S                                                     DRAWC018          
140   I=I+1 $ SS=SS-D(I)                                                DRAWC019          
      IF(SS) 170,210,250                                                DRAWC020          
170   S=D(I)+SS $ I=I-1                                                 DRAWC021          
      IF(I.EQ.1) GOTO 470                                               DRAWC022          
      IF(I.EQ.N-1) GOTO 520                                             DRAWC023          
      GOTO 310                                                          DRAWC024          
210   S=0. $ XX=X(I)*RSX                                                DRAWC025          
      YY=Y(I)*RSY $ GOTO 450                                            DRAWC026          
250   IF(I.EQ.N) GOTO 580                                               DRAWC027          
      IF(L.EQ.1H ) GOTO 140                                             DRAWC028          
      CALL PLOT(X(I),Y(I),4,LUN)                                        DRAWC029          
      CALL TEXT(L,0,M,LUN)                                              DRAWC030          
      S=0.                                                              DRAWC031          
      GOTO 120                                                          DRAWC032          
310   J=1                                                               DRAWC033          
311   IF(J.GT.3) GOTO 319                                               DRAWC034          
      ISUB=I+J-2                                                        DRAWC035          
      U(J)=X(ISUB)*RSX                                                  DRAWC036          
      V(J)=Y(ISUB)*RSY                                                  DRAWC037          
      J=J+1                                                             DRAWC038          
      GOTO 311                                                          DRAWC039          
319   CONTINUE                                                          DRAWC040          
      CALL ARC(U,V,S,XA,YA,S23A)                                        DRAWC041          
      J=1                                                               DRAWC042          
361   IF(J.GT.3) GOTO 369                                               DRAWC043          
      ISUB=I-J+3                                                        DRAWC044          
      U(J)=X(ISUB)*RSX                                                  DRAWC045          
      V(J)=Y(ISUB)*RSY                                                  DRAWC046          
      J=J+1                                                             DRAWC047          
      GOTO 361                                                          DRAWC048          
369   CONTINUE                                                          DRAWC049          
      SSS=S23A-S                                                        DRAWC050          
      CALL ARC(U,V,SSS,XB,YB,S23B)                                      DRAWC051          
      Z=2.*S/(S23A+S23B)                                                DRAWC052          
      XX=(1.-Z)*XA+Z*XB                                                 DRAWC053          
      YY=(1.-Z)*YA+Z*YB                                                 DRAWC054          
450   CALL PLOT(XX*SCX,YY*SCY,4,LUN)                                    DRAWC055          
      GOTO 120                                                          DRAWC056          
470   J=1                                                               DRAWC057          
471   IF(J.GT.3) GOTO 479                                               DRAWC058          
      U(J)=X(J)*RSX                                                     DRAWC059          
      V(J)=Y(J)*RSY                                                     DRAWC060          
      J=J+1                                                             DRAWC061          
      GOTO 471                                                          DRAWC062          
479   CONTINUE                                                          DRAWC063          
      CALL ARC(U,V,S-D(2),XX,YY,SSS)                                    DRAWC064          
      GOTO 450                                                          DRAWC065          
520   J=1                                                               DRAWC066          
521   IF(J.GT.3) GOTO 529                                               DRAWC067          
      ISUB=N+J-3                                                        DRAWC068          
      U(J)=X(ISUB)*RSX                                                  DRAWC069          
      V(J)=Y(ISUB)*RSY                                                  DRAWC070          
      J=J+1                                                             DRAWC071          
      GOTO 521                                                          DRAWC072          
529   CONTINUE                                                          DRAWC073          
      CALL ARC(U,V,S,XX,YY,SSS)                                         DRAWC074          
      GOTO 450                                                          DRAWC075          
580   CALL PLOT(X(N),Y(N),4,LUN)                                        DRAWC076          
      CALL TEXT(L,0,M,LUN)                                              DRAWC077          
      RETURN                                                            DRAWC078          
      END                                                               DRAWC079          
      SUBROUTINE ARC(X,Y,S,XX,YY,SS)                                    DRAWC080          
      DIMENSION X(3),Y(3)                                               DRAWC081          
      Z=X(1)*(Y(3)-Y(2))+Y(1)*(X(2)-X(3))+Y(2)*X(3)-X(2)*Y(3)           DRAWC082          
      IF(ABS(Z)-1.E-6) 40,40,90                                         DRAWC083          
40    SS=SQRT((Y(3)-Y(2))**2+(X(3)-X(2))**2)                            DRAWC084          
      Z=S/SS                                                            DRAWC085          
      XX=X(2)+Z*(X(3)-X(2))                                             DRAWC086          
      YY=Y(2)+Z*(Y(3)-Y(2))                                             DRAWC087          
      GOTO 380                                                          DRAWC088          
90    R1=.5*(X(2)**2+Y(2)**2-X(1)**2-Y(1)**2)                           DRAWC089          
      R2=.5*(X(3)**2+Y(3)**2-X(2)**2-Y(2)**2)                           DRAWC090          
      SS=-SIGN(1.,Z)                                                    DRAWC091          
      A11=X(2)-X(1)                                                     DRAWC092          
      A12=Y(2)-Y(1)                                                     DRAWC093          
      A21=X(3)-X(2)                                                     DRAWC094          
      A22=Y(3)-Y(2)                                                     DRAWC095          
      Z=1./(A11*A22-A12*A21)                                            DRAWC096          
      X1=Z*(A22*R1-A12*R2)                                              DRAWC097          
      X2=Z*(A11*R2-A21*R1)                                              DRAWC098          
      R=SQRT((X(1)-X1)**2+(Y(1)-X2)**2)                                 DRAWC099          
      E=Y(2)-X2 $ F=X(2)-X1                                             DRAWC100          
      SOR=SS*S/R                                                        DRAWC101          
      COSA=COS(SOR)                                                     DRAWC102          
      SINA=SIN(SOR)                                                     DRAWC103          
      XX=X1+F*COSA-E*SINA                                               DRAWC104          
      YY=X2+F*SINA+E*COSA                                               DRAWC105          
      R1=Y(3)-X2 $ R2=X(3)-X1                                           DRAWC106          
      X1=R1*F-R2*E                                                      DRAWC107          
      X2=R2*F+R1*E                                                      DRAWC108          
      IF(.NOT.(ABS(X2).LT.1.E-6)) GOTO 340                              DRAWC109          
      Z=1.5707963268 $ GOTO 370                                         DRAWC110          
340   Z=ABS(ATAN(X1/X2))                                                DRAWC111          
      IF(X2) 360,370,370                                                DRAWC112          
360   Z=3.1415926536-Z                                                  DRAWC113          
370   SS=R*Z                                                            DRAWC114          
380   RETURN                                                            DRAWC115          
      END                                                               DRAWC116          
