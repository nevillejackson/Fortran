      SUBROUTINE MINPLOT(NSQ,SQS,IFCONT,NP,IPAIRS)                      MINPL002          
      REAL MAX,MIN                                                      MINPL003          
      DIMENSION IPAIRS(1,1),XKN(10,300),R(2,30),MAX(2,30),MIN(2,30),LP( MINPL004          
     .30)                                                               MINPL005          
      COMMON/FLIX/NEVAL,N,XKN,IFLAG                                     MINPL006          
      COMMON/KINO/R,MAX,MIN,LP,ICOTOUR,S,N1,N2                          MINPL007          
C   AUTHORS- J.A.B.PALMER AND K. SERKOWSKA,C.S.I.R.O.,CANBERRA          MINPL008          
C   LAST REVISION, DECEMBER 1968                                        MINPL009          
C  NSQ  NUMBER OF SQUARES REQUIRED                                      MINPL010          
C  SQS  SIZE OF SQUARE IN INCHES                                        MINPL011          
C  IFCONT=0  IF CONTOURS NOT REQUIRED                                   MINPL012          
C  IFCONT=M  IF CONTOURS REQUIRED                                       MINPL013          
C  M  NUMBER OF CONTOURS REQUIRED IN ONE SQUARE                         MINPL014          
      ICOTOUR=IFCONT                                                    MINPL015          
      S=SQS                                                             MINPL016          
      XSTEP=1.25*S                                                      MINPL017          
      ST=0                                                              MINPL018          
      NN=1                                                              MINPL019          
61    IF(NN.GT.NP) GOTO 69                                              MINPL020          
      NN1=2*NN-1                                                        MINPL021          
      NN2=2*NN                                                          MINPL022          
      N1=IPAIRS(NN1,1)                                                  MINPL023          
      N2=IPAIRS(NN2,1)                                                  MINPL024          
      XMIN=1.E300                                                       MINPL025          
      YMIN=1.E300                                                       MINPL026          
      J=1                                                               MINPL027          
      CALL PLOT(1.,1.,2)                                                MINPL028          
      CALL PLOT(0,0,1)                                                  MINPL029          
      ST=ST+0.4*S                                                       MINPL030          
      CALL PLOT(ST,1.,3)                                                MINPL031          
      ENCODE(1,250,L1) N1                                               MINPL032          
      CALL TEXT(L1,1,2)                                                 MINPL033          
      ST=ST+0.2*S                                                       MINPL034          
      CALL PLOT(ST,1.,3)                                                MINPL035          
      ENCODE(1,250,L2) N2                                               MINPL036          
      CALL TEXT(L2,1,2)                                                 MINPL037          
      CALL PLOT(0,1.,3)                                                 MINPL038          
250   FORMAT(I1)                                                        MINPL039          
      INC=((NEVAL-1)/NSQ)+1                                             MINPL040          
      XMIN=XKN(N1,NEVAL)                                                MINPL041          
      XMAX=XMIN                                                         MINPL042          
      YMIN=XKN(N2,NEVAL)                                                MINPL043          
      YMAX=YMIN                                                         MINPL044          
      MAX(1,1)=XMAX                                                     MINPL045          
      MIN(1,1)=XMIN                                                     MINPL046          
      MAX(2,1)=YMAX                                                     MINPL047          
      MIN(2,1)=YMIN                                                     MINPL048          
      LP(J)=1                                                           MINPL049          
      R(1,1)=0                                                          MINPL050          
      R(2,1)=0                                                          MINPL051          
      NOT=NEVAL-INC                                                     MINPL052          
      II=1                                                              MINPL053          
371   IF(II.GT.NSQ) GOTO 379                                            MINPL054          
      IF(NOT.LT.1) GOTO 630                                             MINPL055          
390   I=NOT                                                             MINPL056          
391   IF(I.GT.NEVAL) GOTO 399                                           MINPL057          
      X1=XKN(N1,I)                                                      MINPL058          
      X2=XKN(N2,I)                                                      MINPL059          
      IF(.NOT.(X1.GT.XMAX)) GOTO 430                                    MINPL060          
      XMAX=X1                                                           MINPL061          
430   CONTINUE                                                          MINPL062          
      IF(.NOT.(X1.LE.XMIN)) GOTO 450                                    MINPL063          
      XMIN=X1                                                           MINPL064          
450   CONTINUE                                                          MINPL065          
      IF(.NOT.(X2.GT.YMAX)) GOTO 470                                    MINPL066          
      YMAX=X2                                                           MINPL067          
470   CONTINUE                                                          MINPL068          
      IF(.NOT.(X2.LE.YMIN)) GOTO 490                                    MINPL069          
      YMIN=X2                                                           MINPL070          
490   CONTINUE                                                          MINPL071          
      I=I+1                                                             MINPL072          
      GOTO 391                                                          MINPL073          
399   CONTINUE                                                          MINPL074          
      R1=XMAX-XMIN                                                      MINPL075          
      R2=YMAX-YMIN                                                      MINPL076          
      J=J+1                                                             MINPL077          
      R(1,J)=R1                                                         MINPL078          
      R(2,J)=R2                                                         MINPL079          
      MAX(1,J)=XMAX                                                     MINPL080          
      MIN(1,J)=XMIN                                                     MINPL081          
      MAX(2,J)=YMAX                                                     MINPL082          
      MIN(2,J)=YMIN                                                     MINPL083          
      LP(J)=NEVAL-NOT+1                                                 MINPL084          
      IF(NOT.EQ.1) GOTO 650                                             MINPL085          
      NOT=NOT-INC                                                       MINPL086          
      II=II+1                                                           MINPL087          
      GOTO 371                                                          MINPL088          
379   CONTINUE                                                          MINPL089          
630   NOT=1                                                             MINPL090          
      GOTO 390                                                          MINPL091          
650   JJ=2                                                              MINPL092          
651   IF(JJ.GT.J) GOTO 659                                              MINPL093          
      CALL SQQ(J-JJ+2)                                                  MINPL094          
      JJ=JJ+1                                                           MINPL095          
      GOTO 651                                                          MINPL096          
659   CONTINUE                                                          MINPL097          
      CALL PLOT(1.,1.,2)                                                MINPL098          
      CALL PLOT(0,0,1)                                                  MINPL099          
      BB=FLOAT(NSQ)*(S+1.)+1.                                           MINPL100          
      CALL PLOT(XSTEP,-BB,3)                                            MINPL101          
      ST=0                                                              MINPL102          
      NN=NN+1                                                           MINPL103          
      GOTO 61                                                           MINPL104          
69    CONTINUE                                                          MINPL105          
      RETURN                                                            MINPL106          
      END                                                               MINPL107          
      SUBROUTINE SQQ(JJ)                                                MINPL108          
      REAL MAX,MIN                                                      MINPL109          
      DIMENSION XKN(10,300),R(2,30),MAX(2,30),MIN(2,30),LP(30),H(41,41) MINPL110          
     .,CO(48),X(10),LQ(41,82)                                           MINPL111          
      COMMON/FLIX/NEVAL,N,XKN,IFLAG                                     MINPL112          
      COMMON/KINO/R,MAX,MIN,LP,ICOTOUR,S,N1,N2                          MINPL113          
      CALL PLOT(1.,1.,2)                                                MINPL114          
      CALL PLOT(0,-1.,1)                                                MINPL115          
      CALL PLOT(0,0,3)                                                  MINPL116          
      CALL PLOT(0,S,4)                                                  MINPL117          
      CALL PLOT(S,S,4)                                                  MINPL118          
      CALL PLOT(S,0,4)                                                  MINPL119          
      CALL PLOT(0,0,4)                                                  MINPL120          
      IF(.NOT.(ICOTOUR.NE.0)) GOTO 240                                  MINPL121          
      IFLAG=1                                                           MINPL122          
      IN=1                                                              MINPL123          
111   IF(IN.GT.N) GOTO 119                                              MINPL124          
      KQZ001=LP(JJ)                                                     MINPL125          
      X(IN)=XKN(IN,KQZ001)                                              MINPL126          
      IN=IN+1                                                           MINPL127          
      GOTO 111                                                          MINPL128          
119   CONTINUE                                                          MINPL129          
      XINT=R(1,JJ)/40E0                                                 MINPL130          
      YINT=R(2,JJ)/40E0                                                 MINPL131          
      CALL PLOT(0,0,1)                                                  MINPL132          
      CALL PLOT(40./S,40./S,2)                                          MINPL133          
      I=1                                                               MINPL134          
171   IF(I.GT.41) GOTO 179                                              MINPL135          
      K=1                                                               MINPL136          
181   IF(K.GT.41) GOTO 189                                              MINPL137          
      X(N1)=MIN(1,JJ)+XINT*FLOAT(I-1)                                   MINPL138          
      X(N2)=MIN(2,JJ)+YINT*FLOAT(K-1)                                   MINPL139          
      CALL GIVEF(X,F)                                                   MINPL140          
      H(I,K)=F                                                          MINPL141          
      K=K+1                                                             MINPL142          
      GOTO 181                                                          MINPL143          
189   CONTINUE                                                          MINPL144          
      I=I+1                                                             MINPL145          
      GOTO 171                                                          MINPL146          
179   CONTINUE                                                          MINPL147          
      CALL LNEW(H,LQ,CO,41,41,82,ICOTOUR,0,0,0)                         MINPL148          
240   CALL PLOT(0,0,3)                                                  MINPL149          
      CALL PLOT(R(1,JJ)/S,R(2,JJ)/S,2)                                  MINPL150          
      CALL PLOT(MIN(1,JJ),MIN(2,JJ),1)                                  MINPL151          
      KZ=NEVAL-LP(JJ)+1                                                 MINPL152          
      CALL PLOT(XKN(N1,KZ),XKN(N2,KZ),3)                                MINPL153          
      I=KZ                                                              MINPL154          
291   IF(I.GT.NEVAL) GOTO 299                                           MINPL155          
      CALL PLOT(XKN(N1,I),XKN(N2,I),4)                                  MINPL156          
      I=I+1                                                             MINPL157          
      GOTO 291                                                          MINPL158          
299   CONTINUE                                                          MINPL159          
      CALL TEXT(1HX,0,1)                                                MINPL160          
      IF(JJ.EQ.2) GOTO 450                                              MINPL161          
      JK=JJ-1                                                           MINPL162          
      RA1=(1E0/(10E0*S))*R(1,JJ)                                        MINPL163          
      RA2=(1E0/(10E0*S))*R(2,JJ)                                        MINPL164          
      IF(.NOT.(R(1,JK).LT.RA1.AND.R(2,JK).LT.RA2)) GOTO 390             MINPL165          
      CALL TEXT(0,0,2)                                                  MINPL166          
      GOTO 450                                                          MINPL167          
390   CALL PLOT(MIN(1,JK),MIN(2,JK),3)                                  MINPL168          
      CALL PLOT(MAX(1,JK),MIN(2,JK),4)                                  MINPL169          
      CALL PLOT(MAX(1,JK),MAX(2,JK),4)                                  MINPL170          
      CALL PLOT(MIN(1,JK),MAX(2,JK),4)                                  MINPL171          
      CALL PLOT(MIN(1,JK),MIN(2,JK),4)                                  MINPL172          
      CALL PLOT(MIN(1,JK),MIN(2,JK),3)                                  MINPL173          
450   CALL PLOT(MIN(1,JJ),MAX(2,JJ),3)                                  MINPL174          
      RETURN                                                            MINPL175          
      END                                                               MINPL176          
