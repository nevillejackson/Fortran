      SUBROUTINE FLEOMIN(N,X,F,EPS,ICONV,LIMIT,H,LOADH)                 FLEPO002          
      DIMENSION X(1),H(1)                                               FLEPO003          
C        MODIFIED NOVEMBER 1971, A. COOK.                               FLEPO004          
C     SUBROUTINE FLEPOMIN(N,X,F,H,G,S,GAMMA,SIGMA,LIMIT,LOADH,ICONV,EPS)FLEPO005          
C     DIMENSION X(1),H(1),G(1),S(1),GAMMA(1),SIGMA(1)                   FLEPO006          
C     TO FIND MINIMA OF FUNCTION OF MANY VARIABLES                      FLEPO007          
C     N IS THE NUMBER OF VARIABLES                                      FLEPO008          
C     X IS ON ENTRY THE INITIAL APPROXIMATION AND ON EXIT THE SOLUTION  FLEPO009          
C     IS FOUND                                                          FLEPO010          
C     F IS THE VALUE OF THE FUNCTION AT THE SUPPOSED SOLUTION           FLEPO011          
C     H IS WORKING SPACE OF DIMENSION AT LEAST N*(4+(N+1)/2)            FLEPO012          
C     LIMIT IS ON ENTRY THE MAXIMUM PERMITED NUMBER OF ITERATIONS       FLEPO013          
C     AND ON EXIT THE NUMBER OF ITERATIONS ACTUALLY USED                FLEPO014          
C     IF LOADH=0 THE PROCEDURE SUPPLIES THE UNIT MATRIX,IF LOADH=1      FLEPO015          
C     THE MATRIX PROVIDED BY THE USER IS USED                           FLEPO016          
C     ICONV=1 IF A MINIMUM HAS BEEN FOUND ,ICONV=0 IF THE NUMBER        FLEPO017          
C     OF ITERATIONS EXCEEDS LIMIT                                       FLEPO018          
      N1=(N*(N+1))/2                                                    FLEPO019          
      N2=N1+N                                                           FLEPO020          
      N3=N2+N                                                           FLEPO021          
      N4=N3+N                                                           FLEPO022          
C     SET INITIAL MATRIX H                                              FLEPO023          
      IF(LOADH.NE.0) GOTO 150                                           FLEPO024          
C     BEGIN FORMATION OF UNIT MATRIX H                                  FLEPO025          
      K=1                                                               FLEPO026          
      I=1                                                               FLEPO027          
81    IF(I.GT.N) GOTO 89                                                FLEPO028          
      H(K)=1.0                                                          FLEPO029          
      NN=N-I                                                            FLEPO030          
      J=1                                                               FLEPO031          
111   IF(J.GT.NN) GOTO 119                                              FLEPO032          
      KJ=K+J                                                            FLEPO033          
      H(KJ)=0.0                                                         FLEPO034          
      J=J+1                                                             FLEPO035          
      GOTO 111                                                          FLEPO036          
119   CONTINUE                                                          FLEPO037          
      K=K+NN+1                                                          FLEPO038          
C     END FORMATION OF UNIT MATRIX H                                    FLEPO039          
C     START OF MINIMISATION                                             FLEPO040          
      I=I+1                                                             FLEPO041          
      GOTO 81                                                           FLEPO042          
89    CONTINUE                                                          FLEPO043          
150   ICONV=1                                                           FLEPO044          
      STEP=1.0                                                          FLEPO045          
      CALL GIVEF(X,F)                                                   FLEPO046          
C     CALL DERIV(X,G)                                                   FLEPO047          
      K1=N1+1                                                           FLEPO048          
      CALL DERIV(X,H(K1))                                               FLEPO049          
      ICOUNT=1                                                          FLEPO050          
201   IF(ICOUNT.GT.LIMIT) GOTO 209                                      FLEPO051          
      IC=ICOUNT                                                         FLEPO052          
C     START OF PRESERVATION X AND G                                     FLEPO053          
      I=1                                                               FLEPO054          
221   IF(I.GT.N) GOTO 229                                               FLEPO055          
C     SIGMA(I)=X(I)                                                     FLEPO056          
      KI4=I+N4                                                          FLEPO057          
      H(KI4)=X(I)                                                       FLEPO058          
C 202 GAMMA(I)=G(I)                                                     FLEPO059          
      KI3=I+N3                                                          FLEPO060          
      KI1=I+N1                                                          FLEPO061          
      H(KI3)=H(KI1)                                                     FLEPO062          
C     END OF PRESERVATION X AND G                                       FLEPO063          
C     FORMATION OF CHOOSEN DIRECTION OF ADVANCE  S=-H*G                 FLEPO064          
      I=I+1                                                             FLEPO065          
      GOTO 221                                                          FLEPO066          
229   CONTINUE                                                          FLEPO067          
      K=0                                                               FLEPO068          
      WORK=0.0                                                          FLEPO069          
      I=1                                                               FLEPO070          
301   IF(I.GT.N) GOTO 309                                               FLEPO071          
      J=1                                                               FLEPO072          
311   IF(J.GT.N) GOTO 319                                               FLEPO073          
      K=K+1                                                             FLEPO074          
      IF(.NOT.(I.GT.J)) GOTO 370                                        FLEPO075          
      IK=J                                                              FLEPO076          
      JK=I                                                              FLEPO077          
      GOTO 390                                                          FLEPO078          
370   IK=I                                                              FLEPO079          
      JK=J                                                              FLEPO080          
C 204 WORK=WORK+H((2*N-IK)*(IK-1)/2+JK)*G(K)                            FLEPO081          
390   JKN=((2*N-IK)*(IK-1))/2+JK                                        FLEPO082          
      K1=K+N1                                                           FLEPO083          
      WORK=WORK+H(JKN)*H(K1)                                            FLEPO084          
C     S(I)=-WORK                                                        FLEPO085          
      J=J+1                                                             FLEPO086          
      GOTO 311                                                          FLEPO087          
319   CONTINUE                                                          FLEPO088          
      KI2=I+N2                                                          FLEPO089          
      H(KI2)=-WORK                                                      FLEPO090          
      K=0                                                               FLEPO091          
      WORK=0.0                                                          FLEPO092          
C     END FORMATION OF  S                                               FLEPO093          
C     SEARCH ALONG  S                                                   FLEPO094          
      I=I+1                                                             FLEPO095          
      GOTO 301                                                          FLEPO096          
309   CONTINUE                                                          FLEPO097          
      FB=F                                                              FLEPO098          
      GB=0.0                                                            FLEPO099          
      I=1                                                               FLEPO100          
481   IF(I.GT.N) GOTO 489                                               FLEPO101          
C 205 GB=GB+G(I)*S(I)                                                   FLEPO102          
      KN1=I+N1                                                          FLEPO103          
      KN2=I+N2                                                          FLEPO104          
      GB=GB+H(KN1)*H(KN2)                                               FLEPO105          
      I=I+1                                                             FLEPO106          
      GOTO 481                                                          FLEPO107          
489   CONTINUE                                                          FLEPO108          
      IF(GB.GT.0.0) GOTO 1730                                           FLEPO109          
      OLDF=F                                                            FLEPO110          
      RITA=STEP                                                         FLEPO111          
C     EXTRAPOLATE                                                       FLEPO112          
550   FA=FB                                                             FLEPO113          
      GA=GB                                                             FLEPO114          
      I=1                                                               FLEPO115          
571   IF(I.GT.N) GOTO 579                                               FLEPO116          
C 206 X(I)=X(I)+RITA*S(I)                                               FLEPO117          
      KN2=I+N2                                                          FLEPO118          
      X(I)=X(I)+RITA*H(KN2)                                             FLEPO119          
      I=I+1                                                             FLEPO120          
      GOTO 571                                                          FLEPO121          
579   CONTINUE                                                          FLEPO122          
      CALL GIVEF(X,F)                                                   FLEPO123          
C     CALL DERIV(X,G)                                                   FLEPO124          
      KN1=N1+1                                                          FLEPO125          
      CALL DERIV(X,H(KN1))                                              FLEPO126          
      FB=F                                                              FLEPO127          
      GB=0.0                                                            FLEPO128          
      I=1                                                               FLEPO129          
651   IF(I.GT.N) GOTO 659                                               FLEPO130          
C 207 GB=GB+G(I)*S(I)                                                   FLEPO131          
      KN1=I+N1                                                          FLEPO132          
      KN2=I+N2                                                          FLEPO133          
      GB=GB+H(KN1)*H(KN2)                                               FLEPO134          
      I=I+1                                                             FLEPO135          
      GOTO 651                                                          FLEPO136          
659   CONTINUE                                                          FLEPO137          
      IF(.NOT.(GB.LT.0.0.AND.FB.LT.FA)) GOTO 730                        FLEPO138          
      RITA=4.0*STEP                                                     FLEPO139          
      STEP=4.0*STEP                                                     FLEPO140          
      GOTO 550                                                          FLEPO141          
C     INTERPOLATE                                                       FLEPO142          
730   Z=3.0*(FA-FB)/RITA+GA+GB                                          FLEPO143          
      W=Z*Z-GA*GB                                                       FLEPO144          
      IF(.NOT.(W.LT.0.0)) GOTO 780                                      FLEPO145          
      W=0.0                                                             FLEPO146          
      GOTO 790                                                          FLEPO147          
780   W=SQRT(W)                                                         FLEPO148          
790   CONTINUE                                                          FLEPO149          
      IF(.NOT.(GA+Z.GE.0.0)) GOTO 830                                   FLEPO150          
      R=(GA+Z+W)/(GA+GB+2.*Z)                                           FLEPO151          
      GOTO 840                                                          FLEPO152          
830   R=GA/(GA+Z-W)                                                     FLEPO153          
840   RLAMBDA=RITA*(1.0-R)                                              FLEPO154          
      I=1                                                               FLEPO155          
851   IF(I.GT.N) GOTO 859                                               FLEPO156          
C 208 X(I)=X(I)-RLAMBDA*S(I)                                            FLEPO157          
      KN2=I+N2                                                          FLEPO158          
      X(I)=X(I)-RLAMBDA*H(KN2)                                          FLEPO159          
      I=I+1                                                             FLEPO160          
      GOTO 851                                                          FLEPO161          
859   CONTINUE                                                          FLEPO162          
      CALL GIVEF(X,F)                                                   FLEPO163          
C     CALL DERIV(X,G)                                                   FLEPO164          
      KN1=N1+1                                                          FLEPO165          
      CALL DERIV(X,H(KN1))                                              FLEPO166          
      IF(.NOT.(F.GT.FA.OR.F.GT.FB)) GOTO 1100                           FLEPO167          
      STEP=STEP/4.0                                                     FLEPO168          
      IF(.NOT.(FB.LT.FA)) GOTO 1010                                     FLEPO169          
      I=1                                                               FLEPO170          
941   IF(I.GT.N) GOTO 949                                               FLEPO171          
C 209 X(I)=X(I)+RLAMBDA*S(I)                                            FLEPO172          
      KN2=I+N2                                                          FLEPO173          
      X(I)=X(I)+RLAMBDA*H(KN2)                                          FLEPO174          
      I=I+1                                                             FLEPO175          
      GOTO 941                                                          FLEPO176          
949   CONTINUE                                                          FLEPO177          
      CALL GIVEF(X,F)                                                   FLEPO178          
C     CALL DERIV(X,G)                                                   FLEPO179          
      KN1=N1+1                                                          FLEPO180          
      CALL DERIV(X,H(KN1))                                              FLEPO181          
      GOTO 1100                                                         FLEPO182          
1010  GB=0.0                                                            FLEPO183          
      I=1                                                               FLEPO184          
1021  IF(I.GT.N) GOTO 1029                                              FLEPO185          
C 210 GB=GB+G(I)*S(I)                                                   FLEPO186          
      KN1=I+N1                                                          FLEPO187          
      KN2=I+N2                                                          FLEPO188          
      GB=GB+H(KN1)*H(KN2)                                               FLEPO189          
      I=I+1                                                             FLEPO190          
      GOTO 1021                                                         FLEPO191          
1029  CONTINUE                                                          FLEPO192          
      IF(GB.LT.0.0.AND.ICOUNT.GT.N.AND.STEP.LT.1.0E-06) GOTO 1730       FLEPO193          
      FB=F                                                              FLEPO194          
      RITA=RITA-RLAMBDA                                                 FLEPO195          
      GOTO 730                                                          FLEPO196          
C     END OF SEARCH ALONG  S                                            FLEPO197          
1100  I=1                                                               FLEPO198          
1101  IF(I.GT.N) GOTO 1109                                              FLEPO199          
C     SIGMA(I)=X(I)-SIGMA(I)                                            FLEPO200          
      KN4=I+N4                                                          FLEPO201          
      H(KN4)=X(I)-H(KN4)                                                FLEPO202          
C 211 GAMMA(I)=G(I)-GAMMA(I)                                            FLEPO203          
      KN1=I+N1                                                          FLEPO204          
      KN3=I+N3                                                          FLEPO205          
      H(KN3)=H(KN1)-H(KN3)                                              FLEPO206          
      I=I+1                                                             FLEPO207          
      GOTO 1101                                                         FLEPO208          
1109  CONTINUE                                                          FLEPO209          
      SG=0.0                                                            FLEPO210          
      I=1                                                               FLEPO211          
1171  IF(I.GT.N) GOTO 1179                                              FLEPO212          
C 212 SG=SG+SIGMA(I)*GAMMA(I)                                           FLEPO213          
      KN4=I+N4                                                          FLEPO214          
      KN3=I+N3                                                          FLEPO215          
      SG=SG+H(KN4)*H(KN3)                                               FLEPO216          
      I=I+1                                                             FLEPO217          
      GOTO 1171                                                         FLEPO218          
1179  CONTINUE                                                          FLEPO219          
      IF(.NOT.(ICOUNT.GE.N)) GOTO 1300                                  FLEPO220          
      SS=0.0                                                            FLEPO221          
      SIGMASQ=0.0                                                       FLEPO222          
      I=1                                                               FLEPO223          
1241  IF(I.GT.N) GOTO 1249                                              FLEPO224          
C     SS=SS+S(I)*S(I)                                                   FLEPO225          
      KN2=I+N2                                                          FLEPO226          
      SS=SS+H(KN2)*H(KN2)                                               FLEPO227          
C 213 SIGMASQ=SIGMASQ+SIGMA(I)*SIGMA(I)                                 FLEPO228          
      KN4=I+N4                                                          FLEPO229          
      SIGMASQ=SIGMASQ+H(KN4)*H(KN4)                                     FLEPO230          
C     INVESTIGATION THE LENGTHS OF THE VECTORS S AND X                  FLEPO231          
      I=I+1                                                             FLEPO232          
      GOTO 1241                                                         FLEPO233          
1249  CONTINUE                                                          FLEPO234          
      IF(SS.LT.EPS**2.AND.SIGMASQ.LT.EPS**2) GOTO 1740                  FLEPO235          
C     UPDATING OF  S                                                    FLEPO236          
1300  K=0                                                               FLEPO237          
      WORK=0.0                                                          FLEPO238          
      I=1                                                               FLEPO239          
1321  IF(I.GT.N) GOTO 1329                                              FLEPO240          
      J=1                                                               FLEPO241          
1331  IF(J.GT.N) GOTO 1339                                              FLEPO242          
      K=K+1                                                             FLEPO243          
      IF(.NOT.(I.GT.J)) GOTO 1390                                       FLEPO244          
      IK=J                                                              FLEPO245          
      JK=I                                                              FLEPO246          
      GOTO 1410                                                         FLEPO247          
1390  IK=I                                                              FLEPO248          
      JK=J                                                              FLEPO249          
C 216 WORK=WORK+H((2*N-IK)*(IK-1)/2+JK)*GAMMA(K)                        FLEPO250          
1410  KNI=((2*N-IK)*(IK-1))/2+JK                                        FLEPO251          
      KN3=K+N3                                                          FLEPO252          
      WORK=WORK+H(KNI)*H(KN3)                                           FLEPO253          
C     S(I)=WORK                                                         FLEPO254          
      J=J+1                                                             FLEPO255          
      GOTO 1331                                                         FLEPO256          
1339  CONTINUE                                                          FLEPO257          
      KN2=I+N2                                                          FLEPO258          
      H(KN2)=WORK                                                       FLEPO259          
      K=0                                                               FLEPO260          
      WORK=0.0                                                          FLEPO261          
C     END UPDATING OF  S                                                FLEPO262          
      I=I+1                                                             FLEPO263          
      GOTO 1321                                                         FLEPO264          
1329  CONTINUE                                                          FLEPO265          
      GHG=0.0                                                           FLEPO266          
      I=1                                                               FLEPO267          
1491  IF(I.GT.N) GOTO 1499                                              FLEPO268          
C 214 GHG=GHG+S(I)*GAMMA(I)                                             FLEPO269          
      KN2=I+N2                                                          FLEPO270          
      KN3=I+N3                                                          FLEPO271          
      GHG=GHG+H(KN2)*H(KN3)                                             FLEPO272          
      I=I+1                                                             FLEPO273          
      GOTO 1491                                                         FLEPO274          
1499  CONTINUE                                                          FLEPO275          
      K=1                                                               FLEPO276          
C     EXIT FROM DIVISION BY ZERO                                        FLEPO277          
      IF(SG.EQ.0.0.OR.GHG.EQ.0.0) GOTO 1630                             FLEPO278          
C     UPDATING O                                                        FLEPO279          
      I=1                                                               FLEPO280          
1551  IF(I.GT.N) GOTO 1559                                              FLEPO281          
      J=I                                                               FLEPO282          
1561  IF(J.GT.N) GOTO 1569                                              FLEPO283          
C 215 H(K)=H(K)+SIGMA(I)*SIGMA(J)/SG-S(I)*S(J)/GHG                      FLEPO284          
      KN4=I+N4                                                          FLEPO285          
      JN4=J+N4                                                          FLEPO286          
      KN2=I+N2                                                          FLEPO287          
      JN2=J+N2                                                          FLEPO288          
      H(K)=H(K)+H(KN4)*H(JN4)/SG-H(KN2)*H(JN2)/GHG                      FLEPO289          
      K=K+1                                                             FLEPO290          
C     END UPDATING OF  H                                                FLEPO291          
C     INVESTIGATION OF THE CONDITION PREVIOUS F.GE. CURRENT F           FLEPO292          
      J=J+1                                                             FLEPO293          
      GOTO 1561                                                         FLEPO294          
1569  CONTINUE                                                          FLEPO295          
      I=I+1                                                             FLEPO296          
      GOTO 1551                                                         FLEPO297          
1559  CONTINUE                                                          FLEPO298          
1630  CONTINUE                                                          FLEPO299          
C     TEST PRINTOUT GOES HERE, IF REQUIRED                              FLEPO300          
      WRITE(61,1670) ICOUNT,F,(X(I),I=1,N)                              FLEPO301          
      M=N*(4+(N+1)/2)                                                   FLEPO302          
      WRITE(61,1680)(H(I),I=1,M)                                        FLEPO303          
1670  FORMAT(8H0ICOUNT=,1H ,I4,5X,3HF= ,E17.8,5X,3HX= ,4E17.8)          FLEPO304          
1680  FORMAT(4H H= ,6E15.6/(4X,6E15.6))                                 FLEPO305          
      WRITE(61,1700) GB                                                 FLEPO306          
1700  FORMAT(4H GB=,E20.8)                                              FLEPO307          
      IF(.NOT.(OLDF.GT.F)) GOTO 1740                                    FLEPO308          
1720  CONTINUE                                                          FLEPO309          
      ICOUNT=ICOUNT+1                                                   FLEPO310          
      GOTO 201                                                          FLEPO311          
209   CONTINUE                                                          FLEPO312          
1730  ICONV=0                                                           FLEPO313          
1740  LIMIT=IC-1                                                        FLEPO314          
      RETURN                                                            FLEPO315          
      END                                                               FLEPO316          
