      SUBROUTINE NEWTON(X,N,C,KM,VOUT,MIT,Y)                            NEWTO002          
      DIMENSION X(1),Y(1),L(30),M(30)                                   NEWTO003          
C   D.P.MCKEE AND K.SERKOWSKA,DIV.OF COMP.RES.,CANBERRA                 NEWTO004          
C  SUBROUTINE NEWTON(X,N,C,KM,VOUT,MIT,A,Y,F,G)                         NEWTO005          
C  DIMENSION A(N,N),X(N),Y(N),F(N),G(N)                                 NEWTO006          
C  TO SOLVE SIMULTANEOUS NONLINEAR EQUATIONS.                           NEWTO007          
C  X  CONTAINS ON ENTRY THE INITIAL APPROXIMATION AND ON EXIT           NEWTO008          
C  THE SOLUTION FOUND.                                                  NEWTO009          
C  N  IS THE NUMBER OF EQUATIONS.                                       NEWTO010          
C  C THE ACCURACY REQUIRED (VALUE OF EUCLIDEAN NORM OF THE FUNCTION F)  NEWTO011          
C  KM  CONTAINS ON ENTRY THE MAXIMUM PERMITTED NUMBER OF ITERATIONS     NEWTO012          
C  AND ON EXIT THE NUMBER OF ITERATIONS PERFORMED.                      NEWTO013          
C  VOUT  CONTAINS ON EXIT THE EUCLIDEAN NORM OF THE FUNCTION AT X.      NEWTO014          
C  MIT  IS ON RETURN:                                                   NEWTO015          
C   ZERO IF THE SPECIFIED ACCURACY HAS BEEN ACHIEVED,                   NEWTO016          
C   -1   IF THE LIMIT KM HAS BEEN PASSED,                               NEWTO017          
C   +1   IF THE FUNCTION DIVERGED RAPIDLY.                              NEWTO018          
C  THE SUBROUTINE GIVEF(X,F),WHICH SPECIFIES THE EQUATIONS TO BE SOLVED,NEWTO019          
C  MUST BE PROVIDED BY THE USER.                                        NEWTO020          
C  WORKING SPACE ALLOCATED IN THE CALLING PROGRAM IS IN FOUR DISTINCT   NEWTO021          
C  REGIONS,ALL CONTAINED IN THE FORMAL PARAMETER Y.                     NEWTO022          
C  THESE DISTINCT PARTS ARE SHOWN IN THE COMMENTS THAT ACCOMPANY        NEWTO023          
C  REFERENCES TO Y.                                                     NEWTO024          
C  TO USE VARIABLE DIMENSIONS WITH 3200 COMPATIBILITY,ALL INDEXES ARE   NEWTO025          
C  SINGLE. THE DOUBLE INDEX FORM IS SHOWN AS AN ACCOMPANYING COMMENT.   NEWTO026          
C  THE NUMBERS OF VARIABLES IS LIMITED TO 30 ONLY BY THE DIMENSION      NEWTO027          
C  OF L AND M,ABOVE.                                                    NEWTO028          
      N1=N                                                              NEWTO029          
      N2=N1+N                                                           NEWTO030          
      N3=N2+N                                                           NEWTO031          
      K11=N1+1                                                          NEWTO032          
C     CALL GIVEF(X,F)                                                   NEWTO033          
      CALL GIVEF(X,Y(K11))                                              NEWTO034          
      VOUT=0.                                                           NEWTO035          
      I=1                                                               NEWTO036          
81    IF(I.GT.N) GOTO 89                                                NEWTO037          
      K1I=N1+I                                                          NEWTO038          
C   1 VOUT=VOUT+F(I)**2                                                 NEWTO039          
      VOUT=VOUT+Y(K1I)**2                                               NEWTO040          
      I=I+1                                                             NEWTO041          
      GOTO 81                                                           NEWTO042          
89    CONTINUE                                                          NEWTO043          
      VOUT=SQRT(VOUT)                                                   NEWTO044          
      FI=VOUT*1.E8                                                      NEWTO045          
      KMK=KM                                                            NEWTO046          
      IJ=1                                                              NEWTO047          
141   IF(IJ.GT.KMK) GOTO 149                                            NEWTO048          
C  THE FOLLOWING NUMERICAL CALC. OF THE JACOBIAN MAY BE OMITTED IF THE  NEWTO049          
C  USER PROVIDES ?SUBROUTINE DERIV? AND USES THE CALL SHOWN AHEAD       NEWTO050          
C  CALCULATE A, THE JACOBIAN :  A(I,J)=DF(I)/DX(J)                      NEWTO051          
      J=1                                                               NEWTO052          
151   IF(J.GT.N) GOTO 159                                               NEWTO053          
      I=1                                                               NEWTO054          
161   IF(I.GT.N) GOTO 169                                               NEWTO055          
      Y(I)=X(I)                                                         NEWTO056          
      I=I+1                                                             NEWTO057          
      GOTO 161                                                          NEWTO058          
169   CONTINUE                                                          NEWTO059          
      ABCD=X(J)*1.E-6+1.E-8                                             NEWTO060          
      Y(J)=X(J)+ABCD                                                    NEWTO061          
      K21=N2+1                                                          NEWTO062          
C     CALL GIVEF(Y,G)                                                   NEWTO063          
      CALL GIVEF(Y,Y(K21))                                              NEWTO064          
      K3J=N3+N*(J-1)                                                    NEWTO065          
      I=1                                                               NEWTO066          
231   IF(I.GT.N) GOTO 239                                               NEWTO067          
      K3J=K3J+1                                                         NEWTO068          
      K2I=N2+I                                                          NEWTO069          
      K1I=N1+I                                                          NEWTO070          
      Y(K3J)=(Y(K2I)-Y(K1I))/ABCD                                       NEWTO071          
C   3 A(I,J)=(G(I)-F(I))/ABCD                                           NEWTO072          
C  END OF JACOBIAN CALCULATION                                          NEWTO073          
C  IF DERIVATIVES ARE SUPPLIED BY THE USER,MAKE THE FOLLOWING CALL.     NEWTO074          
C     CALL DERIV(X,Y(N3+1))                                             NEWTO075          
C  INVERT A.                                                            NEWTO076          
      I=I+1                                                             NEWTO077          
      GOTO 231                                                          NEWTO078          
239   CONTINUE                                                          NEWTO079          
      J=J+1                                                             NEWTO080          
      GOTO 151                                                          NEWTO081          
159   CONTINUE                                                          NEWTO082          
      D=1.                                                              NEWTO083          
      K=1                                                               NEWTO084          
291   IF(K.GT.N) GOTO 299                                               NEWTO085          
      L(K)=K                                                            NEWTO086          
      M(K)=K                                                            NEWTO087          
      K3K=N3+K+N*(K-1)                                                  NEWTO088          
C     AMAX=A(K,K)                                                       NEWTO089          
      AMAX=Y(K3K)                                                       NEWTO090          
      I=K                                                               NEWTO091          
341   IF(I.GT.N) GOTO 349                                               NEWTO092          
      K3J=N3+I+(K-2)*N                                                  NEWTO093          
      J=K                                                               NEWTO094          
361   IF(J.GT.N) GOTO 369                                               NEWTO095          
      K3J=K3J+N                                                         NEWTO096          
C     IF(ABS(AMAX).LT.ABS(A(I,J)))300,301                               NEWTO097          
      IF(.NOT.(ABS(AMAX).LT.ABS(Y(K3J)))) GOTO 420                      NEWTO098          
C 300 AMAX=A(I,J)                                                       NEWTO099          
      AMAX=Y(K3J)                                                       NEWTO100          
      L(K)=I                                                            NEWTO101          
      M(K)=J                                                            NEWTO102          
420   CONTINUE                                                          NEWTO103          
      J=J+1                                                             NEWTO104          
      GOTO 361                                                          NEWTO105          
369   CONTINUE                                                          NEWTO106          
      I=I+1                                                             NEWTO107          
      GOTO 341                                                          NEWTO108          
349   CONTINUE                                                          NEWTO109          
      J=L(K)                                                            NEWTO110          
      IF(.NOT.(J.GT.K)) GOTO 530                                        NEWTO111          
      N3NI1=N3-N                                                        NEWTO112          
      I=1                                                               NEWTO113          
461   IF(I.GT.N) GOTO 469                                               NEWTO114          
      N3NI1=N3NI1+N                                                     NEWTO115          
      K3IK=N3NI1+K                                                      NEWTO116          
C     TT=-A(K,I)                                                        NEWTO117          
      TT=-Y(K3IK)                                                       NEWTO118          
      K3IJ=N3NI1+J                                                      NEWTO119          
C     A(K,I)=A(J,I)                                                     NEWTO120          
      Y(K3IK)=Y(K3IJ)                                                   NEWTO121          
C 303 A(J,I)=TT                                                         NEWTO122          
      Y(K3IJ)=TT                                                        NEWTO123          
      I=I+1                                                             NEWTO124          
      GOTO 461                                                          NEWTO125          
469   CONTINUE                                                          NEWTO126          
530   I=M(K)                                                            NEWTO127          
      IF(.NOT.(I.GT.K)) GOTO 630                                        NEWTO128          
      K3KK=K3K-K                                                        NEWTO129          
      K3I=N3+N*(I-1)                                                    NEWTO130          
      J=1                                                               NEWTO131          
571   IF(J.GT.N) GOTO 579                                               NEWTO132          
      K3KK=K3KK+1                                                       NEWTO133          
      K3I=K3I+1                                                         NEWTO134          
C     TT=-A(J,K)                                                        NEWTO135          
      TT=-Y(K3KK)                                                       NEWTO136          
C     A(J,K)=A(J,I)                                                     NEWTO137          
      Y(K3KK)=Y(K3I)                                                    NEWTO138          
C 306 A(J,I)=TT                                                         NEWTO139          
      Y(K3I)=TT                                                         NEWTO140          
      J=J+1                                                             NEWTO141          
      GOTO 571                                                          NEWTO142          
579   CONTINUE                                                          NEWTO143          
630   K3KK=K3K-K                                                        NEWTO144          
      I=1                                                               NEWTO145          
641   IF(I.GT.N) GOTO 649                                               NEWTO146          
      K3KK=K3KK+1                                                       NEWTO147          
      IF(.NOT.(I.NE.K)) GOTO 680                                        NEWTO148          
C 308 A(I,K)=-A(I,K)/A(K,K)                                             NEWTO149          
      Y(K3KK)=-Y(K3KK)/Y(K3K)                                           NEWTO150          
680   CONTINUE                                                          NEWTO151          
      I=I+1                                                             NEWTO152          
      GOTO 641                                                          NEWTO153          
649   CONTINUE                                                          NEWTO154          
      K3KK=K3K-K                                                        NEWTO155          
      I=1                                                               NEWTO156          
701   IF(I.GT.N) GOTO 709                                               NEWTO157          
      K3KK=K3KK+1                                                       NEWTO158          
      K3J=N3+I-N                                                        NEWTO159          
      K3JK=N3+K-N                                                       NEWTO160          
      J=1                                                               NEWTO161          
741   IF(J.GT.N) GOTO 749                                               NEWTO162          
      K3J=K3J+N                                                         NEWTO163          
      K3JK=K3JK+N                                                       NEWTO164          
      IF(.NOT.(I.NE.K)) GOTO 800                                        NEWTO165          
      IF(.NOT.(J.NE.K)) GOTO 800                                        NEWTO166          
C 311 A(I,J)=A(I,K)*A(K,J)+A(I,J)                                       NEWTO167          
      Y(K3J)=Y(K3KK)*Y(K3JK)+Y(K3J)                                     NEWTO168          
800   CONTINUE                                                          NEWTO169          
      J=J+1                                                             NEWTO170          
      GOTO 741                                                          NEWTO171          
749   CONTINUE                                                          NEWTO172          
      I=I+1                                                             NEWTO173          
      GOTO 701                                                          NEWTO174          
709   CONTINUE                                                          NEWTO175          
      K3J=K3K-N*K                                                       NEWTO176          
      J=1                                                               NEWTO177          
821   IF(J.GT.N) GOTO 829                                               NEWTO178          
      K3J=K3J+N                                                         NEWTO179          
      IF(.NOT.(J.NE.K)) GOTO 860                                        NEWTO180          
C 313 A(K,J)=A(K,J)/A(K,K)                                              NEWTO181          
      Y(K3J)=Y(K3J)/Y(K3K)                                              NEWTO182          
860   CONTINUE                                                          NEWTO183          
C     D=D*A(K,K)                                                        NEWTO184          
      J=J+1                                                             NEWTO185          
      GOTO 821                                                          NEWTO186          
829   CONTINUE                                                          NEWTO187          
      D=D*Y(K3K)                                                        NEWTO188          
C 315 A(K,K)=1./A(K,K)                                                  NEWTO189          
      Y(K3K)=1./Y(K3K)                                                  NEWTO190          
      K=K+1                                                             NEWTO191          
      GOTO 291                                                          NEWTO192          
299   CONTINUE                                                          NEWTO193          
      K=N                                                               NEWTO194          
900   K=K-1                                                             NEWTO195          
      IF(.NOT.(K.GT.0)) GOTO 1130                                       NEWTO196          
      I=L(K)                                                            NEWTO197          
      IF(.NOT.(I.GT.K)) GOTO 1020                                       NEWTO198          
      K3K=N3+N*(K-1)                                                    NEWTO199          
      K3I=N3+N*(I-1)                                                    NEWTO200          
      J=1                                                               NEWTO201          
961   IF(J.GT.N) GOTO 969                                               NEWTO202          
      K3K=K3K+1                                                         NEWTO203          
C     TT=A(J,K)                                                         NEWTO204          
      TT=Y(K3K)                                                         NEWTO205          
      K3I=K3I+1                                                         NEWTO206          
C     A(J,K)=-A(J,I)                                                    NEWTO207          
      Y(K3K)=-Y(K3I)                                                    NEWTO208          
C 319 A(J,I)=TT                                                         NEWTO209          
      Y(K3I)=TT                                                         NEWTO210          
      J=J+1                                                             NEWTO211          
      GOTO 961                                                          NEWTO212          
969   CONTINUE                                                          NEWTO213          
1020  J=M(K)                                                            NEWTO214          
      IF(J.LE.K) GOTO 900                                               NEWTO215          
      K3I=N3+K-N                                                        NEWTO216          
      K3IJ=N3+J-N                                                       NEWTO217          
      I=1                                                               NEWTO218          
1061  IF(I.GT.N) GOTO 1069                                              NEWTO219          
      K3I=K3I+N                                                         NEWTO220          
C     TT=A(K,I)                                                         NEWTO221          
      TT=Y(K3I)                                                         NEWTO222          
      K3IJ=K3IJ+N                                                       NEWTO223          
C     A(K,I)=-A(J,I)                                                    NEWTO224          
      Y(K3I)=-Y(K3IJ)                                                   NEWTO225          
C 322 A(J,I)=TT                                                         NEWTO226          
C     K3I=K3IJ+N                                                        NEWTO227          
C 322 Y(K3I)=TT                                                         NEWTO228          
      Y(K3IJ)=TT                                                        NEWTO229          
      I=I+1                                                             NEWTO230          
      GOTO 1061                                                         NEWTO231          
1069  CONTINUE                                                          NEWTO232          
      GOTO 900                                                          NEWTO233          
C  HERE ENDS INVERT.                                                    NEWTO234          
C  CALCULATE THE NEW X.                                                 NEWTO235          
1130  I=1                                                               NEWTO236          
1131  IF(I.GT.N) GOTO 1139                                              NEWTO237          
      B=0.                                                              NEWTO238          
      K3J=N3+I-N                                                        NEWTO239          
      J=1                                                               NEWTO240          
1161  IF(J.GT.N) GOTO 1169                                              NEWTO241          
      K3J=K3J+N                                                         NEWTO242          
      K1J=N1+J                                                          NEWTO243          
C   4 B=B+A(I,J)*F(J)                                                   NEWTO244          
      B=B+Y(K3J)*Y(K1J)                                                 NEWTO245          
      J=J+1                                                             NEWTO246          
      GOTO 1161                                                         NEWTO247          
1169  CONTINUE                                                          NEWTO248          
      X(I)=X(I)-B                                                       NEWTO249          
      I=I+1                                                             NEWTO250          
      GOTO 1131                                                         NEWTO251          
1139  CONTINUE                                                          NEWTO252          
      K11=N1+1                                                          NEWTO253          
C     CALL GIVEF(X,F)                                                   NEWTO254          
      CALL GIVEF(X,Y(K11))                                              NEWTO255          
      VOUT=0.                                                           NEWTO256          
      I=1                                                               NEWTO257          
1241  IF(I.GT.N) GOTO 1249                                              NEWTO258          
      K1I=N1+I                                                          NEWTO259          
C   6 VOUT=VOUT+F(I)**2                                                 NEWTO260          
      VOUT=VOUT+Y(K1I)**2                                               NEWTO261          
      I=I+1                                                             NEWTO262          
      GOTO 1241                                                         NEWTO263          
1249  CONTINUE                                                          NEWTO264          
      VOUT=SQRT(VOUT)                                                   NEWTO265          
      KM=IJ                                                             NEWTO266          
      IF(VOUT.LT.C) GOTO 1340                                           NEWTO267          
      IF(VOUT.GT.FI) GOTO 1360                                          NEWTO268          
1310  CONTINUE                                                          NEWTO269          
      IJ=IJ+1                                                           NEWTO270          
      GOTO 141                                                          NEWTO271          
149   CONTINUE                                                          NEWTO272          
      MIT=-1                                                            NEWTO273          
      GOTO 1370                                                         NEWTO274          
1340  MIT=0                                                             NEWTO275          
      GOTO 1370                                                         NEWTO276          
1360  MIT=1                                                             NEWTO277          
1370  RETURN                                                            NEWTO278          
      END                                                               NEWTO279          
