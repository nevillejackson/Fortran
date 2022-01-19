      SUBROUTINE SYMIGEN(N,NM,A,X,D,IVEC)                               SYMEI002          
      DIMENSION A(NM,NM),X(NM,NM),D(NM),B(100)                          SYMEI003          
C EIGENVALUES AND EIGENVECTORS OF REAL SYMMETRIC MATRIX.                SYMEI004          
C  HOUSEHOLDER TRANSFORMATION OF A REAL SYMMETRIC MATRIX TO TRIDIAGONAL SYMEI005          
C  FOLLOWED BY QR ALGORITHM USING SEMI-IMPLICIT SHIFT OF ORIGIN.        SYMEI006          
C ALGORITHM BY J.BOOTHROYD, AUST. COMP. J, VOL.2, P55.                  SYMEI007          
C TRANSLATED FROM ALGOL TO FORTRAN BY N.PUMMEROY, MAY 1971.             SYMEI008          
C *N* IS ORDER OF MATRIX.                                               SYMEI009          
C *NM* IS DIMENSION OF ARRAY.                                           SYMEI010          
C *A* IS INPUT ARRAY.                                                   SYMEI011          
C *X* CONTAINS OUTPUT EIGEN VECTORS IF REQUIRED.                        SYMEI012          
C  THE ARRAY *A* IS LEFT INTACT UNLESS THE ACTUAL PARAMETER FOR *X*     SYMEI013          
C  IS THE SAME AS *A*.                                                  SYMEI014          
C *D* CONTAINS OUTPUT EIGENVALUES.                                      SYMEI015          
C *IVEC* IS NON-ZERO IF EIGENVECTORS REQUIRED.                          SYMEI016          
C     DIMENSION A(20,20),X(20,20),D(20)                                 SYMEI017          
      IF(.NOT.(IVEC.NE.0)) GOTO 50                                      SYMEI018          
      CALL TRIDIAG(N,NM,A,X,D,B,1)                                      SYMEI019          
      GOTO 60                                                           SYMEI020          
50    CALL TRIDIAG(N,NM,A,A,D,B,0)                                      SYMEI021          
60    CALL TREIGEN(N,NM,X,D,B,IVEC)                                     SYMEI022          
      RETURN                                                            SYMEI023          
      END                                                               SYMEI024          
      SUBROUTINE TRIDIAG(N,NM,A,X,D,B,IVEC)                             SYMEI025          
      DIMENSION A(NM,NM),X(NM,NM),D(NM),B(NM),U(100),P(100)             SYMEI026          
C *TRIDIAG* PERFORMS A HOUSEHOLDER TRANSFORMATION OF A REAL SYMMETRIC   SYMEI027          
C  MATRIX TO TRIDIAGONAL FORM. IF VECTORS ARE NOT REQUIRED (IVEC=0),    SYMEI028          
C  THE ACTUAL PARAMETER USED IN PLACE OF *A* MUST ALSO BE USED IN       SYMEI029          
C  PLACE OF *X*.                                                        SYMEI030          
C THE DIAGONAL OF THE TRIDIAGONAL MATRIX IS D(1) TO D(N) WITH           SYMEI031          
C  THE SUB-DIAGONAL IN B(2) TO B(N).                                    SYMEI032          
C     DIMENSION A(20,20),X(20,20),D(20),B(20),U(100),P(100)             SYMEI033          
C IF EIGEN VECTORS REQUIRED COPY *A* TO *X* OTHERWISE SAVE              SYMEI034          
C DIAGONAL TO PERMIT RESTORATION OF *A*.                                SYMEI035          
      IF(.NOT.(IVEC.NE.0)) GOTO 70                                      SYMEI036          
      IF(1.GT.N) GOTO 39                                                SYMEI037          
      DO 38 I=1,N                                                       SYMEI038          
      IF(1.GT.I) GOTO 49                                                SYMEI039          
      DO 48 J=1,I                                                       SYMEI040          
      X(I,J)=A(I,J)                                                     SYMEI041          
48    CONTINUE                                                          SYMEI042          
49    CONTINUE                                                          SYMEI043          
38    CONTINUE                                                          SYMEI044          
39    CONTINUE                                                          SYMEI045          
      GOTO 90                                                           SYMEI046          
70    IF(1.GT.N) GOTO 79                                                SYMEI047          
      DO 78 I=1,N                                                       SYMEI048          
      D(I)=X(I,I)                                                       SYMEI049          
78    CONTINUE                                                          SYMEI050          
79    CONTINUE                                                          SYMEI051          
90    NLESS1=N-1                                                        SYMEI052          
      TOL=1.0E-307                                                      SYMEI053          
C *TOL* SHOULD BE SET TO SMALLEST POSITIVE REAL NUMBER.                 SYMEI054          
      IF(1.GT.NLESS1) GOTO 119                                          SYMEI055          
      DO 118 I=1,NLESS1                                                 SYMEI056          
      I1=I+1                                                            SYMEI057          
      S=0.0                                                             SYMEI058          
      H=S                                                               SYMEI059          
      JLIM=I+2                                                          SYMEI060          
      IF(JLIM.GT.N) GOTO 159                                            SYMEI061          
      DO 158 J=JLIM,N                                                   SYMEI062          
      XIJ=X(J,I)                                                        SYMEI063          
      S=S+XIJ*XIJ                                                       SYMEI064          
158   CONTINUE                                                          SYMEI065          
159   CONTINUE                                                          SYMEI066          
      IF(.NOT.(S.GT.TOL)) GOTO 460                                      SYMEI067          
      XIJ=X(I1,I)                                                       SYMEI068          
      S=S+XIJ*XIJ                                                       SYMEI069          
      G=SQRT(S)                                                         SYMEI070          
      IF(.NOT.(XIJ.GE.0.0)) GOTO 230                                    SYMEI071          
      G=-G                                                              SYMEI072          
230   CONTINUE                                                          SYMEI073          
      H=S-XIJ*G                                                         SYMEI074          
      X(I1,I)=XIJ-G                                                     SYMEI075          
      B(I1)=G                                                           SYMEI076          
      IF(I1.GT.N) GOTO 279                                              SYMEI077          
      DO 278 J=I1,N                                                     SYMEI078          
      U(J)=X(J,I)                                                       SYMEI079          
278   CONTINUE                                                          SYMEI080          
279   CONTINUE                                                          SYMEI081          
      G=0.0                                                             SYMEI082          
      IF(I1.GT.N) GOTO 309                                              SYMEI083          
      DO 308 J=I1,N                                                     SYMEI084          
      S=0.0                                                             SYMEI085          
      IF(I1.GT.J) GOTO 329                                              SYMEI086          
      DO 328 K=I1,J                                                     SYMEI087          
      S=S+U(K)*X(J,K)                                                   SYMEI088          
328   CONTINUE                                                          SYMEI089          
329   CONTINUE                                                          SYMEI090          
      JLIM=J+1                                                          SYMEI091          
      IF(JLIM.GT.N) GOTO 359                                            SYMEI092          
      DO 358 K=JLIM,N                                                   SYMEI093          
      S=S+U(K)*X(K,J)                                                   SYMEI094          
358   CONTINUE                                                          SYMEI095          
359   CONTINUE                                                          SYMEI096          
      P(J)=S/H                                                          SYMEI097          
      PJ=P(J)                                                           SYMEI098          
      G=G+U(J)*PJ                                                       SYMEI099          
308   CONTINUE                                                          SYMEI100          
309   CONTINUE                                                          SYMEI101          
      G=G/(H+H)                                                         SYMEI102          
      IF(I1.GT.N) GOTO 409                                              SYMEI103          
      DO 408 J=I1,N                                                     SYMEI104          
      UJ=U(J)                                                           SYMEI105          
      P(J)=P(J)-G*UJ                                                    SYMEI106          
      PJ=P(J)                                                           SYMEI107          
      IF(I1.GT.J) GOTO 439                                              SYMEI108          
      DO 438 K=I1,J                                                     SYMEI109          
      X(J,K)=X(J,K)-UJ*P(K)-U(K)*PJ                                     SYMEI110          
438   CONTINUE                                                          SYMEI111          
439   CONTINUE                                                          SYMEI112          
408   CONTINUE                                                          SYMEI113          
409   CONTINUE                                                          SYMEI114          
      GOTO 470                                                          SYMEI115          
460   B(I1)=X(I1,I)                                                     SYMEI116          
470   U(I)=H                                                            SYMEI117          
C END REDUCTION TO TRIDIAGONAL FORM.                                    SYMEI118          
118   CONTINUE                                                          SYMEI119          
119   CONTINUE                                                          SYMEI120          
      IF(.NOT.(IVEC.NE.0)) GOTO 740                                     SYMEI121          
      D(N)=X(N,N)                                                       SYMEI122          
      X(N,N)=1.0                                                        SYMEI123          
      I1=N                                                              SYMEI124          
      I=NLESS1+1                                                        SYMEI125          
540   I=I-1                                                             SYMEI126          
      IF(I.LT.1) GOTO 810                                               SYMEI127          
      IF(.NOT.(U(I).NE.0)) GOTO 680                                     SYMEI128          
      IF(I1.GT.N) GOTO 589                                              SYMEI129          
      DO 588 J=I1,N                                                     SYMEI130          
      U(J)=X(J,I)                                                       SYMEI131          
588   CONTINUE                                                          SYMEI132          
589   CONTINUE                                                          SYMEI133          
      IF(I1.GT.N) GOTO 609                                              SYMEI134          
      DO 608 K=I1,N                                                     SYMEI135          
      S=0.0                                                             SYMEI136          
      IF(I1.GT.N) GOTO 629                                              SYMEI137          
      DO 628 J=I1,N                                                     SYMEI138          
      P(J)=X(J,K)                                                       SYMEI139          
      PJ=P(J)                                                           SYMEI140          
      S=S+U(J)*PJ                                                       SYMEI141          
628   CONTINUE                                                          SYMEI142          
629   CONTINUE                                                          SYMEI143          
      S=S/U(I)                                                          SYMEI144          
      IF(I1.GT.N) GOTO 669                                              SYMEI145          
      DO 668 J=I1,N                                                     SYMEI146          
      X(J,K)=P(J)-S*U(J)                                                SYMEI147          
668   CONTINUE                                                          SYMEI148          
669   CONTINUE                                                          SYMEI149          
608   CONTINUE                                                          SYMEI150          
609   CONTINUE                                                          SYMEI151          
680   IF(I1.GT.N) GOTO 689                                              SYMEI152          
      DO 688 J=I1,N                                                     SYMEI153          
      X(J,I)=0.0                                                        SYMEI154          
      X(I,J)=X(J,I)                                                     SYMEI155          
688   CONTINUE                                                          SYMEI156          
689   CONTINUE                                                          SYMEI157          
      D(I)=X(I,I)                                                       SYMEI158          
      X(I,I)=1.0                                                        SYMEI159          
      I1=I                                                              SYMEI160          
      GOTO 540                                                          SYMEI161          
C RESTORE ORIGINAL MATRIX.                                              SYMEI162          
740   IF(1.GT.N) GOTO 749                                               SYMEI163          
      DO 748 I=1,N                                                      SYMEI164          
      XIJ=X(I,I)                                                        SYMEI165          
      X(I,I)=D(I)                                                       SYMEI166          
      D(I)=XIJ                                                          SYMEI167          
      JLIM=I+1                                                          SYMEI168          
      IF(JLIM.GT.N) GOTO 799                                            SYMEI169          
      DO 798 J=JLIM,N                                                   SYMEI170          
      X(J,I)=X(I,J)                                                     SYMEI171          
798   CONTINUE                                                          SYMEI172          
799   CONTINUE                                                          SYMEI173          
748   CONTINUE                                                          SYMEI174          
749   CONTINUE                                                          SYMEI175          
810   CONTINUE                                                          SYMEI176          
      RETURN                                                            SYMEI177          
      END                                                               SYMEI178          
      SUBROUTINE TREIGEN(N,NM,T,D,B,IVEC)                               SYMEI179          
      INTEGER I,J,I1,M,EN,N,NLESS1,IVEC                                 SYMEI180          
      DIMENSION T(NM,NM),D(NM),B(NM)                                    SYMEI181          
C *TREIGEN* FINDS ALL THE EIGENVALUES AND IF *IVEC* IS NON-ZERO,        SYMEI182          
C  ALL THE EIGENVECTORS OF THE REAL SYMMETRIC TRIDIAGONAL MATRIX        SYMEI183          
C  HAVING D(1) TO D(N) AS MAIN DIAGONAL AND B(2) TO B(N) AS             SYMEI184          
C  SUB-DIAGONAL.                                                        SYMEI185          
C     DIMENSION T(20,20),D(20),B(20)                                    SYMEI186          
C *EN* CHANGES AS THE COMPUTATION PROCEEDS, N RETAINS ORIGINAL VALUE.   SYMEI187          
C *EPS* IS RELATIVE MACHINE PRECISION.                                  SYMEI188          
      EN=N                                                              SYMEI189          
      EPS=1.0E-10                                                       SYMEI190          
C DETECT NEGLIGIBLE OFF-DIAGONAL ELEMENTS.                              SYMEI191          
40    NLESS1=EN-1                                                       SYMEI192          
      M=1                                                               SYMEI193          
      I1=EN                                                             SYMEI194          
      DI1=ABS(D(EN))                                                    SYMEI195          
      I=NLESS1+1                                                        SYMEI196          
90    I=I-1                                                             SYMEI197          
      IF(I.LT.1) GOTO 210                                               SYMEI198          
      DI=ABS(D(I))                                                      SYMEI199          
      IF(.NOT.(DI.LT.DI1)) GOTO 140                                     SYMEI200          
      DI1=DI                                                            SYMEI201          
140   CONTINUE                                                          SYMEI202          
      IF(.NOT.(ABS(B(I1)).LE.EPS*DI1)) GOTO 180                         SYMEI203          
      M=I1                                                              SYMEI204          
      GOTO 210                                                          SYMEI205          
180   I1=I                                                              SYMEI206          
      DI1=DI                                                            SYMEI207          
      GOTO 90                                                           SYMEI208          
C TEST IF A SINGLE ROOT HAS BEEN ISOLATED.                              SYMEI209          
210   IF(.NOT.(M.EQ.EN)) GOTO 240                                       SYMEI210          
      EN=NLESS1                                                         SYMEI211          
      GOTO 890                                                          SYMEI212          
240   DN1=D(NLESS1)                                                     SYMEI213          
      DN=D(EN)                                                          SYMEI214          
      U=B(EN)                                                           SYMEI215          
      V=0.5*(DN1-DN)/U                                                  SYMEI216          
      S=SQRT(1.0+V*V)                                                   SYMEI217          
      S=U/(V+S*SIGN(1.0,V))                                             SYMEI218          
C TEST IF SUB-MATRIX OF ORDER 2 HAS BEEN ISOLATED. IF SO,               SYMEI219          
C REDUCE IT TO DIAGONAL FORM WITH ONE JACOBI ROTATION.                  SYMEI220          
      IF(.NOT.(M.EQ.NLESS1)) GOTO 440                                   SYMEI221          
      D(EN)=DN-S                                                        SYMEI222          
      D(M)=DN1+S                                                        SYMEI223          
      IF(.NOT.(IVEC.NE.0)) GOTO 420                                     SYMEI224          
      X=S/U                                                             SYMEI225          
      C=1.0/SQRT(1.0+X*X)                                               SYMEI226          
      S=X*C                                                             SYMEI227          
      IF(1.GT.N) GOTO 379                                               SYMEI228          
      DO 378 J=1,N                                                      SYMEI229          
      U=T(J,M)                                                          SYMEI230          
      V=T(J,EN)                                                         SYMEI231          
      T(J,M)=C*U+S*V                                                    SYMEI232          
      T(J,EN)=S*U-C*V                                                   SYMEI233          
378   CONTINUE                                                          SYMEI234          
379   CONTINUE                                                          SYMEI235          
420   EN=EN-2                                                           SYMEI236          
      GOTO 880                                                          SYMEI237          
C CHOOSE ORIGIN SHIFT.                                                  SYMEI238          
440   SHIFT=DN-S                                                        SYMEI239          
      IF(.NOT.(V.EQ.0E0)) GOTO 490                                      SYMEI240          
      V=DN1+S                                                           SYMEI241          
      IF(.NOT.(ABS(V).LT.ABS(SHIFT))) GOTO 480                          SYMEI242          
      SHIFT=V                                                           SYMEI243          
C QR ITERATION WITH SEMI-IMPLICIT ORIGIN SHIFT                          SYMEI244          
C ON TRI-DIAGONAL SUB-MATRIX EXTENDING FROM ROW M TO ROW N.             SYMEI245          
480   CONTINUE                                                          SYMEI246          
490   C=-1.0                                                            SYMEI247          
      V=0.0                                                             SYMEI248          
      S=V                                                               SYMEI249          
      DI1=D(M)                                                          SYMEI250          
      I1=M                                                              SYMEI251          
      JLIM=M+1                                                          SYMEI252          
      IF(JLIM.GT.EN) GOTO 549                                           SYMEI253          
      DO 548 I=JLIM,EN                                                  SYMEI254          
      BI=B(I)                                                           SYMEI255          
      W=-C*BI                                                           SYMEI256          
      Y=-C*DI1                                                          SYMEI257          
      DI1=DI1+C*SHIFT                                                   SYMEI258          
      IF(.NOT.(ABS(DI1).GE.ABS(BI))) GOTO 660                           SYMEI259          
      R=BI/DI1                                                          SYMEI260          
      X=SQRT(1.0+R*R)                                                   SYMEI261          
      B(I1)=S*DI1*X                                                     SYMEI262          
      C=1.0/X                                                           SYMEI263          
      S=R*C                                                             SYMEI264          
      GOTO 710                                                          SYMEI265          
660   R=DI1/BI                                                          SYMEI266          
      X=SQRT(1.0+R*R)                                                   SYMEI267          
      B(I1)=S*BI*X                                                      SYMEI268          
      S=1.0/X                                                           SYMEI269          
      C=R*S                                                             SYMEI270          
710   DI=D(I)                                                           SYMEI271          
      W=S*W                                                             SYMEI272          
      DI1=W-C*DI                                                        SYMEI273          
      U=V                                                               SYMEI274          
      V=S*S                                                             SYMEI275          
      D(I1)=(C*W+V*DI+Y)+(U-V)*SHIFT                                    SYMEI276          
      IF(.NOT.(IVEC.NE.0)) GOTO 830                                     SYMEI277          
      IF(1.GT.N) GOTO 789                                               SYMEI278          
      DO 788 J=1,N                                                      SYMEI279          
      X=T(J,I1)                                                         SYMEI280          
      Y=T(J,I)                                                          SYMEI281          
      T(J,I1)=C*X+S*Y                                                   SYMEI282          
      T(J,I)=S*X-C*Y                                                    SYMEI283          
788   CONTINUE                                                          SYMEI284          
789   CONTINUE                                                          SYMEI285          
830   I1=I                                                              SYMEI286          
548   CONTINUE                                                          SYMEI287          
549   CONTINUE                                                          SYMEI288          
      SHIFT=S*SHIFT                                                     SYMEI289          
      B(EN)=S*DI1+C*SHIFT                                               SYMEI290          
      D(EN)=-C*DI1+S*SHIFT                                              SYMEI291          
880   CONTINUE                                                          SYMEI292          
C END QR ITERATION.                                                     SYMEI293          
890   IF(EN.GT.1) GOTO 40                                               SYMEI294          
      RETURN                                                            SYMEI295          
      END                                                               SYMEI296          
