      SUBROUTINE GENDIS(A,N,XL,XU,F,P1,P2,P3,P4,AREA)                   GENDI002          
      DIMENSION A(1)                                                    GENDI003          
C  A IS A ONE DIM. ARRAY HOLDING THE "N" VALUES OF THE RANDOM VARIABLE  GENDI004          
C  ON RETURN FROM THE SUBROUTINE                                        GENDI005          
C  XL  X LOWER /                                                        GENDI006          
C  XU  X UPPER   / DEFINE THE RANGE OF THE RANDOM VARIABLE X            GENDI007          
C  F   FUNCTION NAME OF USER SUPPLIED FUNCTION SUBPROGRAM WHICH         GENDI008          
C      DEFINES THE P.D.F. OF THE RANDOM VARIABLE, THIS MUST BE          GENDI009          
C      DECLARED EXTERNAL IN THE CALLING PROGRAM                         GENDI010          
C      FUNCTION REFERENCE IN GENDIS HAS THE FORM F(X, P1, P2, P3, P4)   GENDI011          
C  P=, P2, P3, P4 FOUR PARAMETERS OF THE FUNCTION SUBPROGRAM            GENDI012          
C  AREA IS SET TO 1.0 FOR A STANDARD P.D.F. OTHERWISE ACTION AS BELOW   GENDI013          
      PLIM=0.01                                                         GENDI014          
C  PLIM (PERCENTAGE LIMIT) IS USED AS THE ACCURACY PARAMETER IN THE     GENDI015          
C  ESTIMATION FOR THE EQUAL AREAS                                       GENDI016          
      IF(AREA.EQ.1.0) GOTO 140                                          GENDI017          
C  IF DEALING WITH A STANDARD P.D.F. OMIT THIS SECTION                  GENDI018          
      NUM=N*10                                                          GENDI019          
      AREA=0.0                                                          GENDI020          
      H=(XU-XL)/FLOAT(NUM)                                              GENDI021          
C  CALCULATE AREA UNDER CURVE BY SIMPLE LINEAR ESTIMATE                 GENDI022          
      I=1                                                               GENDI023          
81    IF(I.GT.NUM) GOTO 89                                              GENDI024          
      X=XL+(FLOAT(I)-0.5)*H                                             GENDI025          
      FX=F(X,P1,P2,P3,P4)                                               GENDI026          
      AREA=AREA+FX                                                      GENDI027          
      I=I+1                                                             GENDI028          
      GOTO 81                                                           GENDI029          
89    CONTINUE                                                          GENDI030          
      AREA=AREA*H                                                       GENDI031          
140   CONTINUE                                                          GENDI032          
      DAREA=AREA/FLOAT(N)                                               GENDI033          
C  DAREA IS THE AREA DEFINED FOR EACH EQUAL AREA SEGMENT                GENDI034          
      DR=(XU-XL)*0.01                                                   GENDI035          
      DRR=5.0*DR                                                        GENDI036          
      D7=DAREA*0.7                                                      GENDI037          
      A1=0.0                                                            GENDI038          
      X=XL                                                              GENDI039          
      FX=F(X,P1,P2,P3,P4)                                               GENDI040          
      AN=N                                                              GENDI041          
      I=1                                                               GENDI042          
231   IF(I.GT.N) GOTO 239                                               GENDI043          
      A2=FLOAT(I)*DAREA                                                 GENDI044          
      K=1                                                               GENDI045          
260   CONTINUE                                                          GENDI046          
      H=(0.1/FLOAT(N))/FX                                               GENDI047          
      IF(.NOT.(H.GT.DR)) GOTO 290                                       GENDI048          
      H=DR                                                              GENDI049          
290   CONTINUE                                                          GENDI050          
      IF(.NOT.(K.EQ.1)) GOTO 310                                        GENDI051          
      HX=H                                                              GENDI052          
310   CONTINUE                                                          GENDI053          
      IF(K.LT.20) GOTO 360                                              GENDI054          
      IF(.NOT.(A1-A2.GT.D7)) GOTO 350                                   GENDI055          
      H=DRR                                                             GENDI056          
350   CONTINUE                                                          GENDI057          
360   CONTINUE                                                          GENDI058          
      X=X+H                                                             GENDI059          
      FX=F(X,P1,P2,P3,P4)                                               GENDI060          
      A1=A1+H*FX                                                        GENDI061          
      K=K+1                                                             GENDI062          
      IF(A1.GT.A2) GOTO 470                                             GENDI063          
      IF(X.GT.XU) GOTO 470                                              GENDI064          
      IF(ABS(A1-A2)/DAREA.GT.PLIM) GOTO 260                             GENDI065          
470   CONTINUE                                                          GENDI066          
      A(I)=X-FLOAT(K)*(HX+H)*0.25                                       GENDI067          
      I=I+1                                                             GENDI068          
      GOTO 231                                                          GENDI069          
239   CONTINUE                                                          GENDI070          
      RETURN                                                            GENDI071          
      END                                                               GENDI072          
