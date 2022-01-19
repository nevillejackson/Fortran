      SUBROUTINE CUBIC(A0,A1,A2,A3,IT,ROOT1,ROOT2,ROOT3,IRR)            CUBIC002          
C         USING CARDANS METHOD THIS SUBROUTINE DETERMINES THE THREE     CUBIC003          
C     ROOTS OF  A0.X**3+A1.X**2+A2.X+A3=0. IF A0.0 THE FLAG IRR IS SET  CUBIC004          
C     EQUAL TO ZERO,OTHERWISE IRR=1. IF CUBIC HAS ONLY ONE REAL ROOT,   CUBIC005          
C     ROOT1,THE FLAG IT IS SET EQUAL TO 1 AND ROOT2,ROOT3 TAKE THE      CUBIC006          
C     VALUES OF THE REAL AND IMAGINARY PARTS OF THE TWO COMPLEX         CUBIC007          
C     CONJUGATE ROOTS.IF CUBIC HAS THREE REAL ROOTS,ROOT1,ROOT2,ROOT3   CUBIC008          
C     IT TAKES THE VALUE 3.                                             CUBIC009          
C                                                                       CUBIC010          
      IF(.NOT.(ABS(A0).LT.1.0E-90)) GOTO 50                             CUBIC011          
      IRR=0                                                             CUBIC012          
      GOTO 580                                                          CUBIC013          
50    IRR=1                                                             CUBIC014          
      B1=A1/A0                                                          CUBIC015          
      B2=A2/A0                                                          CUBIC016          
      B3=A3/A0                                                          CUBIC017          
C         REMOVE QUADRATIC TERM                                         CUBIC018          
      ALPHA=-B1/3.                                                      CUBIC019          
      Q=B2+ALPHA*(2.*B1+3.*ALPHA)                                       CUBIC020          
      R=B3+ALPHA*(B2+ALPHA*(B1+ALPHA))                                  CUBIC021          
      Q=-Q                                                              CUBIC022          
      R=-R                                                              CUBIC023          
      IF(.NOT.(ABS(Q).LE.1.0E-50)) GOTO 250                             CUBIC024          
      IT=1                                                              CUBIC025          
      IF(ABS(R).LE.1.0E-9) GOTO 200                                     CUBIC026          
      IF(R) 180,200,230                                                 CUBIC027          
180   ROOT1=-R**0.33333333                                              CUBIC028          
      GOTO 420                                                          CUBIC029          
200   IT=3                                                              CUBIC030          
      ROOT3=ALPHA                                                       CUBIC031          
      ROOT2=ROOT3                                                       CUBIC032          
      ROOT1=ROOT2                                                       CUBIC033          
      GOTO 580                                                          CUBIC034          
230   ROOT1=R**0.33333333                                               CUBIC035          
      GOTO 420                                                          CUBIC036          
250   CONTINUE                                                          CUBIC037          
      DELTA=0.25*R*R-Q*Q*Q/27.                                          CUBIC038          
      IF(DELTA) 490,490,280                                             CUBIC039          
280   CONTINUE                                                          CUBIC040          
C         CUBIC HAS ONE REAL ROOT AND TWO COMPLEX ROOTS                 CUBIC041          
      IT=1                                                              CUBIC042          
      EPS2=1.                                                           CUBIC043          
      EPS1=EPS2                                                         CUBIC044          
      DELTA=SQRT(DELTA)                                                 CUBIC045          
      X1=0.5*R+DELTA                                                    CUBIC046          
      IF(.NOT.(X1.LT.0.)) GOTO 360                                      CUBIC047          
      EPS1=-1.                                                          CUBIC048          
      X1=-X1                                                            CUBIC049          
360   X2=0.5*R-DELTA                                                    CUBIC050          
      IF(.NOT.(X2.LT.0.)) GOTO 400                                      CUBIC051          
      EPS2=-1.                                                          CUBIC052          
      X2=-X2                                                            CUBIC053          
400   CONTINUE                                                          CUBIC054          
      ROOT1=EPS1*X1**0.33333333+EPS2*X2**0.33333333                     CUBIC055          
C         EVALUATE COMPLEX ROOTS                                        CUBIC056          
420   ROOT1=ROOT1+ALPHA                                                 CUBIC057          
      X1=-0.5*(B1+ROOT1)                                                CUBIC058          
      X2=B2+ROOT1*B1+ROOT1*ROOT1                                        CUBIC059          
      X2=SQRT(X2-X1*X1)                                                 CUBIC060          
      ROOT2=X1                                                          CUBIC061          
      ROOT3=X2                                                          CUBIC062          
      GOTO 580                                                          CUBIC063          
C         EVALUATE REAL ROOTS                                           CUBIC064          
490   IT=3                                                              CUBIC065          
      PHI=ACOS(2.59807622*R/Q**1.5)                                     CUBIC066          
      PHI=PHI/3.                                                        CUBIC067          
      RM=1.1547006*SQRT(Q)                                              CUBIC068          
      PI3=1.04719754                                                    CUBIC069          
      ROOT1=RM*COS(PHI)+ALPHA                                           CUBIC070          
      ROOT2=-RM*COS(PI3-PHI)+ALPHA                                      CUBIC071          
      ROOT3=-RM*COS(PI3+PHI)+ALPHA                                      CUBIC072          
580   RETURN                                                            CUBIC073          
      END                                                               CUBIC074          
