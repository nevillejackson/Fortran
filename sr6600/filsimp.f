      FUNCTION FILSIMP(X,FUNCT,TRIG,AA,BB,N)                            FILSI002          
      INTEGER TRIG,S                                                    FILSI003          
      DIMENSION SUMI(2)                                                 FILSI004          
C                                                                       FILSI005          
C         FILON"S METHOD FOR THE NUMERICAL EVALUATION OF FINITE RANGE   FILSI006          
C     TRIGONOMETRIC INTEGRALS WITH INTEGRANDS OF THE TYPE               FILSI007          
C     FUNCTION(Y).COS/SIN(X.Y).  A AND B ARE THE LOWER AND UPPER LIMITS FILSI008          
C     OF INTEGRATION,RESPECTIVELY AND H=(B-A)/2.N IS THE INTEGRATION STEFILSI009          
C     IF THE INTEGER TRIG IS EQUAL TO UNITY THE COSINE INTEGRAL IS      FILSI010          
C     PRODUCED ELSE THE SINE INTEGRAL IS PRODUCED. THE APPROPRIATE      FILSI011          
C     INTEGRAND FUNCTION FUNCT IS TO BE REFERRED TO IN AN EXTERNAL      FILSI012          
C     STATEMENT IN THE CALLING PROGRAM.                                 FILSI013          
C     REFERENCE  L.N.G.FILON,PROC.ROY.SOC.EDIN. XLIX,(1928-1929),PP38-47FILSI014          
C                                                                       FILSI015          
      OVER=1.0                                                          FILSI016          
      A=AA                                                              FILSI017          
      B=BB                                                              FILSI018          
      IF(.NOT.(B.LT.A)) GOTO 90                                         FILSI019          
      OVER=-1.0                                                         FILSI020          
      A=BB                                                              FILSI021          
      B=AA                                                              FILSI022          
90    IF(.NOT.(N.LE.0)) GOTO 140                                        FILSI023          
      WRITE(61,110) N                                                   FILSI024          
110   FORMAT(8H ERROR I,8HN CALL T,8HO FILSIM,8HP. NO. O,8HF STEPS ,    FILSI025          
     .2H= ,I10)                                                         FILSI026          
      FILSIMP=0.0                                                       FILSI027          
      GOTO 470                                                          FILSI028          
C                                                                       FILSI029          
C     CALCULATE CONSTANTS                                               FILSI030          
C                                                                       FILSI031          
140   YSI=0.0                                                           FILSI032          
      H=(B-A)/(2.0*FLOAT(N))                                            FILSI033          
      THETA=X*H                                                         FILSI034          
      THETA2=THETA*THETA                                                FILSI035          
      THETA3=THETA2*THETA                                               FILSI036          
      IF(.NOT.(THETA.LT.1.E-1)) GOTO 280                                FILSI037          
      THETA4=THETA3*THETA                                               FILSI038          
      THETA5=THETA4*THETA                                               FILSI039          
      THETA6=THETA5*THETA                                               FILSI040          
      THETA7=THETA6*THETA                                               FILSI041          
      ALPHA=4.4444444444E-2*THETA3-6.3492063492E-3*THETA5+              FILSI042          
     .4.2328042328E-4*THETA7                                            FILSI043          
      BETA=6.6666666666E-1+1.3333333333E-1*THETA2-3.8095238059E-2*      FILSI044          
     .THETA4+3.5273368607E-3*THETA6                                     FILSI045          
      GAMMA=1.3333333333E0-1.3333333333E-1*THETA2+4.7619047619E-3*      FILSI046          
     .THETA4-8.8183421517E-5*THETA6                                     FILSI047          
C                                                                       FILSI048          
C     NOTE THAT:                                                        FILSI049          
C     ALPHA=(2./45.)*THETA3-(2./315.)*THETA5+(2./4725.)*THETA7          FILSI050          
C     BETA=(2./3.)+(2./15.)*THETA2-(4./105.)*THETA4+                    FILSI051          
C    *     (2./567.)*THETA6                                             FILSI052          
C     GAMMA=(4./3.)-(2./15.)*THETA2+(1./210.)*THETA4                    FILSI053          
C    *-(1./11340.)*THETA6                                               FILSI054          
C                                                                       FILSI055          
      GOTO 330                                                          FILSI056          
280   SN=SIN(THETA) $ CS=COS(THETA)                                     FILSI057          
      ALPHA=(1./THETA3)*(THETA2+THETA*SN*CS-2.*SN*SN)                   FILSI058          
      BETA=(2./THETA3)*(THETA*(1.+CS*CS)-2.*SN*CS)                      FILSI059          
      GAMMA=(4./THETA3)*(SN-THETA*CS)                                   FILSI060          
330   IF(TRIG.EQ.1) GOTO 350                                            FILSI061          
      YSI=-1.5707963268                                                 FILSI062          
C                                                                       FILSI063          
C     EVALUATE ODD AND EVEN SUMS.                                       FILSI064          
C                                                                       FILSI065          
350   NNP=2*N+2                                                         FILSI066          
      J=1                                                               FILSI067          
361   IF(J.GT.2) GOTO 369                                               FILSI068          
      NNP=NNP-1                                                         FILSI069          
      SUMI(J)=0.0                                                       FILSI070          
      S=J                                                               FILSI071          
391   IF(S.GT.NNP) GOTO 399                                             FILSI072          
      YS=A+FLOAT(S-1)*H                                                 FILSI073          
      SUMI(J)=SUMI(J)+FUNCT(YS)*COS(X*YS+YSI)                           FILSI074          
      S=S+2                                                             FILSI075          
      GOTO 391                                                          FILSI076          
399   CONTINUE                                                          FILSI077          
      J=J+1                                                             FILSI078          
      GOTO 361                                                          FILSI079          
369   CONTINUE                                                          FILSI080          
      SUMI(1)=SUMI(1)-0.5*(FUNCT(A)*COS(X*A+YSI)+FUNCT(B)*COS(X*B+YSI)) FILSI081          
      IF(.NOT.(TRIG.EQ.1)) GOTO 460                                     FILSI082          
C                                                                       FILSI083          
C     COSINE INTEGRAL.                                                  FILSI084          
C                                                                       FILSI085          
      FILSIMP=OVER*H*(ALPHA*(FUNCT(B)*SIN(X*B)-FUNCT(A)*SIN(X*A))+BETA* FILSI086          
     .SUMI(1)+GAMMA*SUMI(2))                                            FILSI087          
      GOTO 470                                                          FILSI088          
C                                                                       FILSI089          
C     SINE INTEGRAL.                                                    FILSI090          
C                                                                       FILSI091          
460   FILSIMP=OVER*H*(-ALPHA*(FUNCT(B)*COS(X*B)-FUNCT(A)*COS(X*A))+BETA FILSI092          
     .*SUMI(1)+GAMMA*SUMI(2))                                           FILSI093          
470   RETURN                                                            FILSI094          
      END                                                               FILSI095          
