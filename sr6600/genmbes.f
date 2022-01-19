      SUBROUTINE GENMBES(NU,Z,MBSSI,MBSSK)                              GENMB002          
      REAL NU,MBSSI,MBSSK                                               GENMB003          
      DIMENSION T(350),R(2)                                             GENMB004          
C   WRITTEN BY G. KEADY, C.S.I.R.O., MELBOURNE, DECEMBER, 1967          GENMB005          
C   MUST BE USED IN CONJUNCTION WITH C3 CSIR GAMMA                      GENMB006          
C   EVALUATION OF MODIFIED BESSEL FUNCTION FOR REAL POSITIVE ARGUMENTS  GENMB007          
C   AND ARBITRARY REAL ORDER.                                           GENMB008          
C   NU IS (REAL) ORDER OF BESSEL FUNCTION                               GENMB009          
C   Z IS THE ARGUMENT                                                   GENMB010          
C   MBSSK IS THE RETURNED VALUE OF K - REAL                             GENMB011          
C   MBSSI IS THE RETURNED VALUE OF I - REAL                             GENMB012          
      INU=IFIX(NU)                                                      GENMB013          
      IJ=IABS(INU)+12                                                   GENMB014          
      IK=2*IFIX(ABS(Z))+12                                              GENMB015          
      M=IK                                                              GENMB016          
      IF(IK.GE.IJ) GOTO 80                                              GENMB017          
      M=IJ                                                              GENMB018          
80    NM=M                                                              GENMB019          
      K=M+1                                                             GENMB020          
      Q=2./Z                                                            GENMB021          
C                                                                       GENMB022          
C       THE FOLLOWING TEST MUST BE CHANGED IF ANY CHANGE IS MADE TO THE GENMB023          
C     DIMENSION OF T TO SAVE SPACE OR HANDLE LARGER ARGUMENTS.          GENMB024          
C                                                                       GENMB025          
      IF(.NOT.(M.GT.348)) GOTO 150                                      GENMB026          
C                                                                       GENMB027          
      WRITE(61,130) Z                                                   GENMB028          
130   FORMAT(8H ERROR I,8HN GENMBE,8HSS. ^ARG,8HUMENT^ T,8HOO LARGE,    GENMB029          
     .2H: ,E12.4)                                                       GENMB030          
      GOTO 1170                                                         GENMB031          
150   III=1                                                             GENMB032          
151   IF(III.GT.2) GOTO 159                                             GENMB033          
      AN=NU-FLOAT(INU)                                                  GENMB034          
      AAN=ABS(AN)                                                       GENMB035          
      T(M+2)=0.                                                         GENMB036          
      T(M+1)=T(M+2)                                                     GENMB037          
      T(M)=1.E-20                                                       GENMB038          
      BN=1.-AN                                                          GENMB039          
      IF(.NOT.(AN.LT.0.)) GOTO 230                                      GENMB040          
      BN=AAN                                                            GENMB041          
230   F=FLOAT(M)-BN                                                     GENMB042          
      T(M-1)=Q*F*T(M)+T(M+1)                                            GENMB043          
      M=M-1                                                             GENMB044          
      IF(.NOT.(M.EQ.1)) GOTO 230                                        GENMB045          
      IF(.NOT.(AAN.LT.1.E-10)) GOTO 490                                 GENMB046          
      P=0. $ KK=1 $ G=-1.                                               GENMB047          
      E=0.0                                                             GENMB048          
      IF(.NOT.(Z.GE.10.0)) GOTO 410                                     GENMB049          
330   LL=2*KK+1                                                         GENMB050          
      P=P+T(KK+1)                                                       GENMB051          
      IF(.NOT.(LL.LT.K)) GOTO 360                                       GENMB052          
      E=E+T(LL)/FLOAT(KK)                                               GENMB053          
360   CONTINUE                                                          GENMB054          
      KK=KK+1                                                           GENMB055          
      IF(.NOT.(KK.GE.K)) GOTO 330                                       GENMB056          
      ALPHA=(T(1)+2.0*P)*EXP(-Z)                                        GENMB057          
      GOTO 650                                                          GENMB058          
410   LL=2*KK+1                                                         GENMB059          
      E=E+T(LL)/FLOAT(KK)                                               GENMB060          
      P=P+G*T(LL)                                                       GENMB061          
      KK=KK+1                                                           GENMB062          
      G=-G                                                              GENMB063          
      IF(.NOT.(LL.GE.K)) GOTO 410                                       GENMB064          
      ALPHA=T(1)+2.0*P                                                  GENMB065          
      GOTO 650                                                          GENMB066          
490   W=T(1)                                                            GENMB067          
      II=1 $ Y=1.                                                       GENMB068          
      IF(.NOT.(AN.LT.0.)) GOTO 550                                      GENMB069          
      DD=AN+1.                                                          GENMB070          
      GOTO 560                                                          GENMB071          
550   DD=AAN                                                            GENMB072          
560   S=Q**DD*GAMMA(DD+1.)                                              GENMB073          
570   JJ=2*II+1                                                         GENMB074          
      X=(DD+2E0*FLOAT(II))*(DD+FLOAT(II)-1.)/(FLOAT(II)*(DD+2E0*FLOAT(  GENMB075          
     .II)-2.))                                                          GENMB076          
      Y=-Y*X                                                            GENMB077          
      H=T(JJ)*Y                                                         GENMB078          
      W=W+H                                                             GENMB079          
      II=II+1                                                           GENMB080          
      IF(.NOT.(JJ.GE.K)) GOTO 570                                       GENMB081          
      ALPHA=S*W                                                         GENMB082          
650   IM=IABS(INU)+1                                                    GENMB083          
      MBSSI=T(IM)/ALPHA                                                 GENMB084          
      R(III)=MBSSI                                                      GENMB085          
      IF(NU.GE.0.) GOTO 790                                             GENMB086          
      IF(.NOT.(AAN.LT.1.E-10)) GOTO 710                                 GENMB087          
      R(III)=MBSSI                                                      GENMB088          
      GOTO 790                                                          GENMB089          
710   S=T(1)/ALPHA $ V=T(2)/ALPHA                                       GENMB090          
      F=AN+1.                                                           GENMB091          
740   MBSSI=Q*F*S+V                                                     GENMB092          
      R(III)=MBSSI                                                      GENMB093          
      V=S $ S=MBSSI                                                     GENMB094          
      F=F-1.                                                            GENMB095          
      IF(.NOT.(ABS(F-NU).LT.1.E-10)) GOTO 740                           GENMB096          
790   NU=-NU                                                            GENMB097          
      INU=-INU                                                          GENMB098          
      M=NM                                                              GENMB099          
      III=III+1                                                         GENMB100          
      GOTO 151                                                          GENMB101          
159   CONTINUE                                                          GENMB102          
      IF(.NOT.(AAN.LT.1.E-10)) GOTO 1140                                GENMB103          
      IF(Z.LT.10.0) GOTO 890                                            GENMB104          
      TWONZ=2.0/Z                                                       GENMB105          
      FKO=(1.25331414+TWONZ*(-0.07832358+TWONZ*(0.02189568+TWONZ*(-     GENMB106          
     .0.01062446+TWONZ*(0.00587872+TWONZ*(-0.00251540+TWONZ*0.00053208) GENMB107          
     .)))))/(SQRT(Z)*EXP(Z))                                            GENMB108          
      GOTO 900                                                          GENMB109          
890   FKO=-(T(1)*(.57721566490+ALOG(Z/2.))-2.*E)/ALPHA                  GENMB110          
900   FKI=ALPHA/T(1)*(1./Z-FKO*T(2)/ALPHA)                              GENMB111          
      IF(.NOT.(INU.EQ.0)) GOTO 940                                      GENMB112          
      MBSSK=FKO                                                         GENMB113          
      GOTO 1160                                                         GENMB114          
940   IF(.NOT.(INU.EQ.1)) GOTO 970                                      GENMB115          
      MBSSK=FKI                                                         GENMB116          
      GOTO 1160                                                         GENMB117          
970   IF(.NOT.(INU.GT.0)) GOTO 1060                                     GENMB118          
      BB=FKI $ CC=FKO $ F=1.                                            GENMB119          
1010  MBSSK=Q*F*BB+CC                                                   GENMB120          
      CC=BB $ BB=MBSSK                                                  GENMB121          
      F=F+1.                                                            GENMB122          
      IF(ABS(FLOAT(INU)-F).LT.1.E-10) GOTO 1160                         GENMB123          
      GOTO 1010                                                         GENMB124          
1060  BB=FKO $ CC=FKI $ F=0.                                            GENMB125          
1090  MBSSK=-Q*F*BB+CC                                                  GENMB126          
      CC=BB $ BB=MBSSK                                                  GENMB127          
      F=F-1.                                                            GENMB128          
      IF(ABS(FLOAT(INU)-F).LT.1.E-10) GOTO 1160                         GENMB129          
      GOTO 1090                                                         GENMB130          
1140  D=3.1415926536*NU                                                 GENMB131          
      MBSSK=1.5707963268*(R(2)-R(1))/SIN(D)                             GENMB132          
1160  MBSSI=R(1)                                                        GENMB133          
1170  RETURN                                                            GENMB134          
      END                                                               GENMB135          
