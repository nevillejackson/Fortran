      SUBROUTINE GENBESS(NU,Z,BESSJ,BESSY)                              GENBE002          
      REAL NU                                                           GENBE003          
      DIMENSION T(350),R(2)                                             GENBE004          
      INU=IFIX(NU)                                                      GENBE005          
      IJ=IABS(INU)+12                                                   GENBE006          
      IK=2*IFIX(ABS(Z))+12                                              GENBE007          
      M=IK                                                              GENBE008          
      IF(IK.GE.IJ) GOTO 80                                              GENBE009          
      M=IJ                                                              GENBE010          
80    NM=M                                                              GENBE011          
      K=M+1                                                             GENBE012          
      Q=2./Z                                                            GENBE013          
      III=1                                                             GENBE014          
111   IF(III.GT.2) GOTO 119                                             GENBE015          
      A=NU-FLOAT(INU)                                                   GENBE016          
      AA=ABS(A)                                                         GENBE017          
      T(M+2)=0.                                                         GENBE018          
      T(M+1)=T(M+2)                                                     GENBE019          
      T(M)=1.E-20                                                       GENBE020          
      B=1.-A                                                            GENBE021          
      IF(.NOT.(A.LT.0.)) GOTO 190                                       GENBE022          
      B=AA                                                              GENBE023          
190   F=FLOAT(M)-B                                                      GENBE024          
      T(M-1)=Q*F*T(M)-T(M+1)                                            GENBE025          
      M=M-1                                                             GENBE026          
      IF(.NOT.(M.EQ.1)) GOTO 190                                        GENBE027          
      IF(.NOT.(AA.LT.1.E-10)) GOTO 350                                  GENBE028          
      E=0.                                                              GENBE029          
      P=E $ KK=1 $ G=-1.                                                GENBE030          
270   LL=2*KK+1                                                         GENBE031          
      E=E+G*T(LL)/FLOAT(KK)                                             GENBE032          
      P=P+T(LL)                                                         GENBE033          
      KK=KK+1                                                           GENBE034          
      G=-G                                                              GENBE035          
      IF(.NOT.(LL.GE.K)) GOTO 270                                       GENBE036          
      ALPHA=T(1)+2.*P                                                   GENBE037          
      GOTO 510                                                          GENBE038          
350   W=T(1)                                                            GENBE039          
      II=1 $ Y=1.                                                       GENBE040          
      IF(.NOT.(A.LT.0.)) GOTO 410                                       GENBE041          
      DD=A+1.                                                           GENBE042          
      GOTO 420                                                          GENBE043          
410   DD=AA                                                             GENBE044          
420   S=Q**DD*GAMMA(DD+1.)                                              GENBE045          
430   JJ=2*II+1                                                         GENBE046          
      X=(DD+2E0*FLOAT(II))*(DD+FLOAT(II)-1.)/(FLOAT(II)*(DD+2E0*FLOAT(  GENBE047          
     .II)-2.))                                                          GENBE048          
      Y=Y*X                                                             GENBE049          
      H=T(JJ)*Y                                                         GENBE050          
      W=W+H                                                             GENBE051          
      II=II+1                                                           GENBE052          
      IF(.NOT.(JJ.GE.K)) GOTO 430                                       GENBE053          
      ALPHA=S*W                                                         GENBE054          
510   IM=IABS(INU)+1                                                    GENBE055          
      BESSJ=T(IM)/ALPHA                                                 GENBE056          
      R(III)=BESSJ                                                      GENBE057          
      IF(NU.GE.0.) GOTO 650                                             GENBE058          
      IF(.NOT.(AA.LT.1.E-10)) GOTO 570                                  GENBE059          
      BESSJ=FLOAT(-1)**(-INU)*BESSJ                                     GENBE060          
      R(III)=BESSJ                                                      GENBE061          
      GOTO 650                                                          GENBE062          
570   S=T(1)/ALPHA $ V=T(2)/ALPHA                                       GENBE063          
      F=A+1.                                                            GENBE064          
600   BESSJ=Q*F*S-V                                                     GENBE065          
      R(III)=BESSJ                                                      GENBE066          
      V=S $ S=BESSJ                                                     GENBE067          
      F=F-1.                                                            GENBE068          
      IF(.NOT.(ABS(F-NU).LT.1.E-10)) GOTO 600                           GENBE069          
650   NU=-NU                                                            GENBE070          
      INU=-INU                                                          GENBE071          
      M=NM                                                              GENBE072          
      III=III+1                                                         GENBE073          
      GOTO 111                                                          GENBE074          
119   CONTINUE                                                          GENBE075          
      IF(.NOT.(AA.LT.1.E-10)) GOTO 950                                  GENBE076          
      Y0=.63661977231/ALPHA*(T(1)*(.57721566490+ALOG(Z/2.))-2.*E)       GENBE077          
      Y1=ALPHA/T(1)*(Y0*T(2)/ALPHA-Q/3.1415926536)                      GENBE078          
      IF(.NOT.(INU.EQ.0)) GOTO 750                                      GENBE079          
      BESSY=Y0                                                          GENBE080          
      GOTO 1000                                                         GENBE081          
750   IF(.NOT.(INU.EQ.1)) GOTO 780                                      GENBE082          
      BESSY=Y1                                                          GENBE083          
      GOTO 1000                                                         GENBE084          
780   IF(.NOT.(INU.GT.0)) GOTO 870                                      GENBE085          
      BB=Y1 $ CC=Y0 $ F=1.                                              GENBE086          
820   BESSY=Q*F*BB-CC                                                   GENBE087          
      CC=BB $ BB=BESSY                                                  GENBE088          
      F=F+1.                                                            GENBE089          
      IF(FLOAT(INU)-F.LT.1.E-10) GOTO 1000                              GENBE090          
      GOTO 820                                                          GENBE091          
870   BB=Y0 $ CC=Y1 $ F=0.                                              GENBE092          
900   BESSY=Q*F*BB-CC                                                   GENBE093          
      CC=BB $ BB=BESSY                                                  GENBE094          
      F=F-1.                                                            GENBE095          
      IF(ABS(FLOAT(INU)-F).LT.1.E-10) GOTO 1000                         GENBE096          
      GOTO 900                                                          GENBE097          
950   D=3.1415926536*ABS(NU)                                            GENBE098          
      IF(.NOT.(NU.LT.0.)) GOTO 990                                      GENBE099          
      BESSY=(R(2)-R(1)*COS(D))/SIN(D)                                   GENBE100          
      GOTO 1000                                                         GENBE101          
990   BESSY=(R(1)*COS(D)-R(2))/SIN(D)                                   GENBE102          
1000  BESSJ=R(1)                                                        GENBE103          
      RETURN                                                            GENBE104          
      END                                                               GENBE105          
