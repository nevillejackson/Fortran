      SUBROUTINE GNROT36(F,NROOT,NIT,LINT,LR,ACC,ZI,EPSILON,ZROOT,FRES) GNROO002          
      COMPLEX       CSQRT,      Z,ZROOT,F,CF,CFR,B,D,Q,V,ZI,EPSILON,ZFM GNROO003          
      DIMENSION Z(4),CF(4),ZROOT(1),CFR(4),FRES(1)                      GNROO004          
C                                                                       GNROO005          
C     WRITTEN BY N.ROBINSON C.S.I.R.O. D.B.R. HIGHETT OCTOBER 1967      GNROO006          
C     LAST REVISION SEPTEMBER, 1968                                     GNROO007          
C     ZEROES OF AN ARBITRARY FUNCTION F(Z)=0                            GNROO008          
C     TRANSFORTRANSLATION OF FRANK/MULLER METHOD.                       GNROO009          
C     REFERENCE-A.C.M.J. VOL.5.1958.PP.154-167.                         GNROO010          
C  F,ZROOT ARE COMPLEX FN FOR ROOT DET. AND COMPLEX ROOT VALUE(OUTPUT). GNROO011          
C     FRES         =ABSOLUTE RESIDUAL VALUE OF F AT END OF ITERATIONS.  GNROO012          
C     NROOT        =NO. OF ROOTS REQD.SPECIFIED OTHERWISE SET=1.        GNROO013          
C     NIT          =NO. OF ITERATIONS.SPECIFIED OTHERWISE SET=35.       GNROO014          
C     LINT         =OPTION ON INTERMEDIATE OUTPUT (1,0)                 GNROO015          
C     LR           =OPTION ON REAL ROOTS ONLY     (1,0)                 GNROO016          
C     ACC          =SPECIFIED ACCURACY OF ROOTS GE 1.E-09 OTHERWISE SET GNROO017          
C ZI,EPSILON=COMPLEX STARTING PT. AND RANGE.SPECIFIED OR(0.,0.),(0.,0.) GNROO018          
C                                                                       GNROO019          
C                                                                       GNROO020          
C     FOR A SAVING IN CALCULATION TIME THE USER MAY SET LCONJ=1 IF IT ISGNROO021          
C     KNOWN THAT ROOTS ARE CONJUGATE,SIMILARLY IF THE ROOTS KNOWN TO BE GNROO022          
C     + AND - OF THE SAME MAGNITUDE SET LASSYM=1.                       GNROO023          
      NNROOT=NROOT $ ACCY=ACC $ NNIT=NIT                                GNROO024          
      LC=1-LR                                                           GNROO025          
      S=0 $ V=1                                                         GNROO026          
      TF=0                                                              GNROO027          
      ZFM=1                                                             GNROO028          
C     ACCURACY                                                          GNROO029          
      IF(.NOT.(ACCY.LT.1.E-09)) GOTO 120                                GNROO030          
      ACCY=1.E-08                                                       GNROO031          
120   E1=ACCY                                                           GNROO032          
C     NUMBER OF ITERATIONS                                              GNROO033          
      IF(.NOT.(NNIT.EQ.0)) GOTO 150                                     GNROO034          
      NNIT=35                                                           GNROO035          
C     NUMBER OF ROOTS REQD.                                             GNROO036          
150   IF(.NOT.(NNROOT.EQ.0)) GOTO 180                                   GNROO037          
      NNROOT=1                                                          GNROO038          
      WRITE(61,1320)                                                    GNROO039          
C                                                                       GNROO040          
C                                                                       GNROO041          
180   IROOT=0                                                           GNROO042          
190   IROOT=IROOT+1                                                     GNROO043          
      IF(IROOT.GT.NNROOT) GOTO 1300                                     GNROO044          
C                                                                       GNROO045          
C     INITIALIZING Z,F(Z).                                              GNROO046          
210   IF(.NOT.(CABS(ZI)+CABS(EPSILON).LT.1.E-08)) GOTO 260              GNROO047          
      Z(1)=-1E0+S $ Z(2)=1E0+S $ Z(3)=S $ GOTO 290                      GNROO048          
260   Z(1)=ZI*(1E0-EPSILON)+S $ Z(2)=ZI*(1E0+EPSILON)+S $ Z(3)=ZI+S     GNROO049          
290   CF(1)=F(Z(1)) $ CF(2)=F(Z(2)) $ CF(3)=F(Z(3))                     GNROO050          
C                                                                       GNROO051          
C     INITIAL REDUCED FUNCTIONS.                                        GNROO052          
      IROOT1=IROOT-1                                                    GNROO053          
      D=V                                                               GNROO054          
      B=D                                                               GNROO055          
      Q=B                                                               GNROO056          
      IF(IROOT.EQ.1) GOTO 390                                           GNROO057          
      IP=1                                                              GNROO058          
351   IF(IP.GT.IROOT1) GOTO 359                                         GNROO059          
      B=B*(Z(1)-ZROOT(IP))                                              GNROO060          
      D=D*(Z(2)-ZROOT(IP))                                              GNROO061          
      Q=Q*(Z(3)-ZROOT(IP))                                              GNROO062          
      IP=IP+1                                                           GNROO063          
      GOTO 351                                                          GNROO064          
359   CONTINUE                                                          GNROO065          
390   CFR(1)=CF(1)/B $ CFR(2)=CF(2)/D $ CFR(3)=CF(3)/Q                  GNROO066          
      B=CFR(1) $ D=CFR(2) $ Q=CFR(3)                                    GNROO067          
      CFR(1)=B*ZFM $ CFR(2)=D*ZFM $ CFR(3)=Q*ZFM                        GNROO068          
      LRETURN=0                                                         GNROO069          
C                                                                       GNROO070          
C     TESTING FOR STARTING VALUES AS ROOTS.                             GNROO071          
      IR=1                                                              GNROO072          
491   IF(IR.GT.3) GOTO 499                                              GNROO073          
      T=CABS(CFR(IR))                                                   GNROO074          
      IF(.NOT.(T.LT.E1)) GOTO 610                                       GNROO075          
      ZROOT(IROOT)=Z(IR) $ FRES(IROOT)=T $ AIMZ=AIMAG(Z(IR)) $ RMAGZ=   GNROO076          
     .CABS(Z(IR))                                                       GNROO077          
      IF(IROOT.GE.NNROOT) GOTO 1330                                     GNROO078          
      S=S+.01 $ LRETURN=1 $ IROOT=IROOT+1                               GNROO079          
610   CONTINUE                                                          GNROO080          
      IR=IR+1                                                           GNROO081          
      GOTO 491                                                          GNROO082          
499   CONTINUE                                                          GNROO083          
      IF(IROOT.GT.NNROOT) GOTO 1330                                     GNROO084          
      IF(LRETURN.EQ.1) GOTO 210                                         GNROO085          
C                                                                       GNROO086          
C     RELATIVE FUNCTION MODIFIER...ZROOT1*ZROOT2*ZROOT3.....            GNROO087          
      ZFM=1                                                             GNROO088          
      IROOT1=IROOT-1                                                    GNROO089          
      IF(IROOT.EQ.1) GOTO 710                                           GNROO090          
      IFM=1                                                             GNROO091          
681   IF(IFM.GT.IROOT1) GOTO 689                                        GNROO092          
      IF(CABS(ZROOT(IFM)).LT.E1) GOTO 710                               GNROO093          
      ZFM=ZFM*ZROOT(IFM)                                                GNROO094          
710   CONTINUE                                                          GNROO095          
      IFM=IFM+1                                                         GNROO096          
      GOTO 681                                                          GNROO097          
689   CONTINUE                                                          GNROO098          
      CFR(1)=B*ZFM $ CFR(2)=D*ZFM $ CFR(3)=Q*ZFM                        GNROO099          
      D=(Z(3)-Z(2))/(Z(2)-Z(1))                                         GNROO100          
C                                                                       GNROO101          
C                                                                       GNROO102          
      IT=0                                                              GNROO103          
770   IT=IT+1                                                           GNROO104          
      IF(IT.GT.NNIT) GOTO 1250                                          GNROO105          
C                                                                       GNROO106          
C     OPTION ON OUTPUT OF INTERMEDIATE VALUES.                          GNROO107          
      IF(.NOT.(LINT.EQ.1)) GOTO 870                                     GNROO108          
      REZ=REAL(Z(3)) $ REF=REAL(CF(3)) $ REFR=REAL(CFR(3))              GNROO109          
      AIMZ=AIMAG(Z(3)) $ AIMF=AIMAG(CF(3)) $ AIMFR=AIMAG(CFR(3))        GNROO110          
      WRITE(61,1310) IROOT,REZ,REF,REFR,AIMZ,AIMF,AIMFR                 GNROO111          
870   B=CFR(1)*D*D-CFR(2)*(1E0+D)*(1E0+D)+CFR(3)*(1E0+2E0*D)            GNROO112          
      Q=CSQRT(B*B-4E0*CFR(3)*D*(1E0+D)*(CFR(1)*D-CFR(2)*(1E0+D)+CFR(3)) GNROO113          
     .)                                                                 GNROO114          
C                                                                       GNROO115          
C     SELECT SMALLEST NEW D,RESULTING FROM QUADRATIC ROOTS.             GNROO116          
      T=CABS(B+Q)                                                       GNROO117          
      IF(T-CABS(B-Q)) 940,910,920                                       GNROO118          
910   IF(T.LT.1.E-10) GOTO 960                                          GNROO119          
920   Q=B+Q $ GOTO 950                                                  GNROO120          
940   Q=B-Q                                                             GNROO121          
950   IF(.NOT.(CABS(Q).LT.1.E-09)) GOTO 980                             GNROO122          
960   D=1 $ GOTO 990                                                    GNROO123          
980   D=-2E0*CFR(3)*(1E0+D)/Q                                           GNROO124          
990   D=CMPLX(REAL(D),FLOAT(LC)*AIMAG(D))                               GNROO125          
C                                                                       GNROO126          
C IF REQUIRE REAL ROOTS USE REAL STARTING POINTS IN ADDITION TO LR=1.   GNROO127          
      Z(4)=Z(3)+(Z(3)-Z(2))*D                                           GNROO128          
C                                                                       GNROO129          
C     TEST FOR RATIO...CABS(F(Z(4)))/CABS(F(Z(3))).                     GNROO130          
      CF(4)=F(Z(4))                                                     GNROO131          
      IF(.NOT.(CABS(CF(4))/CABS(CF(3)).GT.10.0)) GOTO 1050              GNROO132          
      D=D/2E0 $ GOTO 990                                                GNROO133          
C                                                                       GNROO134          
C     DROP Z(1).                                                        GNROO135          
1050  Z(1)=Z(2) $ Z(2)=Z(3) $ Z(3)=Z(4)                                 GNROO136          
      CF(1)=CF(2) $ CF(2)=CF(3) $ CF(3)=CF(4)                           GNROO137          
      CFR(1)=CFR(2) $ CFR(2)=CFR(3)                                     GNROO138          
C                                                                       GNROO139          
C     REDUCED FUNCTION.                                                 GNROO140          
      Q=1                                                               GNROO141          
      IF(IROOT.EQ.1) GOTO 1170                                          GNROO142          
      IP=1                                                              GNROO143          
1151  IF(IP.GT.IROOT1) GOTO 1159                                        GNROO144          
      Q=Q*(Z(3)-ZROOT(IP))                                              GNROO145          
      IP=IP+1                                                           GNROO146          
      GOTO 1151                                                         GNROO147          
1159  CONTINUE                                                          GNROO148          
1170  CFR(3)=CF(3)/Q*ZFM                                                GNROO149          
C                                                                       GNROO150          
C     CONVERGENCE TEST.                                                 GNROO151          
      TF=CABS(CFR(3))                                                   GNROO152          
      TFF=CABS(CF(3))                                                   GNROO153          
      IF(TFF.GT.100E0*E1) GOTO 1240                                     GNROO154          
      IF(CABS(1E0-Z(2)/Z(3)).LT.E1) GOTO 1230                           GNROO155          
C                                                                       GNROO156          
C     OUT OF BOUNDS TEST.                                               GNROO157          
      IF(.NOT.(TFF.LT.1.E-290.OR.TF.LT.1.E-290)) GOTO 1240              GNROO158          
1230  IT=NNIT                                                           GNROO159          
1240  GOTO 770                                                          GNROO160          
1250  CONTINUE                                                          GNROO161          
C                                                                       GNROO162          
      AIMZ=AIMAG(Z(3))                                                  GNROO163          
      ZROOT(IROOT)=Z(3)                                                 GNROO164          
C                                                                       GNROO165          
      FRES(IROOT)=CABS(CF(3))                                           GNROO166          
C                                                                       GNROO167          
      GOTO 190                                                          GNROO168          
1300  CONTINUE                                                          GNROO169          
1310  FORMAT(8H INTERME,8HDIATE VA,4HLUES,10X,6H ROOT ,I3,5X,6HREALZ=,  GNROO170          
     .F20.10,5X,6HREALF=,F20.10,5X,7HREALFR=,F20.10/44X,6HIMAGZ=,F20.10 GNROO171          
     .,5X,6HIMAGF=,F20.10,5X,7HIMAGFR=,F20.10)                          GNROO172          
1320  FORMAT(8H NUMBER ,8HOF ROOTS,8H NOT SPE,8HCIFIED F,8HIRST ROO,    GNROO173          
     .8HT ONLY C,8HALCULATE,2HD //)                                     GNROO174          
1330  CONTINUE                                                          GNROO175          
      RETURN                                                            GNROO176          
      END                                                               GNROO177          
