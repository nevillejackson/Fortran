      SUBROUTINE LP OUT (BUF,XTITLE,YTITLE,ILINE,LLINE)                 PLOTO002          
C ROUTINE      X1 CSIR LPPLOT            DATE MAY 69              017101PLOTO003          
C TITLE        A LINE PRINTER PLOTTING PACKAGE                    017102PLOTO004          
C DESCRIPTION  INTENDED AS A FASTER EQUIVALENT OF QUIKPLOT,       017103PLOTO005          
C              WITH MANY EXTRA FEATURES. USES A PAGE BUFFFER ARRAY017104PLOTO006          
C              AND ALLOWS THE USER CHOICE OF SYMBOLS, TYPE OF PLOT017105PLOTO007          
C              FOR EACH SET OF POINTS TO BE PLOTTED ON THE BUFFER 017106PLOTO008          
C              ARRAY. ROUTINE IS REENTRANT AND CAN HANDLE ANY     017107PLOTO009          
C              NUMBER OF BUFFER ARRAYS SIMULTANEOUSLY. USER OF    017108PLOTO010          
C              DATA DEFINED SCALING, PRINTER CAN BE OUT OF BOUNDS 017109PLOTO011          
C              OF BUFFER, AND DATA MAY CONTAIN MISSING VALUES.    017110PLOTO012          
C              (3200,3600,6400 SEPARATE VERSIONS)                 017111PLOTO013          
C LANGUAGE     32FTN,36FTN,6400FTN       AVAILABLE FROM AUTHOR    017112PLOTO014          
C AVAILABLE AS (32BIN., WRITE-UP                                  017113PLOTO015          
C AUTHOR       R. LAMACRAFT, BIOMETRY SECTION,WAITE AGRICULTURAL  017114PLOTO016          
C              RESEARCH INSTITUTE, P.M.B. 1, GLEN OSMOND,         017115PLOTO017          
C              SOUTH AUST. 5064                                   017116PLOTO018          
C KEYWORDS     PLOTTING,LINEPRINTER,SCALING                       017117PLOTO019          
C SEE ALSO     QUIKPLOT                                           017118PLOTO020          
      DIMENSION BUF(1),FMT1(3),FMT2(3),XAXIS(6),YAXIS(6)                PLOTO021          
      DATA((FMT1(I),I=1,3)=5H(9X6(,5HF12.0,5H,8X)))                     PLOTO022          
      DATA((FMT2(I),I=1,3)=6H(6XA1,,5HF12.0,                            PLOTO023          
     *    7H,11A10))                   ** CDC 6400 VERSION **           PLOTO024          
      KQ=11                            ** CDC 6400 VERSION **           PLOTO025          
      NCH=110                          ** CDC 6400 VERSION **           PLOTO026          
      KK=ILINE                                                          PLOTO027          
      JJ=LLINE                                                          PLOTO028          
      IF(KK.LT.1) KK=1                                                  PLOTO029          
      IF(JJ.GT.55) JJ=55                                                PLOTO030          
C**** LOAD YTITLE INTO BUFFER LINE 54, XTITLE INTO BUFFER LINE 55       PLOTO031          
      LZ=54                                                             PLOTO032          
      LSYM=LDCH (YTITLE,1)                                              PLOTO033          
      K=2                                                               PLOTO034          
      GO TO 30                                                          PLOTO035          
   10 K=K+1                                                             PLOTO036          
      IF(LD.NE.LSYM) GO TO 20                                           PLOTO037          
      GO TO 50                                                          PLOTO038          
   20 IF(K.GT.101) GO TO 50                                             PLOTO039          
   30 IF(LZ.NE.54) GO TO 40                                             PLOTO040          
      LD=LDCH (YTITLE,K)                                                PLOTO041          
      GO TO 10                                                          PLOTO042          
   40 LD=LDCH (XTITLE,K)                                                PLOTO043          
      GO TO 10                                                          PLOTO044          
   50 L=LZ*NCH-(NCH+K)/2+1                                              PLOTO045          
      N1=1                                                              PLOTO046          
      GO TO 70                                                          PLOTO047          
   60 CALL STCH (LD,BUF(8),L)                                           PLOTO048          
      IF(N1.LT.K-2) GO TO 70                                            PLOTO049          
      IF(LZ.NE.54) GO TO 130                                            PLOTO050          
      LZ=55                                                             PLOTO051          
      LSYM=LDCH(XTITLE,1)                                               PLOTO052          
      K=2                                                               PLOTO053          
      GO TO 30                                                          PLOTO054          
   70 N1=N1+1                                                           PLOTO055          
      L=L+1                                                             PLOTO056          
      IF(LZ.NE.54) GO TO 80                                             PLOTO057          
      LD=LDCH (YTITLE,N1)                                               PLOTO058          
      GO TO 60                                                          PLOTO059          
   80 LD=LDCH (XTITLE,N1)                                               PLOTO060          
      GO TO 60                                                          PLOTO061          
C**** SET UP TO PRINT THE REQUIRED PARTS OF THE PAGE BUFFER             PLOTO062          
  130 IF(ILINE.EQ.0) PRINT 140                                          PLOTO063          
  140 FORMAT(1H1)                                                       PLOTO064          
      XAXIS(1)=BUF(1)                                                   PLOTO065          
      YAXIS(1)=BUF(3)                                                   PLOTO066          
      RX=(BUF(2)-XAXIS(1))/5.0                                          PLOTO067          
      RY=(BUF(4)-YAXIS(1))/5.0                                          PLOTO068          
      FMT1(2)=BUF(6)                                                    PLOTO069          
      FMT2(2)=BUF(7)                                                    PLOTO070          
      DO 150 I=2,5                                                      PLOTO071          
      XAXIS(I)=XAXIS(I-1)+RX                                            PLOTO072          
      YAXIS(I)=YAXIS(I-1)+RY                                            PLOTO073          
  150 CONTINUE                                                          PLOTO074          
      XAXIS(6)=BUF(2)                                                   PLOTO075          
      YAXIS(6)=BUF(4)                                                   PLOTO076          
C**** LOOP OVER THE LINES OF THE PAGE BUFFER                            PLOTO077          
  160 IF(KK.GT.JJ) RETURN                                               PLOTO078          
      L=KK*KQ                                                           PLOTO079          
      J=L-KQ+1                                                          PLOTO080          
      J1=KK-2                                                           PLOTO081          
      J2=J1/10                                                          PLOTO082          
      J1=J1-J2*10                                                       PLOTO083          
      M=6-J2                                                            PLOTO084          
      MM=LDCH (BUF(53*KQ+8),KK+25)                                      PLOTO085          
      KK=KK+1                                                           PLOTO086          
      IF(KK.EQ.55) GO TO 180                                            PLOTO087          
      IF(J1.EQ.0) GO TO 190                                             PLOTO088          
C**** PRINT A NORMAL LINE OF THE PAGE BUFFER                            PLOTO089          
      PRINT 170,MM,(BUF(I+7),I=J,L)                                     PLOTO090          
  170 FORMAT(6X,A1,12X,11A10)          ** CDC 6400 VERSION **           PLOTO091          
      GO TO 160                                                         PLOTO092          
C**** PRINT THE X AXIS NUMBERING LINE OF THE GRAPH                      PLOTO093          
  180 PRINT FMT1,(XAXIS(M),M=1,6)                                       PLOTO094          
      GO TO 160                                                         PLOTO095          
C**** PRINT A LINE OF THE PAGE BUFFER THAT INCLUDES A Y AXIS NUMBER     PLOTO096          
  190 PRINT FMT2,MM,YAXIS(M),(BUF(I+7),I=J,L)                           PLOTO097          
      GO TO 160                                                         PLOTO098          
      END                                                               PLOTO099          
      SUBROUTINE LP GRID (BUF,IX,IY)                                    PLOTO100          
      DIMENSION BUF(1)                                                  PLOTO101          
      KQ=11                            ** CDC 6400 VERSION **           PLOTO102          
      NCH=110                          ** CDC 6400 VERSION **           PLOTO103          
      KP=55*KQ+2                                                        PLOTO104          
      DO 760 I=1,KP                                                     PLOTO105          
      BUF(I+5)=HMODE(5H     )                                           PLOTO106          
  760 CONTINUE                                                          PLOTO107          
      ID=1                                                              PLOTO108          
      KSYM=1H-                                                          PLOTO109          
      M2=20                                                             PLOTO110          
      N1=2                                                              PLOTO111          
      N2=52                                                             PLOTO112          
      N3=50/IX                                                          PLOTO113          
      GO TO 880                                                         PLOTO114          
  850 ID=NCH                                                            PLOTO115          
      M2=10                                                             PLOTO116          
      N1=2                                                              PLOTO117          
      N2=102                                                            PLOTO118          
      N3=100/IY                                                         PLOTO119          
      KSYM=1H:                         ** CDC 6400 VERSION **           PLOTO120          
  880 DO 950 I=N1,N2,N3                                                 PLOTO121          
      K=0                                                               PLOTO122          
      L=I                                                               PLOTO123          
      IF(ID.EQ.1) L=(I-1)*NCH+1                                         PLOTO124          
  910 L=L+ID                                                            PLOTO125          
      CALL STCH (1H+,BUF(8),L)                                          PLOTO126          
      IF(K.GE.M2) GO TO 950                                             PLOTO127          
      K=K+1                                                             PLOTO128          
      J=1                                                               PLOTO129          
  930 IF(J.GE.5) GO TO 910                                              PLOTO130          
      L=L+ID                                                            PLOTO131          
      CALL STCH (KSYM,BUF(8),L)                                         PLOTO132          
      J=J+1                                                             PLOTO133          
      GO TO 930                                                         PLOTO134          
  950 CONTINUE                                                          PLOTO135          
      IF(ID.EQ.1) GO TO 850                                             PLOTO136          
      RETURN                                                            PLOTO137          
      END                                                               PLOTO138          
      SUBROUTINE LP LINE (BUF,X,Y,NPOINT,ISYMB)                         PLOTO139          
      DIMENSION BUF(1),X(1),Y(1)                                        PLOTO140          
      NCH=110                          ** CDC 6400 VERSION **           PLOTO141          
C**** TEST WHETHER SCALING HAS BEEN DONE                                PLOTO142          
      VM=BUF(5)                                                         PLOTO143          
      IF(BUF(6).NE.HMODE(5H     )) GO TO 10                             PLOTO144          
      R=BUF(3)                                                          PLOTO145          
      BUF(3)=VM                                                         PLOTO146          
      BUF(6)=SCALES(X,BUF(1),NPOINT)                                    PLOTO147          
      BUF(3)=R                                                          PLOTO148          
      BUF(7)=SCALES(Y,BUF(3),NPOINT)                                    PLOTO149          
C**** LOOP OVER VALUES TO FIND FIRST NON-MISSING VALUE                  PLOTO150          
   10 DO 20 I=1,NPOINT                                                  PLOTO151          
      X1=X(I)                                                           PLOTO152          
      Y1=Y(I)                                                           PLOTO153          
      IF(X1.EQ.VM.OR.Y1.EQ.VM) GO TO 20                                 PLOTO154          
      GO TO 30                                                          PLOTO155          
   20 CONTINUE                                                          PLOTO156          
      RETURN                                                            PLOTO157          
C**** SET UP VALUES FOR DRAWING LINES                                   PLOTO158          
   30 BX=(BUF(2)-BUF(1))/100.0                                          PLOTO159          
      BY=(BUF(4)-BUF(3))/50.0                                           PLOTO160          
      II=I+1                                                            PLOTO161          
C**** LOOP OVER VALUES TO FIND A SUBSEQUENT NON-MISSING VALUE           PLOTO162          
      DO 130 I=II,NPOINT                                                PLOTO163          
      X2=X(I)                                                           PLOTO164          
      Y2=Y(I)                                                           PLOTO165          
      IF(X2.EQ.VM.OR.Y2.EQ.VM) GO TO 130                                PLOTO166          
C**** FIND SLOPE AND DIRECTION OF LINE                                  PLOTO167          
      X5=(X2-X1)*BY                                                     PLOTO168          
      Y5=(Y2-Y1)*BX                                                     PLOTO169          
      ISW=0                                                             PLOTO170          
      IF(Y5*Y5-X5*X5) 35,130,40                                         PLOTO171          
   35 ISW=1                                                             PLOTO172          
      P1=X1                                                             PLOTO173          
      Q1=Y1                                                             PLOTO174          
      P2=X2                                                             PLOTO175          
      Q2=Y2                                                             PLOTO176          
      B1=BX                                                             PLOTO177          
      B2=BY                                                             PLOTO178          
      PB=BUF(1)                                                         PLOTO179          
      QB=BUF(3)                                                         PLOTO180          
      GO TO 50                                                          PLOTO181          
   40 P1=Y1                                                             PLOTO182          
      P2=Y2                                                             PLOTO183          
      Q1=X1                                                             PLOTO184          
      Q2=X2                                                             PLOTO185          
      B1=BY                                                             PLOTO186          
      B2=BX                                                             PLOTO187          
      PB=BUF(3)                                                         PLOTO188          
      QB=BUF(1)                                                         PLOTO189          
   50 IF(P2.GT.P1) GO TO 60                                             PLOTO190          
      R=P1                                                              PLOTO191          
      P1=P2                                                             PLOTO192          
      P2=R                                                              PLOTO193          
      R=Q1                                                              PLOTO194          
      Q1=Q2                                                             PLOTO195          
      Q2=R                                                              PLOTO196          
   60 R=(Q2-Q1)/(P2-P1)                                                 PLOTO197          
C**** DETERMINE THE FIRST END POINT OF THE LINE                         PLOTO198          
      PR=ROUND((P1-PB)/B1)                                              PLOTO199          
      I1=IFIX(PR)                                                       PLOTO200          
      PT=PR*B1+PB                                                       PLOTO201          
      Q1=Q1+(PT-P1)*R                                                   PLOTO202          
      P1=PT                                                             PLOTO203          
      R1=R*B1/2.0                                                       PLOTO204          
      DQ=(Q1-QB)/B2                                                     PLOTO205          
      IF(ISW.EQ.1) R1=-BY/3.0                                           PLOTO206          
C**** DETERMINE CHARACTER POSITION IN CURRENT COLUMN OR LINE            PLOTO207          
   70 QM=ROUND(DQ)                                                      PLOTO208          
      JM=IFIX(QM)                                                       PLOTO209          
      KSYM=ISYMB                                                        PLOTO210          
      IF(KSYM.NE.1H") GO TO 90                                          PLOTO211          
C**** DEAL WITH EXTRA PRECISION LINE                                    PLOTO212          
      QL=ROUND(DQ-R1)                                                   PLOTO213          
      QU=ROUND(DQ+R1)                                                   PLOTO214          
      IF(QU.NE.QL) GO TO 80                                             PLOTO215          
      KSYM=1H:                         ** CDC 3200, 6400 VERSIONS **    PLOTO216          
      GO TO 90                                                          PLOTO217          
   80 IF(QU.EQ.QM) GO TO 90                                             PLOTO218          
      KSYM=1H.                                                          PLOTO219          
C**** INSERT CHARACTER AT REQUIRED POSITION                             PLOTO220          
   90 K1=I1                                                             PLOTO221          
      K2=50-JM                                                          PLOTO222          
      IF(ISW.EQ.1) GO TO 100                                            PLOTO223          
      K1=JM                                                             PLOTO224          
      K2=50-I1                                                          PLOTO225          
  100 IF(K1.LT.0.OR.K1.GT.100) GO TO 110                                PLOTO226          
      IF(K2.LT.0.OR.K2.GT.50) GO TO 110                                 PLOTO227          
      L=(K2+1)*NCH+K1+2                                                 PLOTO228          
      CALL STCH(KSYM,BUF(8),L)                                          PLOTO229          
C**** MOVE TO THE NEXT POSITION ON THE LINE SEGMENT                     PLOTO230          
  110 PR=PR+1.0                                                         PLOTO231          
      I1=I1+1                                                           PLOTO232          
      IF(PR.GT.ROUND((P2-PB)/B1)) GO TO 120                             PLOTO233          
      Q1=Q1+R*B1                                                        PLOTO234          
      DQ=(Q1-QB)/B2                                                     PLOTO235          
      GO TO 70                                                          PLOTO236          
C**** MOVE TO THE NEXT LINE SEGMENT                                     PLOTO237          
  120 X1=X2                                                             PLOTO238          
      Y1=Y2                                                             PLOTO239          
  130 CONTINUE                                                          PLOTO240          
      RETURN                                                            PLOTO241          
      END                                                               PLOTO242          
      SUBROUTINE LP PLOT (BUF,X,Y,NPOINT,IFORM,ISYMB)                   PLOTO243          
      DIMENSION BUF(1),X(1),Y(1),ISYMB(1)                               PLOTO244          
      NCH=110                          ** CDC 6400 VERSION **           PLOTO245          
C**** TEST WHETHER SCALING HAS BEEN DONE                                PLOTO246          
      IF(BUF(6).NE.HMODE(5H     )) GO TO 10                             PLOTO247          
      P=BUF(3)                                                          PLOTO248          
      BUF(3)=BUF(5)                                                     PLOTO249          
      BUF(6)=SCALES(X,BUF(1),NPOINT)                                    PLOTO250          
      BUF(3)=P                                                          PLOTO251          
      BUF(7)=SCALES(Y,BUF(3),NPOINT)                                    PLOTO252          
   10 BX=(BUF(2)-BUF(1))/100.0                                          PLOTO253          
      BY=(BUF(4)-BUF(3))/50.0                                           PLOTO254          
      VM=BUF(5)                                                         PLOTO255          
      M=IFORM                                                           PLOTO256          
C**** LOOP OVER THE NUMBER OF POINTS                                    PLOTO257          
      DO 160 I=1,NPOINT                                                 PLOTO258          
      LK=0                                                              PLOTO259          
      P=X(I)                                                            PLOTO260          
      Q=Y(I)                                                            PLOTO261          
      KSYM=ISYMB(1)                                                     PLOTO262          
      IF(M.LT.0) KSYM=ISYMB(I)                                          PLOTO263          
C**** TEST FOR MISSING VALUE IN X DATA                                  PLOTO264          
      IF(P.NE.VM) GO TO 20                                              PLOTO265          
      K=101                                                             PLOTO266          
      LK=1                                                              PLOTO267          
      KSYM=1HM                                                          PLOTO268          
C**** TEST FOR MISSING VALUE IN Y DATA                                  PLOTO269          
   20 IF(Q.NE.VM) GO TO 30                                              PLOTO270          
      J=-1                                                              PLOTO271          
      LK=1                                                              PLOTO272          
      KSYM=1HM                                                          PLOTO273          
   30 IF(LK.EQ.1) GO TO 40                                              PLOTO274          
C**** FIND CO-ORDINATES ON THE PAGE BUFFER                              PLOTO275          
      K=IFIX((P-BUF(1))/BX+0.5)                                         PLOTO276          
      J=IFIX((BUF(4)-Q)/BY+0.5)                                         PLOTO277          
C**** CHECK FOR POINTS OUT OF BOUNDS                                    PLOTO278          
   40 IF(K.GE.0) GO TO 50                                               PLOTO279          
      K=-1                                                              PLOTO280          
      GO TO 60                                                          PLOTO281          
   50 IF(K.LE.100) GO TO 70                                             PLOTO282          
      K=101                                                             PLOTO283          
   60 LK=1                                                              PLOTO284          
   70 IF(J.GE.0) GO TO 80                                               PLOTO285          
      J=-1                                                              PLOTO286          
      GO TO 90                                                          PLOTO287          
   80 IF(J.LE.50) GO TO 100                                             PLOTO288          
      J=51                                                              PLOTO289          
   90 LK=1                                                              PLOTO290          
C**** INSERT SYMBOL                                                     PLOTO291          
  100 L=(J+1)*NCH+K+2                                                   PLOTO292          
      CALL STCH(KSYM,BUF(8),L)                                          PLOTO293          
      IF(LK.EQ.1) GO TO 160                                             PLOTO294          
      N=1                                                               PLOTO295          
      IF(M) 130,110,140                                                 PLOTO296          
C**** INSERT COLUMN PLOT                                                PLOTO297          
  110 IF(J.LT.0) KSYM=ISYMB(1)                                          PLOTO298          
  120 IF(J.GE.49) GO TO 160                                             PLOTO299          
      L=L+NCH                                                           PLOTO300          
      J=J+1                                                             PLOTO301          
      CALL STCH (KSYM,BUF(8),L)                                         PLOTO302          
      GO TO 120                                                         PLOTO303          
C**** CHECK FOR MULTI-CHARACTER SYMBOL PLOTTING                         PLOTO304          
  130 KK=I                                                              PLOTO305          
      MM=-M                                                             PLOTO306          
      GO TO 150                                                         PLOTO307          
  140 KK=1                                                              PLOTO308          
      MM=M                                                              PLOTO309          
  150 IF(N.GE.MM) GO TO 160                                             PLOTO310          
      L=L+1                                                             PLOTO311          
      N=N+1                                                             PLOTO312          
      CALL STCH (LDCH (ISYMB(KK),N),BUF(8),L)                           PLOTO313          
      GO TO 150                                                         PLOTO314          
  160 CONTINUE                                                          PLOTO315          
      RETURN                                                            PLOTO316          
      END                                                               PLOTO317          
      FUNCTION SCALES (Z,B,N)                                           PLOTO318          
      DIMENSION Z(1),B(1),DIGIT(2)                                      PLOTO319          
      DATA(DIGIT(1)=5H12345),(DIGIT(2)=5H6789 )                         PLOTO320          
      VM=B(3)                                                           PLOTO321          
      K=0                                                               PLOTO322          
      DO 20 I=1,N                                                       PLOTO323          
      R=Z(I)                                                            PLOTO324          
      IF(R.EQ.VM) GO TO 20                                              PLOTO325          
      IF(K.NE.0) GO TO 10                                               PLOTO326          
      ZMIN=R                                                            PLOTO327          
      ZMAX=R                                                            PLOTO328          
      K=1                                                               PLOTO329          
      GO TO 20                                                          PLOTO330          
   10 IF(R.LT.ZMIN) ZMIN=R                                              PLOTO331          
      IF(R.GT.ZMAX) ZMAX=R                                              PLOTO332          
   20 CONTINUE                                                          PLOTO333          
      SPL=B(1)                                                          PLOTO334          
      K=0                                                               PLOTO335          
      IF(SPL.NE.VM) GO TO 30                                            PLOTO336          
      K=-1                                                              PLOTO337          
      SPL=ZMIN                                                          PLOTO338          
   30 SPR=B(2)                                                          PLOTO339          
      IF(SPR.EQ.VM) GO TO 40                                            PLOTO340          
      IF(K.EQ.0) GO TO 290                                              PLOTO341          
      GO TO 50                                                          PLOTO342          
   40 K=K+1                                                             PLOTO343          
      SPR=ZMAX                                                          PLOTO344          
   50 SP=(SPR-SPL)/100.0                                                PLOTO345          
      JJ=-1                                                             PLOTO346          
      P=0.5                                                             PLOTO347          
   60 C=SP                                                              PLOTO348          
      I=0                                                               PLOTO349          
      A=1.0                                                             PLOTO350          
      IF(C) 70,140,80                                                   PLOTO351          
   70 C=-C                                                              PLOTO352          
   80 D=P                                                               PLOTO353          
      IF(C.LE.D) GO TO 120                                              PLOTO354          
   90 IF(C.LT.D*A*10.0) GO TO 140                                       PLOTO355          
      A=A*1.0E+10                                                       PLOTO356          
      I=I+10                                                            PLOTO357          
  100 IF(C-A*D) 110,140,90                                              PLOTO358          
  110 A=A/10.0                                                          PLOTO359          
      I=I-1                                                             PLOTO360          
      GO TO 100                                                         PLOTO361          
  120 A=A/1.0E+10                                                       PLOTO362          
      I=I-10                                                            PLOTO363          
      IF(C-D*A) 120,140,130                                             PLOTO364          
  130 IF(C.LT.D*A*10.0) GO TO 140                                       PLOTO365          
      A=A*10.0                                                          PLOTO366          
      I=I+1                                                             PLOTO367          
      GO TO 130                                                         PLOTO368          
  140 IF(JJ) 150,300,310                                                PLOTO369          
  150 SET=1.0                                                           PLOTO370          
      SF=A                                                              PLOTO371          
      IF(SP.GT.SF) SET=2.0                                              PLOTO372          
      IF(SP.GT.SF+SF) SET=2.5                                           PLOTO373          
      IF(SP.GT.2.5*SF) SET=5.0                                          PLOTO374          
  155 C=SET*SF                                                          PLOTO375          
      SPT=C*100.0                                                       PLOTO376          
      IF(K) 270,160,280                                                 PLOTO377          
  160 R1=SF*10.0                                                        PLOTO378          
      R2=R1                                                             PLOTO379          
      J=-2                                                              PLOTO380          
      GO TO 180                                                         PLOTO381          
  170 B(1)=R                                                            PLOTO382          
      R1=R1*10.0                                                        PLOTO383          
      IF(R.EQ.0.0) GO TO 210                                            PLOTO384          
  180 J=J+1                                                             PLOTO385          
      R=ROUNDD(SPL/R1)*R1                                               PLOTO386          
      IF(R+SPT-ROUND(ZMAX/C)*C) 210,190,170                             PLOTO387          
  190 B(1)=R                                                            PLOTO388          
      GO TO 280                                                         PLOTO389          
  200 B(2)=R                                                            PLOTO390          
      R2=R2*10.0                                                        PLOTO391          
      IF(R.EQ.0.0) GO TO 230                                            PLOTO392          
  210 J=J+1                                                             PLOTO393          
      R=ROUNDU(SPR/R2)*R2                                               PLOTO394          
      IF(R-SPT-ROUND(ZMIN/C)*C) 200,220,230                             PLOTO395          
  220 B(2)=R                                                            PLOTO396          
      GO TO 270                                                         PLOTO397          
  230 IF(J.GT.0) GO TO 265                                              PLOTO398          
      IF(SET.LT.5.0) GO TO 240                                          PLOTO399          
      SF=SF*10.0                                                        PLOTO400          
      SET=1.0                                                           PLOTO401          
      GO TO 155                                                         PLOTO402          
  240 IF(SET.LT.2.5) GO TO 250                                          PLOTO403          
      SET=5.0                                                           PLOTO404          
      GO TO 155                                                         PLOTO405          
  250 IF(SET.LT.2.0) GO TO 260                                          PLOTO406          
      SET=2.5                                                           PLOTO407          
      GO TO 155                                                         PLOTO408          
  260 SET=2.0                                                           PLOTO409          
      GO TO 155                                                         PLOTO410          
  265 IF(R1.GE.R2) GO TO 280                                            PLOTO411          
  270 B(1)=B(2)-SPT                                                     PLOTO412          
      GO TO 290                                                         PLOTO413          
  280 B(2)=B(1)+SPT                                                     PLOTO414          
C**** FIND CORRECT FORMAT FOR AXIS SCALING                              PLOTO415          
  290 SP=(B(2)-B(1))/100.0                                              PLOTO416          
      P=1.0                                                             PLOTO417          
      JJ=0                                                              PLOTO418          
      GO TO 60                                                          PLOTO419          
  300 S=A                                                               PLOTO420          
      SP=B(1)                                                           PLOTO421          
      R2=B(2)                                                           PLOTO422          
      M1=I                                                              PLOTO423          
      IF(SP*SP.LT.R2*R2) SP=R2                                          PLOTO424          
      JJ=1                                                              PLOTO425          
      GO TO 60                                                          PLOTO426          
  310 M2=I                                                              PLOTO427          
      A=HMODE(5HF12.0)                                                  PLOTO428          
      IF(M2-M1.GT.10) GO TO 340                                         PLOTO429          
      IF(M2.LT.-9.OR.M2.GT.11) GO TO 340                                PLOTO430          
      M1=-M1                                                            PLOTO431          
      IF(M1.LE.0) GO TO 330                                             PLOTO432          
C**** MODIFY THE NUMBER OF DECIMAL PLACES IN THE FORMAT SPECIFICATION   PLOTO433          
      K=1                                                               PLOTO434          
      IF(M1.LT.6)  GO TO 320                                            PLOTO435          
      K=2                                                               PLOTO436          
      M1=M1-5                                                           PLOTO437          
  320 CALL STCH (LDCH (DIGIT(K),M1),A,5)                                PLOTO438          
  330 SCALES=A                                                          PLOTO439          
      RETURN                                                            PLOTO440          
C**** WHEN SCALE RANGE REQUIRES TOO MANY SIGNIFICANT DIGITS             PLOTO441          
  340 PRINT 350                                                         PLOTO442          
  350 FORMAT(10X,25HSCALE WILL NOT FIT FORMAT)                          PLOTO443          
      B(1)=0.0                                                          PLOTO444          
      B(2)=100.0                                                        PLOTO445          
      GO TO 330                                                         PLOTO446          
      END                                                               PLOTO447          
      FUNCTION ROUND(Z)                                                 PLOTO448          
      T = 0.0                                                           PLOTO449          
      GO TO 10                                                          PLOTO450          
      ENTRY ROUNDD                                                      PLOTO451          
      T = -0.499999999                                                  PLOTO452          
      GO TO 10                                                          PLOTO453          
      ENTRY ROUNDU                                                      PLOTO454          
      T = 0.49999999                                                    PLOTO455          
   10 P=1.0                                                             PLOTO456          
      X = Z                                                             PLOTO457          
      IF (X) 20,50,30                                                   PLOTO458          
   20 P = -1.0                                                          PLOTO459          
      X = -X                                                            PLOTO460          
   30 Y = X + T*P                                                       PLOTO461          
      IF (Y) 50,50,40                                                   PLOTO462          
   40 X = Y                                                             PLOTO463          
   50 X=X+0.5                                                           PLOTO464          
      ROUND=(X-AMOD(X,1.0))*P                                           PLOTO465          
      RETURN                                                            PLOTO466          
      ENTRY HMODE                                                       PLOTO467          
      ROUND = Z                                                         PLOTO468          
      RETURN                                                            PLOTO469          
      END                                                               PLOTO470          
          IDENT     STCH                 SUBROUTINE STCH(IVAL,ARRAY,NCH)PLOTO471          
          ENTRY     STCH                                                PLOTO472          
STCH      BSS       1                                                   PLOTO473          
          SA1       B3                   PICK UP NCH                    PLOTO474          
          SX1       X1-1                 DECREMENT CHARACTER POSITION   PLOTO475          
          PX0       B0,X1                PACK NCH                       PLOTO476          
          SA2       =10.0                10 CHARACTERS/WORD=NCRW        PLOTO477          
          FX2       X0/X2                WORD COUNT=NCH/NCRW            PLOTO478          
          UX2       B4,X2                UNPACK WORD COUNT              PLOTO479          
          LX2       B4,X2                INTEGERIZE WORD COUNT          PLOTO480          
          SB4       X2                   WORD COUNT IN B-REG 4          PLOTO481          
          LX2       1                                                   PLOTO482          
          BX0       X2                                                  PLOTO483          
          LX0       2                                                   PLOTO484          
          IX2       X2+X0                WORD COUNT*10                  PLOTO485          
          IX2       X2-X1                -(REMAINDER)                   PLOTO486          
          SX2       X2+9                 CHARACTER COUNT FROM RIGHT     PLOTO487          
          LX2       1                                                   PLOTO488          
          BX0       X2                                                  PLOTO489          
          LX0       1                                                   PLOTO490          
          IX2       X2+X0                BIT COUNT CHARACTER COUNT*6    PLOTO491          
          SB5       X2                   IN B-REG 5                     PLOTO492          
          SA1       B4+B2                PICK UP ARRAY ELEMENT=BUF      PLOTO493          
          SX2       77B                  CHARACTER MASK                 PLOTO494          
          SA3       B1                   PICK UP IVAL                   PLOTO495          
          LX3       6                    SHIFT IVAL TO RIGHT MOST BITS  PLOTO496          
          BX3       X3*X2                IVAL=AND(IVAL,MASK)            PLOTO497          
          LX3       B5,X3                IVAL=LEFTSHIFT(IVAL,BIT COUNT) PLOTO498          
          LX2       B5,X2                MASK=LEFTSHIFT(MASK,BIT COUNT) PLOTO499          
          BX1       -X2*X1               BUF=AND(NOT(MASK),BUF)         PLOTO500          
          BX6       X1+X3                BUF=OR(BUF,MASK)               PLOTO501          
          SA6       B4+B2                ARRAY ELEMENT=BUF              PLOTO502          
          JP        STCH                                                PLOTO503          
          END                                                           PLOTO504          
          IDENT     LDCH                 .FUNCTION LDCH (ARRAY,NCH)     PLOTO505          
          ENTRY     LDCH                                                PLOTO506          
LDCH      BSS       1                                                   PLOTO507          
          SA1       B2                   PICK UP NCH                    PLOTO508          
          SX1       X1-1                 DECREMENT CHARACTER POSITION   PLOTO509          
          PX0       B0,X1                PACK NCH                       PLOTO510          
          SA2       =10.0                10 CHARACTERS/WORD=NCRW        PLOTO511          
          FX2       X0/X2                WORD COUNT=NCH/NCRW            PLOTO512          
          UX2       B3,X2                UNPACK WORD COUNT              PLOTO513          
          LX2       B3,X2                INTEGERIZE WORD COUNT          PLOTO514          
          SB3       X2                   WORD COUNT IN B-REG 3          PLOTO515          
          LX2       1                                                   PLOTO516          
          BX0       X2                                                  PLOTO517          
          LX0       2                                                   PLOTO518          
          IX2       X2+X0                WORD COUNT*10                  PLOTO519          
          IX2       X2-X1                -(REMAINDER)                   PLOTO520          
          SX2       X2+9                 CHARACTER COUNT FROM RIGHT     PLOTO521          
          LX2       1                                                   PLOTO522          
          BX0       X2                                                  PLOTO523          
          LX0       1                                                   PLOTO524          
          IX2       X2+X0                BIT COUNT=CHARACTER COUNT*6    PLOTO525          
          SB4       X2                   IN B-REG 4                     PLOTO526          
          SA1       B3+B1                PICK UP ARRAY ELEMENT=BUF      PLOTO527          
          SX2       77B                  CHARACTER MASK                 PLOTO528          
          AX3       B4,X1                BUF=RIGHTSHIFT(BUF,BIT COUNT)  PLOTO529          
          BX6       X3*X2                BUF=AND(BUF,MASK)              PLOTO530          
          LX6       54                   SHIFT CHARACTER TO LEFT-MOST BIPLOTO531          
          SA2       =9R                  PICK UP BLANK MASK             PLOTO532          
          BX6       X6+X2                BUF=OR(BUF,BLANK MASK)         PLOTO533          
          JP        LDCH                                                PLOTO534          
          END                                                           PLOTO535          
