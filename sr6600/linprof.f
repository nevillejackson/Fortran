      PROGRAM LINPROF(INPUT,OUTPUT,TAPE60=INPUT,TAPE61=OUTPUT)          LINEP002          
      DIMENSION TITLE(10),SUBITLE(10),H(100),G(100),F(100),FR(100),FI(  LINEP003          
     .100),IL(3)                                                        LINEP004          
C     STOKES NUMERICAL FOURIER ANALYSIS METHOD FOR THE CORRECTION OF    LINEP005          
C     LINE PROFILES ON X-RAY POWDER DIFFRACTION PATTERNS                LINEP006          
C     REFERENCE STOKES PROC PHYS SOC LONDON 6 382-391  1948             LINEP007          
C     PROGRAM ORIGINALLY WRITTEN BY RADOSLOVITCH AND KATO,DIVISION      LINEP008          
C         OF SOILS,C.S.I.R.O., ADELAIDE                                 LINEP009          
C    PROGRAM AMENDED AND MODIFIED BY P E CIDDOR,  N.S.L. FEB.1966       LINEP010          
C     GENERAL PURPOSE PROGRAM,SCALING AND PLOTTING FLEXIBLE.            LINEP011          
C    PROGRAM IS FULLY COMPATIBLE ON 3200 OR 3600. MARCH 7,1966          LINEP012          
C     DATA CARDS AS FOLLOWS                                             LINEP013          
C     N1  NO. OF SETS OF DATA                     I2                    LINEP014          
C     TITLE        80 COLS. HOLLERITH             10A8                  LINEP015          
C     SUBTITLE     80 COLS. HOLLERITH             10A8                  LINEP016          
C     IPLOTF,IPLOTG,IPLOTH,IPLOTFLD               4I1                   LINEP017          
C         SET TO 1 IF PLOT REQUIRED ,ELSE SET TO 0.                     LINEP018          
C     LAYOUT  IF=1,READ H,G PAIR PER CARD.      I1                      LINEP019          
C             IF=2,READ ALL H(15/CARD),THEN ALL G (15/CARD)             LINEP020          
C     N,M           N+1 POINTS,  M+1  FOURIER TERMS 2I3                 LINEP021          
C     H,G           H=OBS.PROFILE G=INSTR.PROFILE 2F5.0                 LINEP022          
C     ***OR***                                                          LINEP023          
C      H(J)  J=1,NN     (LAYOUT=2)                                      LINEP024          
C     G(J) J=1,NN   (LAYOUT=2)                    15F5.0                LINEP025          
      KQZB00=32                                                         LINEP026          
      KQZB06=8                                                          LINEP027          
      ENCODE(7,30,FOLTAG1)                                              LINEP028          
30    FORMAT(7H+=FOLD*)                                                 LINEP029          
      ENCODE(6,50,FOLTAG2)                                              LINEP030          
50    FORMAT(6HHSCALE)                                                  LINEP031          
      READ(60,70) N1                                                    LINEP032          
70    FORMAT(I2)                                                        LINEP033          
      III=1                                                             LINEP034          
81    IF(III.GT.N1) GOTO 89                                             LINEP035          
      READ(60,100) TITLE,SUBITLE                                        LINEP036          
100   FORMAT(10A8)                                                      LINEP037          
      WRITE(61,120) TITLE,SUBITLE                                       LINEP038          
120   FORMAT(1H1,10A8//10A8)                                            LINEP039          
      READ(60,140) IPLOTF,IPLOTG,IPLOTH,IPLTFLD                         LINEP040          
140   FORMAT(4I1)                                                       LINEP041          
      WRITE(61,160) IPLOTF,IPLOTG,IPLOTH,IPLTFLD                        LINEP042          
160   FORMAT(/5X,8HPLOT OPT,8HIONS  F=,I2,2X,2HG=,I2,2X,2HH=,I2,2X,     LINEP043          
     .5HFOLD=,I2//)                                                     LINEP044          
      READ(60,140) LAYOUT                                               LINEP045          
      READ(60,190) N,M                                                  LINEP046          
190   FORMAT(2I3)                                                       LINEP047          
      TWOPIN=6.2832/FLOAT(N)                                            LINEP048          
      WRITE(61,220) N,M                                                 LINEP049          
220   FORMAT(8H        ,8HN       ,3H  M/7X,I3,7X,I3)                   LINEP050          
      NN=N+1                                                            LINEP051          
      KQZ001=LAYOUT                                                     LINEP052          
      IF(KQZ001.LT.1) KQZ001=1                                          LINEP053          
      IF(KQZ001.GT.2) KQZ001=2                                          LINEP054          
      GOTO(250,290),KQZ001                                              LINEP055          
250   CONTINUE                                                          LINEP056          
      READ(60,270)(H(J),G(J),J=1,NN)                                    LINEP057          
270   FORMAT(2F5.0)                                                     LINEP058          
      GOTO 320                                                          LINEP059          
290   READ(60,310)(H(J),J=1,NN)                                         LINEP060          
      READ(60,310)(G(J),J=1,NN)                                         LINEP061          
310   FORMAT(15F5.0)                                                    LINEP062          
320   CONTINUE                                                          LINEP063          
      WRITE(61,340)(J,H(J),G(J),J=1,NN)                                 LINEP064          
340   FORMAT(10X,1HJ,10X,4HH(J),10X,4HG(J)/(8X,I3,9X,F5.0,9X,F5.0))     LINEP065          
      NF=1+N/2                                                          LINEP066          
      MM=M+1                                                            LINEP067          
      WRITE(61,120) TITLE,SUBITLE                                       LINEP068          
      WRITE(61,390)                                                     LINEP069          
390   FORMAT(/8H        ,8H K      ,8HHR      ,8H  HI    ,8H    GR  ,   LINEP070          
     .8H      GI,8H        ,8H FR     ,5H   FI)                         LINEP071          
      KK=1                                                              LINEP072          
401   IF(KK.GT.MM) GOTO 409                                             LINEP073          
      K=KK-1                                                            LINEP074          
      TWOPIK=TWOPIN*FLOAT(K)                                            LINEP075          
      THETA=-3.1416*FLOAT(K)-TWOPIK                                     LINEP076          
      GI=0                                                              LINEP077          
      GR=GI                                                             LINEP078          
      HI=GR                                                             LINEP079          
      HR=HI                                                             LINEP080          
      J=1                                                               LINEP081          
451   IF(J.GT.NN) GOTO 459                                              LINEP082          
      THETA=THETA+TWOPIK                                                LINEP083          
C     I.E. THETA =(J-1-N/2)*K*2*PI/N                                    LINEP084          
      COSINEN=COS(THETA)/FLOAT(N)                                       LINEP085          
      SINEN=SIN(THETA)/FLOAT(N)                                         LINEP086          
      HR=HR+H(J)*COSINEN                                                LINEP087          
      HI=HI+H(J)*SINEN                                                  LINEP088          
      GR=GR+G(J)*COSINEN                                                LINEP089          
      GI=GI+G(J)*SINEN                                                  LINEP090          
      J=J+1                                                             LINEP091          
      GOTO 451                                                          LINEP092          
459   CONTINUE                                                          LINEP093          
      FR(KK)=(HR*GR+HI*GI)/(GR**2+GI**2+1.E-12)                         LINEP094          
      FI(KK)=(HI*GR-HR*GI)/(GR**2+GI**2+1.E-12)                         LINEP095          
      WRITE(61,560) K,HR,HI,GR,GI,FR(KK),FI(KK)                         LINEP096          
560   FORMAT(7X,I3,6F10.2)                                              LINEP097          
      KK=KK+1                                                           LINEP098          
      GOTO 401                                                          LINEP099          
409   CONTINUE                                                          LINEP100          
      FR(1)=FR(1)/2E0                                                   LINEP101          
      WRITE(61,120) TITLE,SUBITLE                                       LINEP102          
      WRITE(61,610)                                                     LINEP103          
610   FORMAT(/10X,1HL,3X,4HF(L))                                        LINEP104          
      LL=1                                                              LINEP105          
621   IF(LL.GT.NN) GOTO 629                                             LINEP106          
      L=LL-1-N/2                                                        LINEP107          
      F(LL)=0.                                                          LINEP108          
      KK=1                                                              LINEP109          
651   IF(KK.GT.MM) GOTO 659                                             LINEP110          
      K=KK-1                                                            LINEP111          
      IF(FR(KK)) 700,700,680                                            LINEP112          
680   F(LL)=F(LL)+2E0*FR(KK)*COS(FLOAT(K)*2E0*3.1416*FLOAT(L)/FLOAT(N)) LINEP113          
     .+2E0*FI(KK)*SIN(FLOAT(K)*2E0*3.1416*FLOAT(L)/FLOAT(N))            LINEP114          
      KK=KK+1                                                           LINEP115          
      GOTO 651                                                          LINEP116          
659   CONTINUE                                                          LINEP117          
700   F(LL)=F(LL)/FLOAT(N)                                              LINEP118          
C   THIS IS CORRECT VALUE FOR F,NEEDED LATER(STATEMENT 2007)            LINEP119          
      WRITE(61,720) L,F(LL)                                             LINEP120          
720   FORMAT(6X,I5,E12.3)                                               LINEP121          
      LL=LL+1                                                           LINEP122          
      GOTO 621                                                          LINEP123          
629   CONTINUE                                                          LINEP124          
      CALL PLOT(1.,-0.2,1)                                              LINEP125          
      XS=(N-5)/10+1                                                     LINEP126          
      DX=0.3*XS $ DY=0.04                                               LINEP127          
      CALL PLOT(XS,0.1,2)                                               LINEP128          
      CALL PLOT(XS,-0.2,3)                                              LINEP129          
      CALL TEXT(SUBITLE,80,2)                                           LINEP130          
      NLEFT=-N/2                                                        LINEP131          
      NZERO=0                                                           LINEP132          
      NRIGHT=N/2                                                        LINEP133          
      CALL PLOT(1.-DX,-DY,3)                                            LINEP134          
      ENCODE(4,860,IUF) NLEFT                                           LINEP135          
860   FORMAT(I4)                                                        LINEP136          
      CALL TEXT(IUF,4,2)                                                LINEP137          
      CALL PLOT(-DX+FLOAT(1+N/2),-DY,3)                                 LINEP138          
      ENCODE(4,860,IUF) NZERO                                           LINEP139          
      CALL TEXT(IUF,4,2)                                                LINEP140          
      CALL PLOT(-DX+FLOAT(NN),-DY,3)                                    LINEP141          
      ENCODE(4,860,IUF) NRIGHT                                          LINEP142          
      CALL TEXT(IUF,4,2)                                                LINEP143          
      CALL PLOT(1.0,0.0,3)                                              LINEP144          
      CALL PLOT(1.0,1.0,4)                                              LINEP145          
      CALL PLOT(1.0-DX,1.0,3)                                           LINEP146          
      NY=1.0                                                            LINEP147          
      ENCODE(4,860,IUF) NY                                              LINEP148          
      CALL TEXT(IUF,4,2)                                                LINEP149          
      CALL PLOT(1.0,0.0,3)                                              LINEP150          
      CALL PLOT(FLOAT(NN),0.0,4)                                        LINEP151          
      CALL PLOT(FLOAT(NN),1.0,4)                                        LINEP152          
      CALL PLOT(FLOAT(NN)-DX,1.0,3)                                     LINEP153          
      CALL TEXT(IUF,4,2)                                                LINEP154          
      CALL HILO(F,NN,FI,SM,IFI,ISM) $ FSCALE=1.0/FI(1)                  LINEP155          
      GSCALE=1./G(NF)                                                   LINEP156          
      HSCALE=1E0/H(NF)                                                  LINEP157          
C     SCALE FACTORS REDUCE F,G,H TO UNIT HEIGHT IN PLOTS.               LINEP158          
C     PLOT F,G,H AGAINST LL IF REQUIRED                                 LINEP159          
      DX=0.                                                             LINEP160          
C     IDENTIFYING SYMBOL LOCATED AT PLOTTED POINT.FOR OFFSET,SET DX NE 0LINEP161          
      IF(IPLOTH) 1120,1120,1110                                         LINEP162          
1110  CALL TAGPLOT(H,NN,1HH,DX,HSCALE)                                  LINEP163          
1120  IF(IPLOTG) 1140,1140,1130                                         LINEP164          
1130  CALL TAGPLOT(G,NN,1HG,DX,GSCALE)                                  LINEP165          
1140  IF(IPLOTF) 1160,1160,1150                                         LINEP166          
1150  CALL TAGPLOT(F,NN,1HF,DX,FSCALE)                                  LINEP167          
1160  CONTINUE                                                          LINEP168          
      WRITE(61,120) TITLE,SUBITLE                                       LINEP169          
      WRITE(61,1190)                                                    LINEP170          
1190  FORMAT(//5X,8HBACK-FOR,8HMATION O,8HF FOLD F,8H(*)G FOR,          LINEP171          
     .8H COMPARI,4HSON //5X,2HLL,8X,1HH,8X,4HFOLD,6X,4HDIFF/)           LINEP172          
      L=1                                                               LINEP173          
1201  IF(L.GT.NN) GOTO 1209                                             LINEP174          
      LL=L-1-N/2                                                        LINEP175          
      FOLD=0.0                                                          LINEP176          
      IF(.NOT.(L.LT.NF)) GOTO 1280                                      LINEP177          
      JL=1 $ JU=L+N/2 $ JS=L+N/2+1                                      LINEP178          
      GOTO 1360                                                         LINEP179          
1280  IF(.NOT.(L.EQ.NF)) GOTO 1330                                      LINEP180          
      JL=1 $ JU=NN $ JS=N+2                                             LINEP181          
      GOTO 1360                                                         LINEP182          
1330  JL=L-N/2 $ JU=NN $ JS=L+N/2+1                                     LINEP183          
1360  J=JL                                                              LINEP184          
1361  IF(J.GT.JU) GOTO 1369                                             LINEP185          
      J1=JS-J                                                           LINEP186          
      FOLD=FOLD+F(J)*G(J1)                                              LINEP187          
      J=J+1                                                             LINEP188          
      GOTO 1361                                                         LINEP189          
1369  CONTINUE                                                          LINEP190          
      DIFF=FOLD-H(L)                                                    LINEP191          
      WRITE(61,1410) LL,H(L),FOLD,DIFF                                  LINEP192          
1410  FORMAT(4X,I4,3F10.2)                                              LINEP193          
      IF(IPLTFLD) 1450,1450,1430                                        LINEP194          
C     PLOT FOLD*HSCALE IF REQUIRED.                                     LINEP195          
1430  CALL PLOT(FLOAT(L),FOLD*HSCALE,3)                                 LINEP196          
      CALL TEXT(1H+,1,1)                                                LINEP197          
1450  CONTINUE                                                          LINEP198          
      L=L+1                                                             LINEP199          
      GOTO 1201                                                         LINEP200          
1209  CONTINUE                                                          LINEP201          
      CALL PLOT(1.0+XS/2.0,0.9,3)                                       LINEP202          
      CALL TEXT(FOLTAG1,7,2)                                            LINEP203          
      CALL TEXT(FOLTAG2,6,2)                                            LINEP204          
      CALL PLOT(1.0,2.0,3)                                              LINEP205          
      III=III+1                                                         LINEP206          
      GOTO 81                                                           LINEP207          
89    CONTINUE                                                          LINEP208          
      END                                                               LINEP209          
      SUBROUTINE HILO(X,N,HI,SM,INDEXHI,INDEXSM)                        LINEP210          
      DIMENSION X(1)                                                    LINEP211          
C     THIS ROUTINE WILL RETURN THE ALGEBRAICALLY LARGEST AND            LINEP212          
C         SMALLEST ELEMENTS OF  A  VECTOR X                             LINEP213          
      HI=-1E10                                                          LINEP214          
      SM=1E10                                                           LINEP215          
      I=1                                                               LINEP216          
41    IF(I.GT.N) GOTO 49                                                LINEP217          
      IF(.NOT.(X(I).GT.HI)) GOTO 90                                     LINEP218          
      HI=X(I)                                                           LINEP219          
      INDEXHI=I                                                         LINEP220          
      GOTO 120                                                          LINEP221          
90    IF(.NOT.(X(I).LT.SM)) GOTO 120                                    LINEP222          
      SM=X(I)                                                           LINEP223          
      INDEXSM=I                                                         LINEP224          
120   CONTINUE                                                          LINEP225          
      I=I+1                                                             LINEP226          
      GOTO 41                                                           LINEP227          
49    CONTINUE                                                          LINEP228          
      RETURN                                                            LINEP229          
      END                                                               LINEP230          
      SUBROUTINE TAGPLOT(V,NV,ITAG,DX,VSCALE)                           LINEP231          
      DIMENSION V(100)                                                  LINEP232          
C     PRODUCES LABELLED LINEPLOT.ITAG=1 ALPHANUMERIC,DX=SEPARATION OF TALINEP233          
      ENCODE(1,30,IBUF) ITAG                                            LINEP234          
30    FORMAT(A1)                                                        LINEP235          
      CALL PLOT(1.+DX,V(1)*VSCALE,3)                                    LINEP236          
      CALL TEXT(IBUF,1,1)                                               LINEP237          
      CALL PLOT(1.,V(1)*VSCALE,3)                                       LINEP238          
      I=2                                                               LINEP239          
71    IF(I.GT.NV) GOTO 79                                               LINEP240          
      XI=I                                                              LINEP241          
      VS=V(I)*VSCALE                                                    LINEP242          
      CALL PLOT(XI,VS,4)                                                LINEP243          
      CALL PLOT(XI+DX,VS,3)                                             LINEP244          
      CALL TEXT(IBUF,1,1)                                               LINEP245          
      CALL PLOT(XI,VS,3)                                                LINEP246          
      I=I+1                                                             LINEP247          
      GOTO 71                                                           LINEP248          
79    CONTINUE                                                          LINEP249          
      RETURN                                                            LINEP250          
      END                                                               LINEP251          
