      SUBROUTINE POWELL(X,E,N,F,ESCALE,IPRINT,ICON,MAXIT,W)             POWEL002          
      DIMENSION W(1),X(1),E(1)                                          POWEL003          
C                                                                       POWEL004          
C MINIMISATION OF FUNCTION OF SEVERAL VARIABLES                         POWEL005          
C AUTHOR M.J.D.POWELL,AERE,HARWELL,U.K.                                 POWEL006          
C ADAPTED BY K.SERKOWSKA,C.S.I.R.O.,CANBERRA,DECEMBER,1968              POWEL007          
C                                                                       POWEL008          
C X IS ARRAY OF VARIABLES-ON ENTRY,INITIAL APPROX-ON EXIT,SOLUTION      POWEL009          
C E IS ACCURACY REQUIRED(DIMENSION OF E AND X IS N-NUMBER OF VARS.)     POWEL010          
C ESCALE LIMITS MAXIMUM CHANGE IN VAR. TO E(J)*ESCALE                   POWEL011          
C IPRINT=0 FOR NO PRINTING,=1 PRINT EVERY SEARCH,=2 EVERY ITERATION     POWEL012          
C ICON CONTROLS ULTIMATE CONVERGENCE CRITERION-NORMALLY SET TO 1        POWEL013          
C MAXIT-ON ENTRY MAX NUMBER OF ITERATINS-ON EXIT NUMBER ACTUALLY USED   POWEL014          
C W IS WORKING SPACE-DIMENSION AT LEAST 3N+N**2                         POWEL015          
C                                                                       POWEL016          
      DDMAG=0.1*ESCALE                                                  POWEL017          
      SCER=0.05/ESCALE                                                  POWEL018          
      JJ=N*N+N                                                          POWEL019          
      JJJ=JJ+N                                                          POWEL020          
      K=N+1                                                             POWEL021          
      NFCC=1                                                            POWEL022          
      IND=1                                                             POWEL023          
      INN=1                                                             POWEL024          
      I=1                                                               POWEL025          
101   IF(I.GT.N) GOTO 109                                               POWEL026          
      J=1                                                               POWEL027          
111   IF(J.GT.N) GOTO 119                                               POWEL028          
      W(K)=0.                                                           POWEL029          
      IF(I-J) 160,140,160                                               POWEL030          
140   W(K)=ABS(E(I))                                                    POWEL031          
      W(I)=ESCALE                                                       POWEL032          
160   K=K+1                                                             POWEL033          
      J=J+1                                                             POWEL034          
      GOTO 111                                                          POWEL035          
119   CONTINUE                                                          POWEL036          
      I=I+1                                                             POWEL037          
      GOTO 101                                                          POWEL038          
109   CONTINUE                                                          POWEL039          
      ITERC=1                                                           POWEL040          
      ISGRAD=2                                                          POWEL041          
      CALL GIVEF(X,F)                                                   POWEL042          
      FKEEP=ABS(F)+ABS(F)                                               POWEL043          
230   ITONE=1                                                           POWEL044          
      FP=F                                                              POWEL045          
      SLM=0                                                             POWEL046          
      IXP=JJ                                                            POWEL047          
      I=1                                                               POWEL048          
271   IF(I.GT.N) GOTO 279                                               POWEL049          
      IXP=IXP+1                                                         POWEL050          
      W(IXP)=X(I)                                                       POWEL051          
      I=I+1                                                             POWEL052          
      GOTO 271                                                          POWEL053          
279   CONTINUE                                                          POWEL054          
      IDIRN=N+1                                                         POWEL055          
      ILINE=1                                                           POWEL056          
330   DMAX=W(ILINE)                                                     POWEL057          
      DACC=DMAX*SCER                                                    POWEL058          
      DMAG=AMIN1(DDMAG,0.1*DMAX)                                        POWEL059          
      DMAG=AMAX1(DMAG,20.*DACC)                                         POWEL060          
      DDMAX=10.*DMAG                                                    POWEL061          
      KQZ001=ITONE                                                      POWEL062          
      IF(KQZ001.LT.1) KQZ001=1                                          POWEL063          
      IF(KQZ001.GT.3) KQZ001=3                                          POWEL064          
      GOTO(390,390,990),KQZ001                                          POWEL065          
390   DL=0.                                                             POWEL066          
      D=DMAG                                                            POWEL067          
      FPREV=F                                                           POWEL068          
      IS=5                                                              POWEL069          
      FA=F                                                              POWEL070          
      DA=DL                                                             POWEL071          
450   DD=D-DL                                                           POWEL072          
      DL=D                                                              POWEL073          
470   K=IDIRN                                                           POWEL074          
      I=1                                                               POWEL075          
481   IF(I.GT.N) GOTO 489                                               POWEL076          
      X(I)=X(I)+DD*W(K)                                                 POWEL077          
      K=K+1                                                             POWEL078          
      I=I+1                                                             POWEL079          
      GOTO 481                                                          POWEL080          
489   CONTINUE                                                          POWEL081          
      CALL GIVEF(X,F)                                                   POWEL082          
      NFCC=NFCC+1                                                       POWEL083          
      KQZ001=IS                                                         POWEL084          
      IF(KQZ001.LT.1) KQZ001=1                                          POWEL085          
      IF(KQZ001.GT.6) KQZ001=6                                          POWEL086          
      GOTO(1060,950,910,850,550,1660),KQZ001                            POWEL087          
550   IF(F-FA) 620,560,650                                              POWEL088          
560   IF(ABS(D)-DMAX) 570,570,590                                       POWEL089          
570   D=D+D                                                             POWEL090          
      GOTO 450                                                          POWEL091          
590   WRITE(61,600)                                                     POWEL092          
600   FORMAT(5X,8HPOWELL M,8HAXIMUM C,8HHANGE DO,8HES NOT A,8HLTER FUN, POWEL093          
     .5HCTION)                                                          POWEL094          
      GOTO 2620                                                         POWEL095          
620   FB=F                                                              POWEL096          
      DB=D                                                              POWEL097          
      GOTO 690                                                          POWEL098          
650   FB=FA                                                             POWEL099          
      DB=DA                                                             POWEL100          
      FA=F                                                              POWEL101          
      DA=D                                                              POWEL102          
690   KQZ001=ISGRAD                                                     POWEL103          
      IF(KQZ001.LT.1) KQZ001=1                                          POWEL104          
      IF(KQZ001.GT.2) KQZ001=2                                          POWEL105          
      GOTO(730,700),KQZ001                                              POWEL106          
700   D=DB+DB-DA                                                        POWEL107          
      IS=1                                                              POWEL108          
      GOTO 450                                                          POWEL109          
730   D=0.5*(DA+DB-(FA-FB)/(DA-DB))                                     POWEL110          
      IS=4                                                              POWEL111          
      IF((DA-D)*(D-DB)) 760,450,450                                     POWEL112          
760   IS=1                                                              POWEL113          
      IF(ABS(D-DB)-DDMAX) 450,450,780                                   POWEL114          
780   D=DB+SIGN(DDMAX,DB-DA)                                            POWEL115          
      IS=1                                                              POWEL116          
      DDMAX=DDMAX+DDMAX                                                 POWEL117          
      DDMAG=DDMAG+DDMAG                                                 POWEL118          
      IF(DDMAX-DMAX) 450,450,830                                        POWEL119          
830   DDMAX=DMAX                                                        POWEL120          
      GOTO 450                                                          POWEL121          
850   IF(F-FA) 860,700,700                                              POWEL122          
860   FC=FB                                                             POWEL123          
      DC=DB                                                             POWEL124          
880   FB=F                                                              POWEL125          
      DB=D                                                              POWEL126          
      GOTO 1080                                                         POWEL127          
910   IF(F-FB) 860,860,920                                              POWEL128          
920   FA=F                                                              POWEL129          
      DA=D                                                              POWEL130          
      GOTO 1080                                                         POWEL131          
950   IF(F-FB) 960,1060,1060                                            POWEL132          
960   FA=FB                                                             POWEL133          
      DA=DB                                                             POWEL134          
      GOTO 880                                                          POWEL135          
990   DL=1.                                                             POWEL136          
      DDMAX=5.                                                          POWEL137          
      FA=FP                                                             POWEL138          
      DA=-1.                                                            POWEL139          
      FB=FHOLD                                                          POWEL140          
      DB=0.                                                             POWEL141          
      D=1.                                                              POWEL142          
1060  FC=F                                                              POWEL143          
      DC=D                                                              POWEL144          
1080  A=(DB-DC)*(FA-FC)                                                 POWEL145          
      B=(DC-DA)*(FB-FC)                                                 POWEL146          
      IF((A+B)*(DA-DC)) 1110,1110,1160                                  POWEL147          
1110  FA=FB                                                             POWEL148          
      DA=DB                                                             POWEL149          
      FB=FC                                                             POWEL150          
      DB=DC                                                             POWEL151          
      GOTO 780                                                          POWEL152          
1160  D=0.5*(A*(DB+DC)+B*(DA+DC))/(A+B)                                 POWEL153          
      DI=DB                                                             POWEL154          
      FI=FB                                                             POWEL155          
      IF(FB-FC) 1220,1220,1200                                          POWEL156          
1200  DI=DC                                                             POWEL157          
      FI=FC                                                             POWEL158          
1220  KQZ001=ITONE                                                      POWEL159          
      IF(KQZ001.LT.1) KQZ001=1                                          POWEL160          
      IF(KQZ001.GT.3) KQZ001=3                                          POWEL161          
      GOTO(1250,1250,1230),KQZ001                                       POWEL162          
1230  ITONE=2                                                           POWEL163          
      GOTO 1270                                                         POWEL164          
1250  IF(ABS(D-DI)-DACC) 1370,1370,1260                                 POWEL165          
1260  IF(ABS(D-DI)-0.03*ABS(D)) 1370,1370,1270                          POWEL166          
1270  IF((DA-DC)*(DC-D)) 1330,1280,1280                                 POWEL167          
1280  FA=FB                                                             POWEL168          
      DA=DB                                                             POWEL169          
      FB=FC                                                             POWEL170          
      DB=DC                                                             POWEL171          
      GOTO 760                                                          POWEL172          
1330  IS=2                                                              POWEL173          
      IF((DB-D)*(D-DC)) 1350,450,450                                    POWEL174          
1350  IS=3                                                              POWEL175          
      GOTO 450                                                          POWEL176          
1370  F=FI                                                              POWEL177          
      D=DI-DL                                                           POWEL178          
      DD=SQRT((DC-DB)*(DC-DA)*(DA-DB)/(A+B))                            POWEL179          
      I=1                                                               POWEL180          
1401  IF(I.GT.N) GOTO 1409                                              POWEL181          
      X(I)=X(I)+D*W(IDIRN)                                              POWEL182          
      W(IDIRN)=DD*W(IDIRN)                                              POWEL183          
      IDIRN=IDIRN+1                                                     POWEL184          
      I=I+1                                                             POWEL185          
      GOTO 1401                                                         POWEL186          
1409  CONTINUE                                                          POWEL187          
      W(ILINE)=W(ILINE)/DD                                              POWEL188          
      ILINE=ILINE+1                                                     POWEL189          
      IF(IPRINT-1) 1510,1480,1510                                       POWEL190          
1480  WRITE(61,1490) ITERC,NFCC,F,(X(I),I=1,N)                          POWEL191          
1490  FORMAT(/1X,8HITERATIO,1HN,I5,I15,8H FUNCTIO,8HN VALUES,10X,3HF =, POWEL192          
     .E21.14/(5E24.14))                                                 POWEL193          
      KQZ001=IPRINT                                                     POWEL194          
      IF(KQZ001.LT.1) KQZ001=1                                          POWEL195          
      IF(KQZ001.GT.2) KQZ001=2                                          POWEL196          
      GOTO(1510,2080),KQZ001                                            POWEL197          
1510  KQZ001=ITONE                                                      POWEL198          
      IF(KQZ001.LT.1) KQZ001=1                                          POWEL199          
      IF(KQZ001.GT.2) KQZ001=2                                          POWEL200          
      GOTO(1520,2050),KQZ001                                            POWEL201          
1520  IF(FPREV-F-SUM) 1550,1530,1530                                    POWEL202          
1530  SUM=FPREV-F                                                       POWEL203          
      JIL=ILINE                                                         POWEL204          
1550  IF(IDIRN-JJ) 330,330,1560                                         POWEL205          
1560  KQZ001=IND                                                        POWEL206          
      IF(KQZ001.LT.1) KQZ001=1                                          POWEL207          
      IF(KQZ001.GT.2) KQZ001=2                                          POWEL208          
      GOTO(1570,2070),KQZ001                                            POWEL209          
1570  FHOLD=F                                                           POWEL210          
      IS=6                                                              POWEL211          
      IXP=JJ                                                            POWEL212          
      I=1                                                               POWEL213          
1601  IF(I.GT.N) GOTO 1609                                              POWEL214          
      IXP=IXP+1                                                         POWEL215          
      W(IXP)=X(I)-W(IXP)                                                POWEL216          
      I=I+1                                                             POWEL217          
      GOTO 1601                                                         POWEL218          
1609  CONTINUE                                                          POWEL219          
      DD=1                                                              POWEL220          
      GOTO 470                                                          POWEL221          
1660  KQZ001=IND                                                        POWEL222          
      IF(KQZ001.LT.1) KQZ001=1                                          POWEL223          
      IF(KQZ001.GT.2) KQZ001=2                                          POWEL224          
      GOTO(1670,1700),KQZ001                                            POWEL225          
1670  IF(FP-F) 1950,1950,1680                                           POWEL226          
1680  D=2.*(FP+F-2.*FHOLD)/(FP-F)**2                                    POWEL227          
      IF(D*(FP-FHOLD-SUM)**2-SUM) 1700,1950,1950                        POWEL228          
1700  J=JIL*N+1                                                         POWEL229          
      IF(J-JJ) 1720,1720,1790                                           POWEL230          
1720  I=J                                                               POWEL231          
1721  IF(I.GT.JJ) GOTO 1729                                             POWEL232          
      K=I-N                                                             POWEL233          
      W(K)=W(I)                                                         POWEL234          
      I=I+1                                                             POWEL235          
      GOTO 1721                                                         POWEL236          
1729  CONTINUE                                                          POWEL237          
      I=JIL                                                             POWEL238          
1761  IF(I.GT.N) GOTO 1769                                              POWEL239          
      W(I-1)=W(I)                                                       POWEL240          
      I=I+1                                                             POWEL241          
      GOTO 1761                                                         POWEL242          
1769  CONTINUE                                                          POWEL243          
1790  IDIRN=IDIRN-N                                                     POWEL244          
      ITONE=3                                                           POWEL245          
      K=IDIRN                                                           POWEL246          
      IXP=JJ                                                            POWEL247          
      AAA=0.                                                            POWEL248          
      I=1                                                               POWEL249          
1841  IF(I.GT.N) GOTO 1849                                              POWEL250          
      IXP=IXP+1                                                         POWEL251          
      W(K)=W(IXP)                                                       POWEL252          
      IF(AAA-ABS(W(K)/E(I))) 1880,1890,1890                             POWEL253          
1880  AAA=ABS(W(K)/E(I))                                                POWEL254          
1890  K=K+1                                                             POWEL255          
      I=I+1                                                             POWEL256          
      GOTO 1841                                                         POWEL257          
1849  CONTINUE                                                          POWEL258          
      DDMAG=1                                                           POWEL259          
      W(N)=ESCALE/AAA                                                   POWEL260          
      ILINE=N                                                           POWEL261          
      GOTO 330                                                          POWEL262          
1950  IXP=JJ                                                            POWEL263          
      AAA=0                                                             POWEL264          
      F=FHOLD                                                           POWEL265          
      I=1                                                               POWEL266          
1981  IF(I.GT.N) GOTO 1989                                              POWEL267          
      IXP=IXP+1                                                         POWEL268          
      X(I)=X(I)-W(IXP)                                                  POWEL269          
      IF(AAA*ABS(E(I))-ABS(W(IXP))) 2020,2030,2030                      POWEL270          
2020  AAA=ABS(W(IXP)/E(I))                                              POWEL271          
2030  CONTINUE                                                          POWEL272          
      I=I+1                                                             POWEL273          
      GOTO 1981                                                         POWEL274          
1989  CONTINUE                                                          POWEL275          
      GOTO 2070                                                         POWEL276          
2050  AAA=AAA*(1.+DI)                                                   POWEL277          
      KQZ001=IND                                                        POWEL278          
      IF(KQZ001.LT.1) KQZ001=1                                          POWEL279          
      IF(KQZ001.GT.2) KQZ001=2                                          POWEL280          
      GOTO(2070,2610),KQZ001                                            POWEL281          
2070  IF(IPRINT-2) 2080,1480,1480                                       POWEL282          
2080  KQZ001=IND                                                        POWEL283          
      IF(KQZ001.LT.1) KQZ001=1                                          POWEL284          
      IF(KQZ001.GT.2) KQZ001=2                                          POWEL285          
      GOTO(2090,2290),KQZ001                                            POWEL286          
2090  IF(AAA-0.1) 2100,2100,2250                                        POWEL287          
2100  KQZ001=ICON                                                       POWEL288          
      IF(KQZ001.LT.1) KQZ001=1                                          POWEL289          
      IF(KQZ001.GT.2) KQZ001=2                                          POWEL290          
      GOTO(2620,2110),KQZ001                                            POWEL291          
2110  IND=2                                                             POWEL292          
      KQZ001=INN                                                        POWEL293          
      IF(KQZ001.LT.1) KQZ001=1                                          POWEL294          
      IF(KQZ001.GT.2) KQZ001=2                                          POWEL295          
      GOTO(2130,2430),KQZ001                                            POWEL296          
2130  INN=2                                                             POWEL297          
      K=JJJ                                                             POWEL298          
      I=1                                                               POWEL299          
2151  IF(I.GT.N) GOTO 2159                                              POWEL300          
      K=K+1                                                             POWEL301          
      W(K)=X(I)                                                         POWEL302          
      X(I)=X(I)+10.*E(I)                                                POWEL303          
      I=I+1                                                             POWEL304          
      GOTO 2151                                                         POWEL305          
2159  CONTINUE                                                          POWEL306          
      FKEEP=F                                                           POWEL307          
      CALL GIVEF(X,F)                                                   POWEL308          
      NFCC=NFCC+1                                                       POWEL309          
      DDMAG=0.                                                          POWEL310          
      GOTO 2320                                                         POWEL311          
2250  IF(F-FP) 2300,2260,2260                                           POWEL312          
2260  WRITE(61,2270)                                                    POWEL313          
2270  FORMAT(5X,8HPOWELL A,8HCCURACY ,8HLIMITED ,8HBY ERROR,6HS IN F)   POWEL314          
      GOTO 2620                                                         POWEL315          
2290  IND=1                                                             POWEL316          
2300  DDMAG=0.4*SQRT(FP-F)                                              POWEL317          
      ISGRAD=1                                                          POWEL318          
2320  ITERC=ITERC+1                                                     POWEL319          
      IF(ITERC-MAXIT) 230,230,2340                                      POWEL320          
2340  WRITE(61,2350) MAXIT                                              POWEL321          
2350  FORMAT(I5,8H ITERATI,8HONS COMP,8HLETED BY,7H POWELL)             POWEL322          
      IF(F-FKEEP) 2620,2620,2370                                        POWEL323          
2370  F=FKEEP                                                           POWEL324          
      I=1                                                               POWEL325          
2381  IF(I.GT.N) GOTO 2389                                              POWEL326          
      JJJ=JJJ+1                                                         POWEL327          
      X(I)=W(JJJ)                                                       POWEL328          
      I=I+1                                                             POWEL329          
      GOTO 2381                                                         POWEL330          
2389  CONTINUE                                                          POWEL331          
      GOTO 2620                                                         POWEL332          
2430  JIL=1                                                             POWEL333          
      FP=FKEEP                                                          POWEL334          
      IF(F-FKEEP) 2490,2260,2460                                        POWEL335          
2460  JIL=2                                                             POWEL336          
      FP=F                                                              POWEL337          
      F=FKEEP                                                           POWEL338          
2490  IXP=JJ                                                            POWEL339          
      I=1                                                               POWEL340          
2501  IF(I.GT.N) GOTO 2509                                              POWEL341          
      IXP=IXP+1                                                         POWEL342          
      K=IXP+N                                                           POWEL343          
      KQZ001=JIL                                                        POWEL344          
      IF(KQZ001.LT.1) KQZ001=1                                          POWEL345          
      IF(KQZ001.GT.2) KQZ001=2                                          POWEL346          
      GOTO(2540,2560),KQZ001                                            POWEL347          
2540  W(IXP)=W(K)                                                       POWEL348          
      GOTO 2580                                                         POWEL349          
2560  W(IXP)=X(I)                                                       POWEL350          
      X(I)=W(K)                                                         POWEL351          
2580  CONTINUE                                                          POWEL352          
      I=I+1                                                             POWEL353          
      GOTO 2501                                                         POWEL354          
2509  CONTINUE                                                          POWEL355          
      JIL=2                                                             POWEL356          
      GOTO 1570                                                         POWEL357          
2610  IF(AAA-0.1) 2620,2620,2630                                        POWEL358          
2620  GOTO 2650                                                         POWEL359          
2630  INN=1                                                             POWEL360          
      GOTO 2300                                                         POWEL361          
2650  RETURN                                                            POWEL362          
      END                                                               POWEL363          
