      SUBROUTINE LSQFUN(M,N,F,X,E,ESCALE,IPRINT,MAXFUN,W)               LSQFU002          
      DIMENSION F(1),X(1),E(1),W(1)                                     LSQFU003          
C5 CSIR LSQFUN - MINIMIZATION OF SUM OF SQUARES OF FUNCTIONS OF         LSQFU004          
C     SEVERAL VARIABLES                                                 LSQFU005          
C  P.J.ROSS, CSIRO BRISBANE, SEPTEMBER 1969                             LSQFU006          
      MPLUSN=M+N                                                        LSQFU007          
      KST=N+MPLUSN                                                      LSQFU008          
      NPLUS=N+1                                                         LSQFU009          
      KINV=NPLUS*(MPLUSN+1)                                             LSQFU010          
      KSTORE=KINV-MPLUSN-1                                              LSQFU011          
      CALL GIVEF(X,F)                                                   LSQFU012          
      NN=N+N                                                            LSQFU013          
      K=NN                                                              LSQFU014          
      I=1                                                               LSQFU015          
101   IF(I.GT.M) GOTO 109                                               LSQFU016          
      K=K+1                                                             LSQFU017          
      W(K)=F(I)                                                         LSQFU018          
      I=I+1                                                             LSQFU019          
      GOTO 101                                                          LSQFU020          
109   CONTINUE                                                          LSQFU021          
      IINV=2                                                            LSQFU022          
      K=KST                                                             LSQFU023          
      I=1                                                               LSQFU024          
170   X(I)=X(I)+E(I)                                                    LSQFU025          
      CALL GIVEF(X,F)                                                   LSQFU026          
      X(I)=X(I)-E(I)                                                    LSQFU027          
      J=1                                                               LSQFU028          
201   IF(J.GT.N) GOTO 209                                               LSQFU029          
      K=K+1                                                             LSQFU030          
      W(K)=0.                                                           LSQFU031          
      W(J)=0.                                                           LSQFU032          
      J=J+1                                                             LSQFU033          
      GOTO 201                                                          LSQFU034          
209   CONTINUE                                                          LSQFU035          
      SUM=0.                                                            LSQFU036          
      KK=NN                                                             LSQFU037          
      J=1                                                               LSQFU038          
271   IF(J.GT.M) GOTO 279                                               LSQFU039          
      KK=KK+1                                                           LSQFU040          
      F(J)=F(J)-W(KK)                                                   LSQFU041          
      SUM=SUM+F(J)*F(J)                                                 LSQFU042          
      J=J+1                                                             LSQFU043          
      GOTO 271                                                          LSQFU044          
279   CONTINUE                                                          LSQFU045          
      IF(SUM) 330,330,410                                               LSQFU046          
330   WRITE(61,340) I                                                   LSQFU047          
340   FORMAT(5X,8HLSQFUN E,1H(,I3,8H) UNREAS,8HONABLY S,4HMALL)         LSQFU048          
      MAXFUN=0                                                          LSQFU049          
      J=1                                                               LSQFU050          
361   IF(J.GT.M) GOTO 369                                               LSQFU051          
      NN=NN+1                                                           LSQFU052          
      F(J)=W(NN)                                                        LSQFU053          
      J=J+1                                                             LSQFU054          
      GOTO 361                                                          LSQFU055          
369   CONTINUE                                                          LSQFU056          
      GOTO 1220                                                         LSQFU057          
410   SUM=1./SQRT(SUM)                                                  LSQFU058          
      J=K-N+I                                                           LSQFU059          
      W(J)=E(I)*SUM                                                     LSQFU060          
      J=1                                                               LSQFU061          
441   IF(J.GT.M) GOTO 449                                               LSQFU062          
      K=K+1                                                             LSQFU063          
      W(K)=F(J)*SUM                                                     LSQFU064          
      KK=NN+J                                                           LSQFU065          
      II=1                                                              LSQFU066          
481   IF(II.GT.I) GOTO 489                                              LSQFU067          
      KK=KK+MPLUSN                                                      LSQFU068          
      W(II)=W(II)+W(KK)*W(K)                                            LSQFU069          
      II=II+1                                                           LSQFU070          
      GOTO 481                                                          LSQFU071          
489   CONTINUE                                                          LSQFU072          
      J=J+1                                                             LSQFU073          
      GOTO 441                                                          LSQFU074          
449   CONTINUE                                                          LSQFU075          
      ILESS=I-1                                                         LSQFU076          
      IGAMAX=N+I-1                                                      LSQFU077          
      INCINV=N-ILESS                                                    LSQFU078          
      INCINP=INCINV+1                                                   LSQFU079          
      IF(ILESS) 580,580,600                                             LSQFU080          
580   W(KINV)=1.                                                        LSQFU081          
      GOTO 910                                                          LSQFU082          
600   B=1.                                                              LSQFU083          
      J=NPLUS                                                           LSQFU084          
611   IF(J.GT.IGAMAX) GOTO 619                                          LSQFU085          
      W(J)=0.                                                           LSQFU086          
      J=J+1                                                             LSQFU087          
      GOTO 611                                                          LSQFU088          
619   CONTINUE                                                          LSQFU089          
      KK=KINV                                                           LSQFU090          
      II=1                                                              LSQFU091          
651   IF(II.GT.ILESS) GOTO 659                                          LSQFU092          
      IIP=II+N                                                          LSQFU093          
      W(IIP)=W(IIP)+W(KK)*W(II)                                         LSQFU094          
      JL=II+1                                                           LSQFU095          
      IF(JL-ILESS) 700,700,760                                          LSQFU096          
700   JJ=JL                                                             LSQFU097          
701   IF(JJ.GT.ILESS) GOTO 709                                          LSQFU098          
      KK=KK+1                                                           LSQFU099          
      JJP=JJ+N                                                          LSQFU100          
      W(IIP)=W(IIP)+W(KK)*W(JJ)                                         LSQFU101          
      W(JJP)=W(JJP)+W(KK)*W(II)                                         LSQFU102          
      JJ=JJ+1                                                           LSQFU103          
      GOTO 701                                                          LSQFU104          
709   CONTINUE                                                          LSQFU105          
760   B=B-W(II)*W(IIP)                                                  LSQFU106          
      KK=KK+INCINP                                                      LSQFU107          
      II=II+1                                                           LSQFU108          
      GOTO 651                                                          LSQFU109          
659   CONTINUE                                                          LSQFU110          
      B=1./B                                                            LSQFU111          
      KK=KINV                                                           LSQFU112          
      II=NPLUS                                                          LSQFU113          
811   IF(II.GT.IGAMAX) GOTO 819                                         LSQFU114          
      BB=-B*W(II)                                                       LSQFU115          
      JJ=II                                                             LSQFU116          
831   IF(JJ.GT.IGAMAX) GOTO 839                                         LSQFU117          
      W(KK)=W(KK)-BB*W(JJ)                                              LSQFU118          
      KK=KK+1                                                           LSQFU119          
      JJ=JJ+1                                                           LSQFU120          
      GOTO 831                                                          LSQFU121          
839   CONTINUE                                                          LSQFU122          
      W(KK)=BB                                                          LSQFU123          
      KK=KK+INCINV                                                      LSQFU124          
      II=II+1                                                           LSQFU125          
      GOTO 811                                                          LSQFU126          
819   CONTINUE                                                          LSQFU127          
      W(KK)=B                                                           LSQFU128          
910   KQZ001=IINV                                                       LSQFU129          
      IF(KQZ001.LT.1) KQZ001=1                                          LSQFU130          
      IF(KQZ001.GT.2) KQZ001=2                                          LSQFU131          
      GOTO(1090,920),KQZ001                                             LSQFU132          
920   I=I+1                                                             LSQFU133          
      IF(I-N) 170,170,940                                               LSQFU134          
940   IINV=1                                                            LSQFU135          
      FF=0.                                                             LSQFU136          
      KL=NN                                                             LSQFU137          
      I=1                                                               LSQFU138          
971   IF(I.GT.M) GOTO 979                                               LSQFU139          
      KL=KL+1                                                           LSQFU140          
      F(I)=W(KL)                                                        LSQFU141          
      FF=FF+F(I)*F(I)                                                   LSQFU142          
      I=I+1                                                             LSQFU143          
      GOTO 971                                                          LSQFU144          
979   CONTINUE                                                          LSQFU145          
      ICONT=1                                                           LSQFU146          
      ISS=1                                                             LSQFU147          
      MC=N+1                                                            LSQFU148          
      IPP=IABS(IPRINT)*(IABS(IPRINT)-1)                                 LSQFU149          
      ITC=0                                                             LSQFU150          
      IPS=1                                                             LSQFU151          
      IPC=0                                                             LSQFU152          
1090  IPC=IPC-IABS(IPRINT)                                              LSQFU153          
      IF(IPC) 1110,1200,1200                                            LSQFU154          
1110  WRITE(61,1120) ITC,MC,FF                                          LSQFU155          
1120  FORMAT(//5X,8HITERATIO,1HN,I4,I9,8H CALLS O,7HF GIVEF,5X,2HF=,    LSQFU156          
     .E24.14)                                                           LSQFU157          
      WRITE(61,1140)(X(I),I=1,N)                                        LSQFU158          
1140  FORMAT(5X,8HVARIABLE,1HS,/(5E24.14))                              LSQFU159          
      IF(IPRINT) 1180,1160,1160                                         LSQFU160          
1160  WRITE(61,1170)(F(I),I=1,M)                                        LSQFU161          
1170  FORMAT(5X,8HFUNCTION,1HS,/(5E24.14))                              LSQFU162          
1180  IPC=IPP                                                           LSQFU163          
      KQZ001=IPS                                                        LSQFU164          
      IF(KQZ001.LT.1) KQZ001=1                                          LSQFU165          
      IF(KQZ001.GT.2) KQZ001=2                                          LSQFU166          
      GOTO(1200,1320),KQZ001                                            LSQFU167          
1200  KQZ001=ICONT                                                      LSQFU168          
      IF(KQZ001.LT.1) KQZ001=1                                          LSQFU169          
      IF(KQZ001.GT.2) KQZ001=2                                          LSQFU170          
      GOTO(1340,1210),KQZ001                                            LSQFU171          
1210  IF(CHANGE-1.) 1220,1220,1330                                      LSQFU172          
1220  IF(IPRINT) 1230,1320,1260                                         LSQFU173          
1230  WRITE(61,1240)                                                    LSQFU174          
1240  FORMAT(//5X,8HLSQFUN F,8HINAL VAL,8HUES OF V,8HARIABLES)          LSQFU175          
      GOTO 1280                                                         LSQFU176          
1260  WRITE(61,1270)                                                    LSQFU177          
1270  FORMAT(//5X,8HLSQFUN F,8HINAL VAL,8HUES OF F,8HUNCTIONS,          LSQFU178          
     .8H AND VAR,6HIABLES)                                              LSQFU179          
1280  IPS=2                                                             LSQFU180          
      IF(MAXFUN-1) 1110,1110,1300                                       LSQFU181          
1300  MAXFUN=MC                                                         LSQFU182          
      GOTO 1110                                                         LSQFU183          
1320  GOTO 3460                                                         LSQFU184          
1330  ICONT=1                                                           LSQFU185          
1340  ITC=ITC+1                                                         LSQFU186          
      K=N                                                               LSQFU187          
      KK=KST                                                            LSQFU188          
      I=1                                                               LSQFU189          
1371  IF(I.GT.N) GOTO 1379                                              LSQFU190          
      K=K+1                                                             LSQFU191          
      W(K)=0.                                                           LSQFU192          
      KK=KK+N                                                           LSQFU193          
      W(I)=0.                                                           LSQFU194          
      J=1                                                               LSQFU195          
1421  IF(J.GT.M) GOTO 1429                                              LSQFU196          
      KK=KK+1                                                           LSQFU197          
      W(I)=W(I)+W(KK)*F(J)                                              LSQFU198          
      J=J+1                                                             LSQFU199          
      GOTO 1421                                                         LSQFU200          
1429  CONTINUE                                                          LSQFU201          
      I=I+1                                                             LSQFU202          
      GOTO 1371                                                         LSQFU203          
1379  CONTINUE                                                          LSQFU204          
      DM=0.                                                             LSQFU205          
      K=KINV                                                            LSQFU206          
      II=1                                                              LSQFU207          
1491  IF(II.GT.N) GOTO 1499                                             LSQFU208          
      IIP=II+N                                                          LSQFU209          
      W(IIP)=W(IIP)+W(K)*W(II)                                          LSQFU210          
      JL=II+1                                                           LSQFU211          
      IF(JL-N) 1540,1540,1610                                           LSQFU212          
1540  JJ=JL                                                             LSQFU213          
1541  IF(JJ.GT.N) GOTO 1549                                             LSQFU214          
      JJP=JJ+N                                                          LSQFU215          
      K=K+1                                                             LSQFU216          
      W(IIP)=W(IIP)+W(K)*W(JJ)                                          LSQFU217          
      W(JJP)=W(JJP)+W(K)*W(II)                                          LSQFU218          
      JJ=JJ+1                                                           LSQFU219          
      GOTO 1541                                                         LSQFU220          
1549  CONTINUE                                                          LSQFU221          
      K=K+1                                                             LSQFU222          
1610  IF(DM-ABS(W(II)*W(IIP))) 1620,1640,1640                           LSQFU223          
1620  DM=ABS(W(II)*W(IIP))                                              LSQFU224          
      KL=II                                                             LSQFU225          
1640  CONTINUE                                                          LSQFU226          
      II=II+1                                                           LSQFU227          
      GOTO 1491                                                         LSQFU228          
1499  CONTINUE                                                          LSQFU229          
      II=N+MPLUSN*KL                                                    LSQFU230          
      CHANGE=0.                                                         LSQFU231          
      I=1                                                               LSQFU232          
1671  IF(I.GT.N) GOTO 1679                                              LSQFU233          
      JL=N+I                                                            LSQFU234          
      W(I)=0.                                                           LSQFU235          
      J=NPLUS                                                           LSQFU236          
1701  IF(J.GT.NN) GOTO 1709                                             LSQFU237          
      JL=JL+MPLUSN                                                      LSQFU238          
      W(I)=W(I)+W(J)*W(JL)                                              LSQFU239          
      J=J+1                                                             LSQFU240          
      GOTO 1701                                                         LSQFU241          
1709  CONTINUE                                                          LSQFU242          
      II=II+1                                                           LSQFU243          
      W(II)=W(JL)                                                       LSQFU244          
      W(JL)=X(I)                                                        LSQFU245          
      IF(ABS(E(I)*CHANGE)-ABS(W(I))) 1780,1780,1790                     LSQFU246          
1780  CHANGE=ABS(W(I)/E(I))                                             LSQFU247          
1790  CONTINUE                                                          LSQFU248          
      I=I+1                                                             LSQFU249          
      GOTO 1671                                                         LSQFU250          
1679  CONTINUE                                                          LSQFU251          
      I=1                                                               LSQFU252          
1801  IF(I.GT.M) GOTO 1809                                              LSQFU253          
      II=II+1                                                           LSQFU254          
      JL=JL+1                                                           LSQFU255          
      W(II)=W(JL)                                                       LSQFU256          
      W(JL)=F(I)                                                        LSQFU257          
      I=I+1                                                             LSQFU258          
      GOTO 1801                                                         LSQFU259          
1809  CONTINUE                                                          LSQFU260          
      FC=FF                                                             LSQFU261          
      ACC=0.1/CHANGE                                                    LSQFU262          
      IT=3                                                              LSQFU263          
      XC=0.                                                             LSQFU264          
      XL=0.                                                             LSQFU265          
      IS=3                                                              LSQFU266          
      XSTEP=-AMIN1(0.5,ESCALE/CHANGE)                                   LSQFU267          
      IF(CHANGE-1.) 1940,1940,1950                                      LSQFU268          
1940  ICONT=2                                                           LSQFU269          
1950  CONTINUE                                                          LSQFU270          
      IF(N-1) 1320,1970,2180                                            LSQFU271          
1970  MAX=MAXFUN-MC-1                                                   LSQFU272          
      XSTEP=0.5*ESCALE*E(1)                                             LSQFU273          
1990  CALL MINFUN(IT,X(1),FF,MAX,E(1),0.0,XSTEP)                        LSQFU274          
      KQZ001=IT                                                         LSQFU275          
      IF(KQZ001.LT.1) KQZ001=1                                          LSQFU276          
      IF(KQZ001.GT.4) KQZ001=4                                          LSQFU277          
      GOTO(2010,2130,2070,2110),KQZ001                                  LSQFU278          
2010  MC=MC+1                                                           LSQFU279          
      CALL GIVEF(X,F)                                                   LSQFU280          
      FF=0.                                                             LSQFU281          
      J=1                                                               LSQFU282          
2041  IF(J.GT.M) GOTO 2049                                              LSQFU283          
      FF=FF+F(J)*F(J)                                                   LSQFU284          
      J=J+1                                                             LSQFU285          
      GOTO 2041                                                         LSQFU286          
2049  CONTINUE                                                          LSQFU287          
      GOTO 1990                                                         LSQFU288          
2070  I=1                                                               LSQFU289          
      WRITE(61,340) I                                                   LSQFU290          
      MAXFUN=0                                                          LSQFU291          
      GOTO 2130                                                         LSQFU292          
2110  WRITE(61,2230) MAXFUN                                             LSQFU293          
      MAXFUN=1                                                          LSQFU294          
2130  CALL GIVEF(X,F)                                                   LSQFU295          
      MC=MC+1                                                           LSQFU296          
      CHANGE=0.                                                         LSQFU297          
      ICONT=2                                                           LSQFU298          
      GOTO 1090                                                         LSQFU299          
2180  CALL MINFUN(IT,XC,FC,6,ACC,0.1,XSTEP)                             LSQFU300          
      KQZ001=IT                                                         LSQFU301          
      IF(KQZ001.LT.1) KQZ001=1                                          LSQFU302          
      IF(KQZ001.GT.4) KQZ001=4                                          LSQFU303          
      GOTO(2200,2670,2670,2670),KQZ001                                  LSQFU304          
2200  MC=MC+1                                                           LSQFU305          
      IF(MC-MAXFUN) 2270,2270,2220                                      LSQFU306          
2220  WRITE(61,2230) MAXFUN                                             LSQFU307          
2230  FORMAT(5X,6HLSQFUN,I6,8H CALLS O,7HF GIVEF)                       LSQFU308          
      MAXFUN=1                                                          LSQFU309          
      ISS=2                                                             LSQFU310          
      GOTO 2670                                                         LSQFU311          
2270  XL=XC-XL                                                          LSQFU312          
      J=1                                                               LSQFU313          
2281  IF(J.GT.N) GOTO 2289                                              LSQFU314          
      X(J)=X(J)+XL*W(J)                                                 LSQFU315          
      J=J+1                                                             LSQFU316          
      GOTO 2281                                                         LSQFU317          
2289  CONTINUE                                                          LSQFU318          
      XL=XC                                                             LSQFU319          
      CALL GIVEF(X,F)                                                   LSQFU320          
      FC=0.                                                             LSQFU321          
      J=1                                                               LSQFU322          
2341  IF(J.GT.M) GOTO 2349                                              LSQFU323          
      FC=FC+F(J)*F(J)                                                   LSQFU324          
      J=J+1                                                             LSQFU325          
      GOTO 2341                                                         LSQFU326          
2349  CONTINUE                                                          LSQFU327          
      KQZ001=IS                                                         LSQFU328          
      IF(KQZ001.LT.1) KQZ001=1                                          LSQFU329          
      IF(KQZ001.GT.3) KQZ001=3                                          LSQFU330          
      GOTO(2480,2480,2380),KQZ001                                       LSQFU331          
2380  K=N                                                               LSQFU332          
      IF(FC-FF) 2400,1950,2440                                          LSQFU333          
2400  IS=2                                                              LSQFU334          
      FMIN=FC                                                           LSQFU335          
      FSEC=FF                                                           LSQFU336          
      GOTO 2580                                                         LSQFU337          
2440  IS=1                                                              LSQFU338          
      FMIN=FF                                                           LSQFU339          
      FSEC=FC                                                           LSQFU340          
      GOTO 2580                                                         LSQFU341          
2480  IF(FC-FSEC) 2490,1950,1950                                        LSQFU342          
2490  K=KSTORE                                                          LSQFU343          
      KQZ001=IS                                                         LSQFU344          
      IF(KQZ001.LT.1) KQZ001=1                                          LSQFU345          
      IF(KQZ001.GT.2) KQZ001=2                                          LSQFU346          
      GOTO(2510,2520),KQZ001                                            LSQFU347          
2510  K=N                                                               LSQFU348          
2520  IF(FC-FMIN) 2550,1950,2530                                        LSQFU349          
2530  FSEC=FC                                                           LSQFU350          
      GOTO 2580                                                         LSQFU351          
2550  IS=3-IS                                                           LSQFU352          
      FSEC=FMIN                                                         LSQFU353          
      FMIN=FC                                                           LSQFU354          
2580  J=1                                                               LSQFU355          
2581  IF(J.GT.N) GOTO 2589                                              LSQFU356          
      K=K+1                                                             LSQFU357          
      W(K)=X(J)                                                         LSQFU358          
      J=J+1                                                             LSQFU359          
      GOTO 2581                                                         LSQFU360          
2589  CONTINUE                                                          LSQFU361          
      J=1                                                               LSQFU362          
2621  IF(J.GT.M) GOTO 2629                                              LSQFU363          
      K=K+1                                                             LSQFU364          
      W(K)=F(J)                                                         LSQFU365          
      J=J+1                                                             LSQFU366          
      GOTO 2621                                                         LSQFU367          
2629  CONTINUE                                                          LSQFU368          
      GOTO 1950                                                         LSQFU369          
2670  K=KSTORE                                                          LSQFU370          
      KK=N                                                              LSQFU371          
      KQZ001=IS                                                         LSQFU372          
      IF(KQZ001.LT.1) KQZ001=1                                          LSQFU373          
      IF(KQZ001.GT.3) KQZ001=3                                          LSQFU374          
      GOTO(2720,2700,2720),KQZ001                                       LSQFU375          
2700  K=N                                                               LSQFU376          
      KK=KSTORE                                                         LSQFU377          
2720  SUM=0.                                                            LSQFU378          
      DM=0.                                                             LSQFU379          
      JJ=KSTORE                                                         LSQFU380          
      J=1                                                               LSQFU381          
2751  IF(J.GT.N) GOTO 2759                                              LSQFU382          
      K=K+1                                                             LSQFU383          
      KK=KK+1                                                           LSQFU384          
      JJ=JJ+1                                                           LSQFU385          
      X(J)=W(K)                                                         LSQFU386          
      W(JJ)=W(K)-W(KK)                                                  LSQFU387          
      J=J+1                                                             LSQFU388          
      GOTO 2751                                                         LSQFU389          
2759  CONTINUE                                                          LSQFU390          
      J=1                                                               LSQFU391          
2821  IF(J.GT.M) GOTO 2829                                              LSQFU392          
      K=K+1                                                             LSQFU393          
      KK=KK+1                                                           LSQFU394          
      JJ=JJ+1                                                           LSQFU395          
      F(J)=W(K)                                                         LSQFU396          
      W(JJ)=W(K)-W(KK)                                                  LSQFU397          
      SUM=SUM+W(JJ)*W(JJ)                                               LSQFU398          
      DM=DM+F(J)*W(JJ)                                                  LSQFU399          
      J=J+1                                                             LSQFU400          
      GOTO 2821                                                         LSQFU401          
2829  CONTINUE                                                          LSQFU402          
      KQZ001=ISS                                                        LSQFU403          
      IF(KQZ001.LT.1) KQZ001=1                                          LSQFU404          
      IF(KQZ001.GT.2) KQZ001=2                                          LSQFU405          
      GOTO(2920,1220),KQZ001                                            LSQFU406          
2920  J=KINV                                                            LSQFU407          
      KK=NPLUS-KL                                                       LSQFU408          
      I=1                                                               LSQFU409          
2941  IF(I.GT.KL) GOTO 2949                                             LSQFU410          
      K=J+KL-I                                                          LSQFU411          
      J=K+KK                                                            LSQFU412          
      W(I)=W(K)                                                         LSQFU413          
      W(K)=W(J-1)                                                       LSQFU414          
      I=I+1                                                             LSQFU415          
      GOTO 2941                                                         LSQFU416          
2949  CONTINUE                                                          LSQFU417          
      IF(KL-N) 3010,3130,3130                                           LSQFU418          
3010  KL=KL+1                                                           LSQFU419          
      JJ=K                                                              LSQFU420          
      I=KL                                                              LSQFU421          
3031  IF(I.GT.N) GOTO 3039                                              LSQFU422          
      K=K+1                                                             LSQFU423          
      J=J+NPLUS-I                                                       LSQFU424          
      W(I)=W(K)                                                         LSQFU425          
      W(K)=W(J-1)                                                       LSQFU426          
      I=I+1                                                             LSQFU427          
      GOTO 3031                                                         LSQFU428          
3039  CONTINUE                                                          LSQFU429          
      W(JJ)=W(K)                                                        LSQFU430          
      B=1./W(KL-1)                                                      LSQFU431          
      W(KL-1)=W(N)                                                      LSQFU432          
      GOTO 3140                                                         LSQFU433          
3130  B=1./W(N)                                                         LSQFU434          
3140  K=KINV                                                            LSQFU435          
      I=1                                                               LSQFU436          
3151  IF(I.GT.ILESS) GOTO 3159                                          LSQFU437          
      BB=B*W(I)                                                         LSQFU438          
      J=I                                                               LSQFU439          
3171  IF(J.GT.ILESS) GOTO 3179                                          LSQFU440          
      W(K)=W(K)-BB*W(J)                                                 LSQFU441          
      K=K+1                                                             LSQFU442          
      J=J+1                                                             LSQFU443          
      GOTO 3171                                                         LSQFU444          
3179  CONTINUE                                                          LSQFU445          
      K=K+1                                                             LSQFU446          
      I=I+1                                                             LSQFU447          
      GOTO 3151                                                         LSQFU448          
3159  CONTINUE                                                          LSQFU449          
      IF(FMIN-FF) 3260,3240,3240                                        LSQFU450          
3240  CHANGE=0.                                                         LSQFU451          
      GOTO 3280                                                         LSQFU452          
3260  FF=FMIN                                                           LSQFU453          
      CHANGE=ABS(XC)*CHANGE                                             LSQFU454          
3280  XL=-DM/FMIN                                                       LSQFU455          
      SUM=1./SQRT(SUM+DM*XL)                                            LSQFU456          
      K=KSTORE                                                          LSQFU457          
      I=1                                                               LSQFU458          
3311  IF(I.GT.N) GOTO 3319                                              LSQFU459          
      K=K+1                                                             LSQFU460          
      W(K)=SUM*W(K)                                                     LSQFU461          
      W(I)=0.                                                           LSQFU462          
      I=I+1                                                             LSQFU463          
      GOTO 3311                                                         LSQFU464          
3319  CONTINUE                                                          LSQFU465          
      I=1                                                               LSQFU466          
3361  IF(I.GT.M) GOTO 3369                                              LSQFU467          
      K=K+1                                                             LSQFU468          
      W(K)=SUM*(W(K)+XL*F(I))                                           LSQFU469          
      KK=NN+I                                                           LSQFU470          
      J=1                                                               LSQFU471          
3401  IF(J.GT.N) GOTO 3409                                              LSQFU472          
      KK=KK+MPLUSN                                                      LSQFU473          
      W(J)=W(J)+W(KK)*W(K)                                              LSQFU474          
      J=J+1                                                             LSQFU475          
      GOTO 3401                                                         LSQFU476          
3409  CONTINUE                                                          LSQFU477          
      I=I+1                                                             LSQFU478          
      GOTO 3361                                                         LSQFU479          
3369  CONTINUE                                                          LSQFU480          
      GOTO 600                                                          LSQFU481          
3460  RETURN                                                            LSQFU482          
      END                                                               LSQFU483          
