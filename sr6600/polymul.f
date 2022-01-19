      SUBROUTINE POLYMUL(N,KER,CRR,ROOTR)                               POLYM002          
      COMPLEX CRR,ROOTR,XR,FXR,SR,AR,BR,CR,ZR,RR,DBARR,ABARR,BBARR,     POLYM003          
     .AMIBR,DENR,DR,TAR,TCR,T1R,T3R,CSQRT                               POLYM004          
      DIMENSION CRR(21),ROOTR(20),XR(4),FXR(4),SR(3),RXR(8)             POLYM005          
      EQUIVALENCE(XR,RXR)                                               POLYM006          
      IMAX=25                                                           POLYM007          
      NUM=3                                                             POLYM008          
      DEL=.2                                                            POLYM009          
      RATIO=5.                                                          POLYM010          
      ALTER=1.000001                                                    POLYM011          
      EP1=1.E-21                                                        POLYM012          
      EP2=1.E-8                                                         POLYM013          
      EP3=1.E-7                                                         POLYM014          
      EP4=1.E-8                                                         POLYM015          
      SR(1)=-1.                                                         POLYM016          
      SR(2)=1.                                                          POLYM017          
      SR(3)=0.                                                          POLYM018          
      C1=CABS(CRR(1))                                                   POLYM019          
      K=1                                                               POLYM020          
151   IF(K.GT.N) GOTO 159                                               POLYM021          
      I=1                                                               POLYM022          
      NP1=N+2-K                                                         POLYM023          
      NN=NP1-1                                                          POLYM024          
      IF(K+1-N) 310,240,200                                             POLYM025          
200   XR(1)=-CRR(2)/CRR(1)                                              POLYM026          
      K1=1                                                              POLYM027          
      K2=1                                                              POLYM028          
      GOTO 350                                                          POLYM029          
240   AR=CRR(1)                                                         POLYM030          
      BR=CRR(2)                                                         POLYM031          
      CR=CRR(3)                                                         POLYM032          
      K1=1                                                              POLYM033          
      K2=1                                                              POLYM034          
      M4=2                                                              POLYM035          
      GOTO 800                                                          POLYM036          
310   J=1                                                               POLYM037          
311   IF(J.GT.3) GOTO 319                                               POLYM038          
      XR(J)=SR(J)                                                       POLYM039          
      J=J+1                                                             POLYM040          
      GOTO 311                                                          POLYM041          
319   CONTINUE                                                          POLYM042          
      K1=1                                                              POLYM043          
      K2=3                                                              POLYM044          
350   M1=1                                                              POLYM045          
      M2=1                                                              POLYM046          
      M3=1                                                              POLYM047          
      M4=1                                                              POLYM048          
390   L=K1                                                              POLYM049          
391   IF(L.GT.K2) GOTO 399                                              POLYM050          
      ZR=XR(L)                                                          POLYM051          
      RR=0.0                                                            POLYM052          
      J=1                                                               POLYM053          
421   IF(J.GT.NP1) GOTO 429                                             POLYM054          
      RR=ZR*RR+CRR(J)                                                   POLYM055          
      J=J+1                                                             POLYM056          
      GOTO 421                                                          POLYM057          
429   CONTINUE                                                          POLYM058          
      FXR(L)=RR                                                         POLYM059          
      PMAG=CABS(RR)                                                     POLYM060          
      RAD=ALTER*(PMAG/C1)**(1./FLOAT(NN))                               POLYM061          
      KQZ001=M3                                                         POLYM062          
      IF(KQZ001.LT.1) KQZ001=1                                          POLYM063          
      IF(KQZ001.GT.3) KQZ001=3                                          POLYM064          
      GOTO(480,560,1270),KQZ001                                         POLYM065          
480   POLYO=POLYN                                                       POLYM066          
      POLYN=PMAG                                                        POLYM067          
      IF(PMAG) 510,1550,510                                             POLYM068          
510   I=I+1                                                             POLYM069          
      L=L+1                                                             POLYM070          
      GOTO 391                                                          POLYM071          
399   CONTINUE                                                          POLYM072          
      IF(K+1-N) 530,1550,1550                                           POLYM073          
530   KQZ001=M1                                                         POLYM074          
      IF(KQZ001.LT.1) KQZ001=1                                          POLYM075          
      IF(KQZ001.GT.2) KQZ001=2                                          POLYM076          
      GOTO(540,1290),KQZ001                                             POLYM077          
540   VAL=DEL*POLYN                                                     POLYM078          
      DBARR=RAD                                                         POLYM079          
560   K1=4                                                              POLYM080          
      K2=4                                                              POLYM081          
      M1=2                                                              POLYM082          
      M3=1                                                              POLYM083          
600   ABARR=XR(1)-XR(3)                                                 POLYM084          
      BBARR=XR(2)-XR(3)                                                 POLYM085          
      AMIBR=XR(1)-XR(2)                                                 POLYM086          
      DENR=AMIBR*BBARR*ABARR                                            POLYM087          
      TA=CABS(DENR)                                                     POLYM088          
      T4=CABS(XR(3))                                                    POLYM089          
      IF(TA-EP1*T4) 670,670,750                                         POLYM090          
670   DR=0.                                                             POLYM091          
      RR=0.                                                             POLYM092          
      JCSQ=1                                                            POLYM093          
691   IF(JCSQ.GT.NP1) GOTO 699                                          POLYM094          
      DR=XR(3)*DR+RR                                                    POLYM095          
      RR=XR(3)*RR+CRR(JCSQ)                                             POLYM096          
      JCSQ=JCSQ+1                                                       POLYM097          
      GOTO 691                                                          POLYM098          
699   CONTINUE                                                          POLYM099          
      TR=CABS(RR)                                                       POLYM100          
      TD=CABS(DR)                                                       POLYM101          
      IF(TR-EP4*TD) 1550,1120,1120                                      POLYM102          
750   TAR=BBARR*(FXR(1)-FXR(3))                                         POLYM103          
      TCR=ABARR*(FXR(2)-FXR(3))                                         POLYM104          
      AR=(TAR-TCR)/DENR                                                 POLYM105          
      BR=(ABARR*TCR-BBARR*TAR)/DENR                                     POLYM106          
      CR=FXR(3)                                                         POLYM107          
800   TAR=BR*BR-4.0*AR*CR                                               POLYM108          
      TCR=CSQRT(TAR)                                                    POLYM109          
      T1R=-BR+TCR                                                       POLYM110          
      T3R=-BR-TCR                                                       POLYM111          
      TA=CABS(T1R)                                                      POLYM112          
      TB=CABS(T3R)                                                      POLYM113          
      IF(TA-TB) 870,890,890                                             POLYM114          
870   TA=TB                                                             POLYM115          
      T1R=T3R                                                           POLYM116          
890   KQZ001=M4                                                         POLYM117          
      IF(KQZ001.LT.1) KQZ001=1                                          POLYM118          
      IF(KQZ001.GT.2) KQZ001=2                                          POLYM119          
      GOTO(900,930),KQZ001                                              POLYM120          
900   IF(TA) 970,970,910                                                POLYM121          
910   TB=2.0*CABS(CR)                                                   POLYM122          
      IF(TB-RAD*TA) 930,930,1090                                        POLYM123          
930   DBARR=2.*CR/T1R                                                   POLYM124          
      KQZ001=M4                                                         POLYM125          
      IF(KQZ001.LT.1) KQZ001=1                                          POLYM126          
      IF(KQZ001.GT.2) KQZ001=2                                          POLYM127          
      GOTO(970,950),KQZ001                                              POLYM128          
950   XR(1)=DBARR                                                       POLYM129          
      GOTO 350                                                          POLYM130          
970   XR(4)=XR(3)+DBARR                                                 POLYM131          
      TR=ABS(RXR(7))                                                    POLYM132          
      TI=ABS(RXR(8))                                                    POLYM133          
      IF(TR) 1080,1080,1010                                             POLYM134          
1010  IF(TI) 1080,1080,1020                                             POLYM135          
1020  IF(TR-TI) 1060,1080,1030                                          POLYM136          
1030  IF(TI-EP2*TR) 1040,1080,1080                                      POLYM137          
1040  RXR(8)=0.0                                                        POLYM138          
      GOTO 390                                                          POLYM139          
1060  IF(TR-EP2*TI) 1070,1080,1080                                      POLYM140          
1070  RXR(7)=0.0                                                        POLYM141          
1080  GOTO 390                                                          POLYM142          
1090  T1R=T1R/TA                                                        POLYM143          
      CR=CR*(RAD/TB)                                                    POLYM144          
      GOTO 930                                                          POLYM145          
1120  T1=CABS(ABARR)                                                    POLYM146          
      IF(T1-EP3*T4) 1180,1180,1140                                      POLYM147          
1140  T2=CABS(BBARR)                                                    POLYM148          
      IF(T2-EP3*T4) 1220,1220,1160                                      POLYM149          
1160  T3=CABS(AMIBR)                                                    POLYM150          
      IF(T3-EP3*T4) 1180,1180,750                                       POLYM151          
1180  K1=1                                                              POLYM152          
      K2=1                                                              POLYM153          
      M3=2                                                              POLYM154          
      GOTO 1250                                                         POLYM155          
1220  K1=2                                                              POLYM156          
      K2=2                                                              POLYM157          
      M3=3                                                              POLYM158          
1250  XR(K1)=XR(K1)*(1.0+2.0*EP3)                                       POLYM159          
      GOTO 390                                                          POLYM160          
1270  POLYO=PMAG                                                        POLYM161          
      GOTO 560                                                          POLYM162          
1290  IF(POLYN-VAL) 1300,1300,1410                                      POLYM163          
1300  VAL=DEL*POLYN                                                     POLYM164          
      LIM=I+NUM                                                         POLYM165          
      M2=2                                                              POLYM166          
      DR=0.0                                                            POLYM167          
      RR=0.0                                                            POLYM168          
      JCSQ=1                                                            POLYM169          
1351  IF(JCSQ.GT.NP1) GOTO 1359                                         POLYM170          
      DR=XR(4)*DR+RR                                                    POLYM171          
      RR=XR(4)*RR+CRR(JCSQ)                                             POLYM172          
      JCSQ=JCSQ+1                                                       POLYM173          
      GOTO 1351                                                         POLYM174          
1359  CONTINUE                                                          POLYM175          
      TR=CABS(RR)                                                       POLYM176          
      TD=CABS(DR)                                                       POLYM177          
      IF(TR-EP4*TD) 1550,1410,1410                                      POLYM178          
1410  DLT=RATIO*POLYO/POLYN                                             POLYM179          
      IF(1.0-DLT) 1460,1460,1430                                        POLYM180          
1430  DBARR=DLT*DBARR                                                   POLYM181          
      LIM=LIM+1                                                         POLYM182          
      GOTO 970                                                          POLYM183          
1460  KQZ001=M2                                                         POLYM184          
      IF(KQZ001.LT.1) KQZ001=1                                          POLYM185          
      IF(KQZ001.GT.2) KQZ001=2                                          POLYM186          
      GOTO(1470,1500),KQZ001                                            POLYM187          
1470  IF(I-IMAX) 1510,1510,1480                                         POLYM188          
1480  KER=1                                                             POLYM189          
      GOTO 1610                                                         POLYM190          
1500  IF(I-LIM) 1510,1510,1550                                          POLYM191          
1510  L=1                                                               POLYM192          
1511  IF(L.GT.3) GOTO 1519                                              POLYM193          
      XR(L)=XR(L+1)                                                     POLYM194          
      FXR(L)=FXR(L+1)                                                   POLYM195          
      L=L+1                                                             POLYM196          
      GOTO 1511                                                         POLYM197          
1519  CONTINUE                                                          POLYM198          
      GOTO 600                                                          POLYM199          
1550  J=2                                                               POLYM200          
1551  IF(J.GT.NN) GOTO 1559                                             POLYM201          
      CRR(J)=ZR*CRR(J-1)+CRR(J)                                         POLYM202          
      J=J+1                                                             POLYM203          
      GOTO 1551                                                         POLYM204          
1559  CONTINUE                                                          POLYM205          
      ROOTR(K)=ZR                                                       POLYM206          
      K=K+1                                                             POLYM207          
      GOTO 151                                                          POLYM208          
159   CONTINUE                                                          POLYM209          
      KER=0                                                             POLYM210          
1610  RETURN                                                            POLYM211          
      END                                                               POLYM212          
