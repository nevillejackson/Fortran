      SUBROUTINE LSQPOLY(M,X,Y,F,DISCR,OP,N,H1,NLIM)                    LSQPO002          
      INTEGER H1,F5,F6,F7,F8,F9,F44,F48,TITLE,OP                        LSQPO003          
      DIMENSION C(20),D(200),TITLE(18),F48(18),F44(21),F5(7),F6(12),F7( LSQPO004          
     .44),F8(10),F9(7),LX(9),X(200),Y(200),W(200),F(200),P(200),Q(200), LSQPO005          
     .A(20),B(20),AAA(22),BBB(22),CCC(22)                               LSQPO006          
      COMMON/ /QQQ,CC,C,D,TITLE                                         LSQPO007          
      EQUIVALENCE(Z,BBB(1)),(BB,BBB(2)),(B(1),BBB(3)),(AA,AAA(2)),(A(1) LSQPO008          
     .,AAA(3)),(CC,CCC(2))                                              LSQPO009          
      J=1                                                               LSQPO010          
21    IF(J.GT.M) GOTO 29                                                LSQPO011          
      W(J)=1.0                                                          LSQPO012          
      J=J+1                                                             LSQPO013          
      GOTO 21                                                           LSQPO014          
29    CONTINUE                                                          LSQPO015          
      S14=M                                                             LSQPO016          
      GOTO 110                                                          LSQPO017          
      ENTRY WLSPOLY                                                     LSQPO018          
      S14=0.0                                                           LSQPO019          
      J=1                                                               LSQPO020          
81    IF(J.GT.M) GOTO 89                                                LSQPO021          
      W(J)=F(J)                                                         LSQPO022          
      S14=S14+W(J)                                                      LSQPO023          
      J=J+1                                                             LSQPO024          
      GOTO 81                                                           LSQPO025          
89    CONTINUE                                                          LSQPO026          
110   IF(.NOT.(M.GT.200)) GOTO 150                                      LSQPO027          
      WRITE(61,130)                                                     LSQPO028          
130   FORMAT(1H1,8HTOO MANY,8H DATA PO,4HINTS)                          LSQPO029          
      GOTO 1600                                                         LSQPO030          
150   IF(.NOT.(NLIM.GT.20)) GOTO 190                                    LSQPO031          
      NLIM=20                                                           LSQPO032          
      WRITE(61,180)                                                     LSQPO033          
180   FORMAT(1H1,8HMAX DEGR,8HEE EXCEE,5HDS 20/8H WILL PR,8HOCEED AS,   LSQPO034          
     .8H FAR AS ,2H20)                                                  LSQPO035          
190   IF(.NOT.(NLIM.GE.M)) GOTO 230                                     LSQPO036          
      NLIM=M-1                                                          LSQPO037          
      WRITE(61,220)                                                     LSQPO038          
220   FORMAT(1H1,8HMAX DEGR,8HEE EXCEE,8HDS NO OF,7H POINTS/8H WILL PR, LSQPO039          
     .8HOCEED AS,8H FAR AS ,8HPOSSIBLE)                                 LSQPO040          
230   NN=N+8                                                            LSQPO041          
      N1=NN/2+4                                                         LSQPO042          
      N2=NN-2                                                           LSQPO043          
      N3=NN-6                                                           LSQPO044          
      N4=NN-3                                                           LSQPO045          
      ENCODE(70,620,F48) NN,N,NN,N,NN,N                                 LSQPO046          
      ENCODE(84,1490,F44) N1,N2,N3,N4,N1                                LSQPO047          
      ENCODE(26,1320,F5) NN,N                                           LSQPO048          
      ENCODE(45,1380,F6) NN,N                                           LSQPO049          
      ENCODE(176,1400,F7)(NN,N,J=1,4)                                   LSQPO050          
      ENCODE(38,1440,F8) NN,N                                           LSQPO051          
      ENCODE(23,1550,F9) NN,N                                           LSQPO052          
      LX(7)=OP/10                                                       LSQPO053          
      LX(8)=OP-10*LX(7)                                                 LSQPO054          
      S3=0.                                                             LSQPO055          
      S2=S3                                                             LSQPO056          
      S5=S2                                                             LSQPO057          
      S13=S5                                                            LSQPO058          
      S12=S13                                                           LSQPO059          
      S10=M                                                             LSQPO060          
      K=0                                                               LSQPO061          
400   S5=0.                                                             LSQPO062          
      S3=S5                                                             LSQPO063          
      S2=S3                                                             LSQPO064          
      J=1                                                               LSQPO065          
411   IF(J.GT.M) GOTO 419                                               LSQPO066          
      W(J)=W(J)*S10/S14                                                 LSQPO067          
      X(J)=X(J)-S12                                                     LSQPO068          
      FF=X(J)*W(J)                                                      LSQPO069          
      S2=S2+FF                                                          LSQPO070          
      Y(J)=Y(J)-S13                                                     LSQPO071          
      G=Y(J)*W(J)                                                       LSQPO072          
      S3=S3+G                                                           LSQPO073          
      S5=S5+G*Y(J)                                                      LSQPO074          
      Q(J)=0.0                                                          LSQPO075          
      P(J)=1                                                            LSQPO076          
      J=J+1                                                             LSQPO077          
      GOTO 411                                                          LSQPO078          
419   CONTINUE                                                          LSQPO079          
      S14=S10                                                           LSQPO080          
      K=K+1                                                             LSQPO081          
      CC=S3/S10                                                         LSQPO082          
      S13=CC                                                            LSQPO083          
      S12=S2/S10                                                        LSQPO084          
      IF(H1.LT.0.AND.K.EQ.1) GOTO 400                                   LSQPO085          
      E=S5-(S3*S3)/S10                                                  LSQPO086          
      S=SQRT(E/(S10-1.0))                                               LSQPO087          
      IF(OP.EQ.33) GOTO 630                                             LSQPO088          
      WRITE(61,F48) TITLE,M,S12,S13                                     LSQPO089          
620   FORMAT(8H(1H1,18A,8H4/1H ,I4,8H,7H POIN,8HTS/8H ME,7HAN X=,E,I3,  LSQPO090          
     .1H.,I3,8H,5X,8H M,8HEAN Y=,E,I3,1H.,I3,1H))                       LSQPO091          
630   S1=S10                                                            LSQPO092          
      AA=0.0                                                            LSQPO093          
      Z=0.0                                                             LSQPO094          
      BB=1.0                                                            LSQPO095          
      SIG=S                                                             LSQPO096          
      I=0                                                               LSQPO097          
      GOTO 960                                                          LSQPO098          
700   S5=S2/S1                                                          LSQPO099          
      S4=S1                                                             LSQPO100          
      S1=0.                                                             LSQPO101          
      S2=S1                                                             LSQPO102          
      S3=S2                                                             LSQPO103          
      J=1                                                               LSQPO104          
731   IF(J.GT.M) GOTO 739                                               LSQPO105          
      S7=(X(J)-S5)*P(J)-S6*Q(J)                                         LSQPO106          
      Q(J)=P(J)                                                         LSQPO107          
      P(J)=S7                                                           LSQPO108          
      S3=S3+S7*Y(J)*W(J)                                                LSQPO109          
      S2=S2+S7*S7*X(J)*W(J)                                             LSQPO110          
      S1=S1+S7*S7*W(J)                                                  LSQPO111          
      J=J+1                                                             LSQPO112          
      GOTO 731                                                          LSQPO113          
739   CONTINUE                                                          LSQPO114          
      S7=S3/S1                                                          LSQPO115          
      E=E-S1*S7*S7                                                      LSQPO116          
      S=E/(S10-1.0)                                                     LSQPO117          
      IF(S.LT.0.) GOTO 860                                              LSQPO118          
      SIG=SQRT(S)                                                       LSQPO119          
      GOTO 870                                                          LSQPO120          
860   SIG=-1.0                                                          LSQPO121          
870   B(I)=1.0                                                          LSQPO122          
      A(I-1)=0.0                                                        LSQPO123          
      C(I)=S7                                                           LSQPO124          
      JJ=1                                                              LSQPO125          
901   IF(JJ.GT.I) GOTO 909                                              LSQPO126          
      J=I+2-JJ                                                          LSQPO127          
      S11=BBB(J-1)-S6*AAA(J)-S5*BBB(J)                                  LSQPO128          
      AAA(J)=BBB(J)                                                     LSQPO129          
      BBB(J)=S11                                                        LSQPO130          
      CCC(J)=CCC(J)+S11*S7                                              LSQPO131          
      JJ=JJ+1                                                           LSQPO132          
      GOTO 901                                                          LSQPO133          
909   CONTINUE                                                          LSQPO134          
960   S9=0.                                                             LSQPO135          
      S6=S9                                                             LSQPO136          
      S5=S6                                                             LSQPO137          
      S8=S5                                                             LSQPO138          
      S3=S8                                                             LSQPO139          
      J4=0                                                              LSQPO140          
      J3=J4                                                             LSQPO141          
      J=1                                                               LSQPO142          
981   IF(J.GT.M) GOTO 989                                               LSQPO143          
      S11=C(I)                                                          LSQPO144          
      KK=1                                                              LSQPO145          
1001  IF(KK.GT.I) GOTO 1009                                             LSQPO146          
      K=I+3-KK                                                          LSQPO147          
      S11=S11*X(J)+CCC(K-1)                                             LSQPO148          
      KK=KK+1                                                           LSQPO149          
      GOTO 1001                                                         LSQPO150          
1009  CONTINUE                                                          LSQPO151          
      GOTO 1040                                                         LSQPO152          
1040  D(J)=S11                                                          LSQPO153          
      S7=Y(J)-S11                                                       LSQPO154          
      IF(S7.LT.0.) GOTO 1130                                            LSQPO155          
      J3=J3+1                                                           LSQPO156          
      IF(S7.GT.S6) GOTO 1180                                            LSQPO157          
1090  S8=S8+S7*S7*W(J)                                                  LSQPO158          
      S3=S3+S7                                                          LSQPO159          
      S5=S5+ABS(S7)                                                     LSQPO160          
      J=J+1                                                             LSQPO161          
      GOTO 981                                                          LSQPO162          
989   CONTINUE                                                          LSQPO163          
      GOTO 1210                                                         LSQPO164          
1130  J4=J4+1                                                           LSQPO165          
      IF(.NOT.(S7.LE.S9)) GOTO 1090                                     LSQPO166          
      S9=S7                                                             LSQPO167          
      J2=J                                                              LSQPO168          
      GOTO 1090                                                         LSQPO169          
1180  S6=S7                                                             LSQPO170          
      J1=J                                                              LSQPO171          
      GOTO 1090                                                         LSQPO172          
1210  S3=S3/S10                                                         LSQPO173          
      S8=SQRT(S8/(S10-1.0))                                             LSQPO174          
      S5=S5/S10                                                         LSQPO175          
      KEY=LX(7)                                                         LSQPO176          
      IF(OP.EQ.33) GOTO 1560                                            LSQPO177          
      IF(.NOT.(I.EQ.NLIM)) GOTO 1280                                    LSQPO178          
      KEY=LX(8)                                                         LSQPO179          
1280  IF(KEY.GE.3) GOTO 1410                                            LSQPO180          
      WRITE(61,1300) I                                                  LSQPO181          
1300  FORMAT(/////1HM,20X,6HDEGREE,I4)                                  LSQPO182          
      WRITE(61,F5) S8                                                   LSQPO183          
1320  FORMAT(8H(12H+STA,8HND DEVN=,2H,E,I3,1H.,I3,1H))                  LSQPO184          
      IF(.NOT.(SIG.LE.0.)) GOTO 1370                                    LSQPO185          
      WRITE(61,1350)                                                    LSQPO186          
1350  FORMAT(1H ,35X,8HROUNDING,8H ERRORS ,8HACCUMULA,8HTING DAN,       LSQPO187          
     .8HGEROUSLY)                                                       LSQPO188          
      GOTO 1390                                                         LSQPO189          
1370  WRITE(61,F6) SIG                                                  LSQPO190          
1380  FORMAT(8H(1H ,35X,8H,23HCF. ,8HLESS ACC,8HURATE VA,5HLUE,E,I3,1H. LSQPO191          
     .,I3,1H))                                                          LSQPO192          
1390  WRITE(61,F7) S3,S5,J3,J1,S6,J4,J2,S9                              LSQPO193          
1400  FORMAT(8H(11H MEA,8HN DEVN=,,1HE,I3,1H.,I3,8H,/15H ME,8HAN MOD D, LSQPO194          
     .6HEVN=,E,I3,1H.,I3,8H,/17H NO,8H OF +VE ,8HDEVNS=,I,8H4,3X,7H ,   LSQPO195          
     .8HMAX AT,I,8H4,6HTH P,4HT=,E,I3,1H.,I3,8H,/17H NO,8H OF -VE ,     LSQPO196          
     .8HDEVNS=,I,8H4,3X,7H ,8HMAX AT,I,8H4,6HTH P,4HT=,E,I3,1H.,I3,2H/) LSQPO197          
     .)                                                                 LSQPO198          
1410  IF(KEY.GE.2) GOTO 1450                                            LSQPO199          
      J=0                                                               LSQPO200          
      WRITE(61,F8) J,CC,(J,C(J),J=1,I)                                  LSQPO201          
1440  FORMAT(8H(13H0COE,8HFFICIENT,8HS,/(I4,5,3HX,E,I3,1H.,I3,3H )))    LSQPO202          
1450  IF(KEY.GE.1) GOTO 1570                                            LSQPO203          
      WRITE(61,1470)                                                    LSQPO204          
1470  FORMAT(//4H FIT)                                                  LSQPO205          
      WRITE(61,F44)                                                     LSQPO206          
1490  FORMAT(7H(4X,1HJ,I3,8HX,4HX(J),1H,,I3,8HX,10HDAT,8HA  Y(J),,I3,   LSQPO207          
     .8HX,12HFIT,8HTED  Y(J,2H),,I3,7HX4HDEVN,I3,8HX8HREL.E,4HRR.))     LSQPO208          
      J=1                                                               LSQPO209          
1501  IF(J.GT.M) GOTO 1509                                              LSQPO210          
      S7=Y(J)-D(J)                                                      LSQPO211          
      S6=S7/ABS(Y(J))                                                   LSQPO212          
      IF(ABS(S6).LT.DISCR) GOTO 1560                                    LSQPO213          
      WRITE(61,F9) J,X(J),Y(J),D(J),S7,S6                               LSQPO214          
1550  FORMAT(8H(1H ,I4,,6H5(5X,E,I3,1H.,I3,2H)))                        LSQPO215          
1560  CONTINUE                                                          LSQPO216          
      J=J+1                                                             LSQPO217          
      GOTO 1501                                                         LSQPO218          
1509  CONTINUE                                                          LSQPO219          
1570  I=I+1                                                             LSQPO220          
      S6=S1/S4                                                          LSQPO221          
      IF(I.GT.NLIM) GOTO 1610                                           LSQPO222          
      GOTO 700                                                          LSQPO223          
1600  STOP                                                              LSQPO224          
1610  CONTINUE                                                          LSQPO225          
      RETURN                                                            LSQPO226          
      END                                                               LSQPO227          
