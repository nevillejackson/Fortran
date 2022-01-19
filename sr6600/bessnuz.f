      SUBROUTINE BESSNUZ(REZ,AIMZ,NU,BESJR,BESJI,BESYR,BESYI)           BESSN002          
      REAL ALOG,NU                                                      BESSN003          
      INTEGER EXFLTF                                                    BESSN004          
      DIMENSION BESJR(1),BESJI(1),BESYR(1),BESYI(1)                     BESSN005          
C   EVALUATION OF BESSEL FUNCTIONS FOR ANY REAL ORDER, COMPLEX ARGUMENT BESSN006          
C   WRITTEN BY G. KEADY, C.S.I.R.O., MELBOURNE, DECEMBER, 1967          BESSN007          
C   LAST REVISION OCTOBER 1968                                          BESSN008          
C   MUST BE USED IN CONJUNCTION WITH C3 CSIR GAMMA                      BESSN009          
C   NU IS (REAL) ORDER OF BESSEL FUNCTION                               BESSN010          
C   REZ IS THE REAL PART OF THE ARGUMENT Z                              BESSN011          
C   AIMZ IS THE IMAGINARY PART OF THE ARGUMENT Z                        BESSN012          
C   NU IS (REAL) ORDER OF BESSEL FUNCTION                               BESSN013          
C   BESJR (M+1) RETURNS THE REAL PART OF THE BESSEL FUNCTION J          BESSN014          
C   BESJI(M+1) RETURNS THE IMAG PART OF THE BESSEL FN J                 BESSN015          
C   BESKR(M+1) RETURNS THE REAL PART  OF THE BESSEL FN K                BESSN016          
C   BESKI (M+1) RETURNS THE IMAG PART OF THE BESSEL FN K                BESSN017          
CALCULATE BESSEL FUNCTIONS WITH COMPLEX ARGUMENTS. ENTER WITH REAL AND  BESSN018          
C/  IMAGY PARTS OF Z IN REZ AND AIMZ. RESULTS FOR J AND Y TO ORDER N AREBESSN019          
C/  IN BESJR,BESJI ETC.  TECHNIQUE FOLLOWS J.J.RUSSELL.                 BESSN020          
C                                                                       BESSN021          
C IN THE COMPLEX PLANE ERRORS INCREASE EXPONENTIALLY WITH ARGUMENT      BESSN022          
C  ASYMPTOTIC FORMULAE MUST BE USED                                     BESSN023          
C  SEE MICHELS    NASA TN D2141  GIVES UNTESTED ASYMPTOTIC              BESSN024          
C  RECURRENCE FORMULAE                                                  BESSN025          
C                                                                       BESSN026          
      IFLAG=0                                                           BESSN027          
      ZD=REZ*REZ+AIMZ*AIMZ                                              BESSN028          
      ZMOD=SQRT(ZD)                                                     BESSN029          
      INU=IFIX(NU)                                                      BESSN030          
      N=IABS(INU)                                                       BESSN031          
70    L=2*IFIX(6.5+ZMOD-8.E-4*ZD+8.E-10*ZD*ZD)                          BESSN032          
      IF(L-N-30) 90,100,100                                             BESSN033          
90    L=2*(15+N/2)                                                      BESSN034          
100   IF(.NOT.(N.GT.100.OR.ZMOD.GT.1000..OR.ZMOD.LT.1.E-6)) GOTO 140    BESSN035          
      WRITE(61,120) REZ,AIMZ,NU                                         BESSN036          
120   FORMAT(//,10X,8HBESCMPLX,8H REJECT.,8H Z OR NU,8H OFF SCA,3HLE.,  BESSN037          
     .3E15.5)                                                           BESSN038          
130   GOTO 2080                                                         BESSN039          
140   J=EXFLTF(J)                                                       BESSN040          
      LN=L                                                              BESSN041          
      IF(.NOT.(ABS(REZ).LT.1.E-20)) GOTO 190                            BESSN042          
      ARGZ=SIGN(1.570796327,AIMZ)                                       BESSN043          
      GOTO 210                                                          BESSN044          
190   ARGZ=ATAN(AIMZ/REZ)                                               BESSN045          
      ARGZ=ARGZ+SIGN(3.141592654,ARGZ)*(SIGN(0.5,REZ)-0.5)              BESSN046          
210   ZRR=REZ/ZD                                                        BESSN047          
      ZRI=-AIMZ/ZD                                                      BESSN048          
      ZSQRR=(ZRR*ZRR-ZRI*ZRI)*4E0                                       BESSN049          
      ZSQRI=ZRR*ZRI*8E0                                                 BESSN050          
      IF(IFLAG-1) 310,260,280                                           BESSN051          
260   ABC=1.E-300                                                       BESSN052          
      AD=ABC                                                            BESSN053          
      AC=AD                                                             BESSN054          
      GOTO 320                                                          BESSN055          
280   WRITE(61,300) $ GOTO 2080                                         BESSN056          
300   FORMAT(8H OVERFLO,8HW HAS OC,8HCURRED A,8HFTER TWO,8H STARTIN,    BESSN057          
     .8HG ATTEMP,8HTS, CALL,8H REJECTE,8HD, RESUL,8HTS UNDEF,6HINED  )  BESSN058          
310   ABC=1.E-25                                                        BESSN059          
      AD=ABC                                                            BESSN060          
      AC=AD                                                             BESSN061          
320   BD=0                                                              BESSN062          
      BC=BD                                                             BESSN063          
      BB=BC                                                             BESSN064          
      BA=BB                                                             BESSN065          
      AB=BA                                                             BESSN066          
      AA=AB                                                             BESSN067          
      IF(ABS(NU-FLOAT(INU))-1.E-10) 340,340,1020                        BESSN068          
340   J=L-L/4*4-1                                                       BESSN069          
      GOTO 450                                                          BESSN070          
360   BA=BA+2E0*AE                                                      BESSN071          
      BB=BB+2E0*AF                                                      BESSN072          
      A=4.*FLOAT(J)/FLOAT(L)                                            BESSN073          
      BC=BC+A*AE                                                        BESSN074          
      BD=BD+A*AF                                                        BESSN075          
      AA=AC                                                             BESSN076          
      AB=AD                                                             BESSN077          
      AC=AE                                                             BESSN078          
      AD=AF                                                             BESSN079          
450   A=FLOAT(L)*FLOAT(L-1)*ZSQRR-2.*FLOAT(L)/FLOAT(L+1)                BESSN080          
      B=FLOAT(L)*FLOAT(L-1)*ZSQRI                                       BESSN081          
      C=(1.-FLOAT(L))/FLOAT(1+L)                                        BESSN082          
      AE=A*AC-B*AD+C*AA                                                 BESSN083          
      AF=A*AD+B*AC+C*AB                                                 BESSN084          
      IF(L-3-N) 510,510,530                                             BESSN085          
510   BESJR(L-1)=AE                                                     BESSN086          
      BESJI(L-1)=AF                                                     BESSN087          
530   J=-J                                                              BESSN088          
      L=L-2                                                             BESSN089          
      IF(L.NE.0) GOTO 360                                               BESSN090          
      BA=BA+AE                                                          BESSN091          
      BB=BB+AF                                                          BESSN092          
      A=1E0/(BA*BA+BB*BB)                                               BESSN093          
      AA=A*BA                                                           BESSN094          
      AB=-A*BB                                                          BESSN095          
      BESJR(1)=AA*AE-AB*AF                                              BESSN096          
      BESJI(1)=AA*AF+AB*AE                                              BESSN097          
      A=ALOG(ZMOD)-0.115931516                                          BESSN098          
      BESYR(1)=(A*BESJR(1)-ARGZ*BESJI(1)+AA*BC-AB*BD)*0.6366197724      BESSN099          
      BESYI(1)=(A*BESJI(1)+ARGZ*BESJR(1)+AA*BD+AB*BC)*0.6366197724      BESSN100          
      L=1                                                               BESSN101          
      PIZR=-ZRR*0.6366197724                                            BESSN102          
      PIZI=-ZRI*0.6366197724                                            BESSN103          
      IF(EXFLTF(J).EQ.2) GOTO 740                                       BESSN104          
700   WRITE(61,710) REZ,AIMZ,NU                                         BESSN105          
710   FORMAT(/,10X,8HEXFLT ER,8HROR IN N,8HORMALISI,8HNG SUM B,6HESSEL  BESSN106          
     .,3E15.5/10X,8HSECOND A,8HTTEMPT W,8HITH NEW ,8HSTARTING,6H POINT) BESSN107          
      IFLAG=IFLAG+1 $ GOTO 70                                           BESSN108          
740   C=BESJR(L+2)                                                      BESSN109          
      BESJR(L+2)=AA*C-AB*BESJI(L+2)                                     BESSN110          
      BESJI(L+2)=AA*BESJI(L+2)+AB*C                                     BESSN111          
      A=(BESJR(L)+BESJR(L+2))/FLOAT(2*L)                                BESSN112          
      B=(BESJI(L)+BESJI(L+2))/FLOAT(2*L)                                BESSN113          
      BESJR(L+1)=REZ*A-AIMZ*B                                           BESSN114          
      BESJI(L+1)=REZ*B+AIMZ*A                                           BESSN115          
810   C=1E0/(BESJR(L)*BESJR(L)+BESJI(L)*BESJI(L))                       BESSN116          
      A=(BESJR(L+1)*BESYR(L)-BESJI(L+1)*BESYI(L)+PIZR)*C                BESSN117          
      B=(BESJR(L+1)*BESYI(L)+BESJI(L+1)*BESYR(L)+PIZI)*C                BESSN118          
      BESYR(L+1)=(BESJR(L)*A+BESJI(L)*B)                                BESSN119          
      BESYI(L+1)=(BESJR(L)*B-BESJI(L)*A)                                BESSN120          
      L=L+1                                                             BESSN121          
      IF(L.EQ.L/2*2) GOTO 810                                           BESSN122          
      IF(EXFLTF(J).EQ.2) GOTO 920                                       BESSN123          
890   WRITE(61,910) L $ GOTO 2080                                       BESSN124          
910   FORMAT(/,10X,8HERROR EX,8HFLT IN B,8HESSEL RE,8HCURRENCE,5H. L= , BESSN125          
     .I6/10X,8HHIGHER O,8HRDERS GI,8HVE UNDEF,8HINED VAL,8HUES     )    BESSN126          
920   IF(.NOT.(L.GE.N+1)) GOTO 740                                      BESSN127          
      IF(NU.GE.0.) GOTO 130                                             BESSN128          
      M=(N+1)/2*2                                                       BESSN129          
      L=2                                                               BESSN130          
951   IF(L.GT.M) GOTO 959                                               BESSN131          
      BESJR(L)=-BESJR(L) $ BESJI(L)=-BESJI(L)                           BESSN132          
      BESYR(L)=-BESYR(L) $ BESYI(L)=-BESYI(L)                           BESSN133          
      L=L+2                                                             BESSN134          
      GOTO 951                                                          BESSN135          
959   CONTINUE                                                          BESSN136          
      GOTO 2080                                                         BESSN137          
C     FRACTIONAL ORDER                                                  BESSN138          
1020  AAN=ABS(NU-FLOAT(INU))                                            BESSN139          
      IF(.NOT.(NU.GE.0.)) GOTO 1060                                     BESSN140          
      J=1 $ GOTO 1070                                                   BESSN141          
1060  J=2                                                               BESSN142          
1070  KQZ001=J                                                          BESSN143          
      IF(KQZ001.LT.1) KQZ001=1                                          BESSN144          
      IF(KQZ001.GT.4) KQZ001=4                                          BESSN145          
      GOTO(1080,1110,1110,1080),KQZ001                                  BESSN146          
1080  BN=1.-AAN $ DD=AAN $ GOTO 1140                                    BESSN147          
1110  BN=AAN $ DD=2.-AAN $ L=L+1                                        BESSN148          
1140  II=L/2                                                            BESSN149          
      YMOD=(2./ZMOD)**DD*(DD+2E0*FLOAT(II))*GAMMA(DD+FLOAT(II))/GAMMA(  BESSN150          
     .FLOAT(II)+1.)                                                     BESSN151          
      II=II+1                                                           BESSN152          
      GOTO 1260                                                         BESSN153          
1180  X=(DD+2E0*FLOAT(II))*(DD+FLOAT(II)-1.)/(FLOAT(II)*(DD+2E0*FLOAT(  BESSN154          
     .II)-2.))                                                          BESSN155          
      YMOD=YMOD/X                                                       BESSN156          
      BA=BA+YMOD*AE                                                     BESSN157          
      BB=BB+YMOD*AF                                                     BESSN158          
1220  AA=AC                                                             BESSN159          
      AB=AD                                                             BESSN160          
      AC=AE                                                             BESSN161          
      AD=AF                                                             BESSN162          
C     RECURRENCE                                                        BESSN163          
1260  FLN=FLOAT(L)-BN+1.                                                BESSN164          
      A=FLN*(FLN-1.)*ZSQRR-2.*FLN/(FLN+1.)                              BESSN165          
      B=FLN*(FLN-1.)*ZSQRI                                              BESSN166          
      C=(1.-FLN)/(1.+FLN)                                               BESSN167          
      AE=A*AC-B*AD+C*AA                                                 BESSN168          
      AF=A*AD+B*AC+C*AB                                                 BESSN169          
      L=L-2                                                             BESSN170          
      II=II-1                                                           BESSN171          
      KQZ001=J                                                          BESSN172          
      IF(KQZ001.LT.1) KQZ001=1                                          BESSN173          
      IF(KQZ001.GT.4) KQZ001=4                                          BESSN174          
      GOTO(1350,1390,1480,1440),KQZ001                                  BESSN175          
1350  IF(L-1-INU) 1360,1360,1180                                        BESSN176          
1360  BESJR(L+1)=AE                                                     BESSN177          
      BESJI(L+1)=AF                                                     BESSN178          
      IF(L) 1530,1530,1180                                              BESSN179          
1390  IF(L+1) 1400,1400,1180                                            BESSN180          
1400  M=-L                                                              BESSN181          
      BESJR(M)=AE                                                       BESSN182          
      BESJI(M)=AF                                                       BESSN183          
      IF(L+1-INU) 1570,1220,1220                                        BESSN184          
1440  IF(L-1-INU) 1450,1450,1180                                        BESSN185          
1450  BESYR(L+1)=AE                                                     BESSN186          
      BESYI(L+1)=AF                                                     BESSN187          
      IF(L) 1530,1530,1180                                              BESSN188          
1480  IF(L+1) 1490,1490,1180                                            BESSN189          
1490  M=-L                                                              BESSN190          
      BESYR(M)=AE                                                       BESSN191          
      BESYI(M)=AF                                                       BESSN192          
      IF(L+1-INU) 1570,1220,1220                                        BESSN193          
1530  X=(DD+2E0*FLOAT(II))*(DD+FLOAT(II)-1.)/(FLOAT(II)*(DD+2E0*FLOAT(  BESSN194          
     .II)-2.))                                                          BESSN195          
      YMOD=YMOD/X                                                       BESSN196          
      BA=BA+YMOD*AE                                                     BESSN197          
      BB=BB+YMOD*AF                                                     BESSN198          
1570  ZDD=DD*ARGZ $ A=COS(ZDD) $ B=SIN(ZDD)                             BESSN199          
      C=BA                                                              BESSN200          
      BA=A*C+B*BB                                                       BESSN201          
      BB=A*BB-B*C                                                       BESSN202          
      KQZ001=J                                                          BESSN203          
      IF(KQZ001.LT.1) KQZ001=1                                          BESSN204          
      IF(KQZ001.GT.4) KQZ001=4                                          BESSN205          
      GOTO(1640,1640,1740,1740),KQZ001                                  BESSN206          
1640  INU=-INU $ L=LN $ J=J+2                                           BESSN207          
      A=1./(BA*BA+BB*BB)                                                BESSN208          
      AAJ=A*BA                                                          BESSN209          
      ABJ=-A*BB                                                         BESSN210          
      BB=0.                                                             BESSN211          
      BA=BB                                                             BESSN212          
      AB=BA                                                             BESSN213          
      AA=AB                                                             BESSN214          
      AC=ABC $ AD=ABC                                                   BESSN215          
      GOTO 1070                                                         BESSN216          
1740  A=1./(BA*BA+BB*BB)                                                BESSN217          
      AAY=A*BA                                                          BESSN218          
      ABY=-A*BB                                                         BESSN219          
      INU=-INU                                                          BESSN220          
C                                                                       BESSN221          
      IF(.NOT.(EXFLTF(J).EQ.2)) GOTO 700                                BESSN222          
      FLN=SIGN(AAN,NU)                                                  BESSN223          
      BA=COS(3.141592654*FLN)                                           BESSN224          
      BB=SIN(3.141592654*FLN)                                           BESSN225          
      C=BESJR(1)                                                        BESSN226          
      BESJR(1)=AAJ*C-ABJ*BESJI(1)                                       BESSN227          
      BESJI(1)=AAJ*BESJI(1)+ABJ*C                                       BESSN228          
      C=BESYR(1)                                                        BESSN229          
      BESYR(1)=(BESJR(1)*BA-AAY*C+ABY*BESYI(1))/BB                      BESSN230          
      BESYI(1)=(BESJI(1)*BA-AAY*BESYI(1)-ABY*C)/BB                      BESSN231          
      FLN=FLN-SIGN(1.,NU)                                               BESSN232          
      L=1                                                               BESSN233          
1900  C=BESJR(L+2)                                                      BESSN234          
      BESJR(L+2)=AAJ*C-ABJ*BESJI(L+2)                                   BESSN235          
      BESJI(L+2)=AAJ*BESJI(L+2)+ABJ*C                                   BESSN236          
      C=BESYR(L+2)                                                      BESSN237          
      BESYR(L+2)=(BESJR(L+2)*BA-AAY*C+ABY*BESYI(L+2))/BB                BESSN238          
      BESYI(L+2)=(BESJI(L+2)*BA-AAY*BESYI(L+2)-ABY*C)/BB                BESSN239          
      FLN=FLN+SIGN(2.,NU)                                               BESSN240          
      A=(BESJR(L)+BESJR(L+2))/(2.*FLN)                                  BESSN241          
      B=(BESJI(L)+BESJI(L+2))/(2.*FLN)                                  BESSN242          
      BESJR(L+1)=REZ*A-AIMZ*B                                           BESSN243          
      BESJI(L+1)=REZ*B+AIMZ*A                                           BESSN244          
      A=(BESYR(L)+BESYR(L+2))/(2.*FLN)                                  BESSN245          
      B=(BESYI(L)+BESYI(L+2))/(2.*FLN)                                  BESSN246          
      BESYR(L+1)=REZ*A-AIMZ*B                                           BESSN247          
      BESYI(L+1)=REZ*B+AIMZ*A                                           BESSN248          
      IF(.NOT.(EXFLTF(J).EQ.2)) GOTO 890                                BESSN249          
      L=L+2                                                             BESSN250          
      IF(L.GE.N+1) GOTO 130                                             BESSN251          
      GOTO 1900                                                         BESSN252          
2080  RETURN                                                            BESSN253          
      END                                                               BESSN254          
