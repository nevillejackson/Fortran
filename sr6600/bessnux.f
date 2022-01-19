      SUBROUTINE BESSNUX(REZ,NU,BESJR,BESYR)                            BESSN002          
      REAL ALOG,NU                                                      BESSN003          
      INTEGER EXFLTF                                                    BESSN004          
      DIMENSION BESJR(1),BESYR(1)                                       BESSN005          
C   WRITTEN BY G. KEADY, C.S.I.R.O., MELBOURNE, DECEMBER, 1967          BESSN006          
C   EVALUATION OF BESSEL FUNCTIONS, REAL POSITIVE ARGUMENTS,REAL ORDER  BESSN007          
C   MUST BE USED IN CONJUNCTION WITH C3 CSIR GAMMA                      BESSN008          
C   NU IS (REAL) ORDER OF BESSEL FUNCTION                               BESSN009          
C   REZ IS THE (REAL) ARGUMENT X                                        BESSN010          
C   BESJR(M+1) RETURNS THE BESSEL FUNCTION J                            BESSN011          
C   BESYR(M+1) RETURNS THE BESSEL FUNCTION Y                            BESSN012          
      IFLAG=0                                                           BESSN013          
      ZMOD=ABS(REZ)                                                     BESSN014          
      INU=IFIX(NU)                                                      BESSN015          
      N=IABS(INU)                                                       BESSN016          
60    L=2*IFIX(ZMOD)+20                                                 BESSN017          
      IF(L-N-30) 80,90,90                                               BESSN018          
80    L=2*(15+N/2)                                                      BESSN019          
90    IF(.NOT.(N.GT.100.OR.REZ.GT.250..OR.REZ.LT.1.E-6)) GOTO 130       BESSN020          
      WRITE(61,110) REZ,NU                                              BESSN021          
110   FORMAT(//,10X,8HBESSNUX ,8HREJECT. ,8HZ OR NU ,8HOFF SCAL,3HE. ,  BESSN022          
     .2E18.5)                                                           BESSN023          
120   GOTO 1390                                                         BESSN024          
130   J=EXFLTF(J)                                                       BESSN025          
      LN=L                                                              BESSN026          
      ZRR=1./REZ                                                        BESSN027          
      ZSQRR=4.*ZRR*ZRR                                                  BESSN028          
      IF(IFLAG-1) 230,180,200                                           BESSN029          
180   ABC=1.E-300                                                       BESSN030          
      AC=ABC                                                            BESSN031          
      GOTO 240                                                          BESSN032          
200   WRITE(61,220) $ GOTO 1390                                         BESSN033          
220   FORMAT(8H OVERFLO,8HW HAS OC,8HCURRED A,8HFTER TWO,8H STARTIN,    BESSN034          
     .8HG ATTEMP,8HTS, CALL,8H REJECTE,8HD, RESUL,8HTS UNDEF,6HINED  )  BESSN035          
230   ABC=1.E-25                                                        BESSN036          
      AC=ABC                                                            BESSN037          
240   BC=0.                                                             BESSN038          
      BA=BC                                                             BESSN039          
      AA=BA                                                             BESSN040          
      IF(ABS(NU-FLOAT(INU))-1.E-10) 260,260,680                         BESSN041          
260   J=L-L/4*4-1                                                       BESSN042          
      GOTO 330                                                          BESSN043          
280   BA=BA+2E0*AE                                                      BESSN044          
      A=4.*FLOAT(J)/FLOAT(L)                                            BESSN045          
      BC=BC+A*AE                                                        BESSN046          
      AA=AC $ AC=AE                                                     BESSN047          
330   A=FLOAT(L)*FLOAT(L-1)*ZSQRR-2.*FLOAT(L)/FLOAT(L+1)                BESSN048          
      C=(1.-FLOAT(L))/FLOAT(1+L)                                        BESSN049          
      AE=A*AC+C*AA                                                      BESSN050          
      IF(L-3-N) 370,370,380                                             BESSN051          
370   BESJR(L-1)=AE                                                     BESSN052          
380   J=-J $ L=L-2                                                      BESSN053          
      IF(L.NE.0) GOTO 280                                               BESSN054          
      BA=BA+AE                                                          BESSN055          
      BESJR(1)=AE/BA                                                    BESSN056          
      A=ALOG(ZMOD)-0.115931516                                          BESSN057          
      BESYR(1)=(A*BESJR(1)+BC/BA)*0.6366197724                          BESSN058          
      L=1                                                               BESSN059          
      PIZR=-ZRR*0.6366197724                                            BESSN060          
      IF(EXFLTF(J).EQ.2) GOTO 520                                       BESSN061          
480   WRITE(61,490) REZ,NU                                              BESSN062          
490   FORMAT(/,10X,8HEXFLT ER,8HROR IN N,8HORMALISI,8HNG SUM B,6HESSEL  BESSN063          
     .,2E15.5/10X,8HSECOND A,8HTTEMPT W,8HITH NEW ,8HSTARTING,6H POINT) BESSN064          
      IFLAG=IFLAG+1 $ GOTO 60                                           BESSN065          
520   BESJR(L+2)=BESJR(L+2)/BA                                          BESSN066          
      BESJR(L+1)=REZ*(BESJR(L)+BESJR(L+2))/FLOAT(2*L)                   BESSN067          
540   BESYR(L+1)=(BESJR(L+1)*BESYR(L)+PIZR)/BESJR(L)                    BESSN068          
      L=L+1                                                             BESSN069          
      IF(L.EQ.L/2*2) GOTO 540                                           BESSN070          
      IF(EXFLTF(J).EQ.2) GOTO 610                                       BESSN071          
580   WRITE(61,600) L $ GOTO 1390                                       BESSN072          
600   FORMAT(/,10X,8HERROR EX,8HFLT IN B,8HESSEL RE,8HCURRENCE,5H. L= , BESSN073          
     .I6/10X,8HHIGHER O,8HRDERS GI,8HVE UNDEF,8HINED VAL,8HUES     )    BESSN074          
610   IF(.NOT.(L.GE.N+1)) GOTO 520                                      BESSN075          
      IF(NU.GE.0.) GOTO 120                                             BESSN076          
      M=(N+1)/2*2                                                       BESSN077          
      L=2                                                               BESSN078          
641   IF(L.GT.M) GOTO 649                                               BESSN079          
      BESJR(L)=-BESJR(L)                                                BESSN080          
      BESYR(L)=-BESYR(L)                                                BESSN081          
      L=L+2                                                             BESSN082          
      GOTO 641                                                          BESSN083          
649   CONTINUE                                                          BESSN084          
      GOTO 1390                                                         BESSN085          
C     FRACTIONAL ORDER                                                  BESSN086          
680   AAN=ABS(NU-FLOAT(INU))                                            BESSN087          
      IF(.NOT.(NU.GE.0.)) GOTO 720                                      BESSN088          
      J=1 $ GOTO 730                                                    BESSN089          
720   J=2                                                               BESSN090          
730   KQZ001=J                                                          BESSN091          
      IF(KQZ001.LT.1) KQZ001=1                                          BESSN092          
      IF(KQZ001.GT.4) KQZ001=4                                          BESSN093          
      GOTO(740,770,770,740),KQZ001                                      BESSN094          
740   BN=1.-AAN $ DD=AAN $ GOTO 800                                     BESSN095          
770   BN=AAN $ DD=2.-AAN $ L=L+1                                        BESSN096          
800   II=L/2                                                            BESSN097          
      YMOD=(2./ZMOD)**DD*(DD+2E0*FLOAT(II))*GAMMA(DD+FLOAT(II))/GAMMA(  BESSN098          
     .FLOAT(II)+1.)                                                     BESSN099          
      II=II+1                                                           BESSN100          
      GOTO 890                                                          BESSN101          
840   X=(DD+2E0*FLOAT(II))*(DD+FLOAT(II)-1.)/(FLOAT(II)*(DD+2E0*FLOAT(  BESSN102          
     .II)-2.))                                                          BESSN103          
      YMOD=YMOD/X                                                       BESSN104          
      BA=BA+YMOD*AE                                                     BESSN105          
870   AA=AC $ AC=AE                                                     BESSN106          
890   FLN=FLOAT(L)-BN+1.                                                BESSN107          
      A=FLN*(FLN-1.)*ZSQRR-2.*FLN/(FLN+1.)                              BESSN108          
      C=(1.-FLN)/(1.+FLN)                                               BESSN109          
      AE=A*AC+C*AA                                                      BESSN110          
      L=L-2 $ II=II-1                                                   BESSN111          
      KQZ001=J                                                          BESSN112          
      IF(KQZ001.LT.1) KQZ001=1                                          BESSN113          
      IF(KQZ001.GT.4) KQZ001=4                                          BESSN114          
      GOTO(960,990,1060,1030),KQZ001                                    BESSN115          
960   IF(L-1-INU) 970,970,840                                           BESSN116          
970   BESJR(L+1)=AE                                                     BESSN117          
      IF(L) 1100,1100,840                                               BESSN118          
990   IF(L+1) 1000,1000,840                                             BESSN119          
1000  M=-L                                                              BESSN120          
      BESJR(M)=AE                                                       BESSN121          
      IF(L+1-INU) 1130,870,870                                          BESSN122          
1030  IF(L-1-INU) 1040,1040,840                                         BESSN123          
1040  BESYR(L+1)=AE                                                     BESSN124          
      IF(L) 1100,1100,840                                               BESSN125          
1060  IF(L+1) 1070,1070,840                                             BESSN126          
1070  M=-L                                                              BESSN127          
      BESYR(M)=AE                                                       BESSN128          
      IF(L+1-INU) 1130,870,870                                          BESSN129          
1100  X=(DD+2E0*FLOAT(II))*(DD+FLOAT(II)-1.)/(FLOAT(II)*(DD+2E0*FLOAT(  BESSN130          
     .II)-2.))                                                          BESSN131          
      YMOD=YMOD/X                                                       BESSN132          
      BA=BA+YMOD*AE                                                     BESSN133          
1130  KQZ001=J                                                          BESSN134          
      IF(KQZ001.LT.1) KQZ001=1                                          BESSN135          
      IF(KQZ001.GT.4) KQZ001=4                                          BESSN136          
      GOTO(1140,1140,1210,1210),KQZ001                                  BESSN137          
1140  INU=-INU $ L=LN $ J=J+2                                           BESSN138          
      AAJ=1./BA                                                         BESSN139          
      BA=0.                                                             BESSN140          
      AA=BA $ AC=ABC                                                    BESSN141          
      GOTO 730                                                          BESSN142          
1210  AAY=1./BA                                                         BESSN143          
      INU=-INU                                                          BESSN144          
C                                                                       BESSN145          
      IF(.NOT.(EXFLTF(J).EQ.2)) GOTO 480                                BESSN146          
      FLN=SIGN(AAN,NU)                                                  BESSN147          
      BA=COS(3.141592654*FLN)                                           BESSN148          
      BB=SIN(3.141592654*FLN)                                           BESSN149          
      BESJR(1)=AAJ*BESJR(1)                                             BESSN150          
      BESYR(1)=(BESJR(1)*BA-AAY*BESYR(1))/BB                            BESSN151          
      FLN=FLN-SIGN(1.,NU)                                               BESSN152          
      L=1                                                               BESSN153          
1310  BESJR(L+2)=AAJ*BESJR(L+2)                                         BESSN154          
      BESYR(L+2)=(BESJR(L+2)*BA-AAY*BESYR(L+2))/BB                      BESSN155          
      FLN=FLN+SIGN(2.,NU)                                               BESSN156          
      BESJR(L+1)=REZ*(BESJR(L)+BESJR(L+2))/(2.*FLN)                     BESSN157          
      BESYR(L+1)=REZ*(BESYR(L)+BESYR(L+2))/(2.*FLN)                     BESSN158          
      IF(.NOT.(EXFLTF(J).EQ.2)) GOTO 580                                BESSN159          
      L=L+2                                                             BESSN160          
      IF(L.GE.N+1) GOTO 120                                             BESSN161          
      GOTO 1310                                                         BESSN162          
1390  RETURN                                                            BESSN163          
      END                                                               BESSN164          
