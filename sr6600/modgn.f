      SUBROUTINE MODGN(X,Y,F,Q,NP,NPTS,IBLOT,IPRINT)                    MODGN002          
      DIMENSION X(1),Y(1),F(1),IBLOT(1),B(100),C(10),FDBAR(100),BB(60), MODGN003          
     .D(10),YBAR(10),FO(100),FBAR(10),FD(1000),FSAVE(10)                MODGN004          
C                                                                       MODGN005          
C***   PROGRAMMED BY D.E. SHAW, C.S.I.R.O. DIVISION OF MATHEMATICAL STATMODGN006          
C***   ALPHA HOUSE, 60 KING ST., NEWTOWN, N.S.W., AUSTRALIA             MODGN007          
C                                                                       MODGN008          
C X IS ARRAY OF INDEPENDANT VARIABLE VALUES,Y IS ARRAY OF DEPENDANT VAR MODGN009          
C ARRAY F CONTAINS ESTIMATES OF PARAMETERS                              MODGN010          
C Q RETURNS SUM OF SQUARES OF RESIDUALS FOR BEST APPROXIMATION          MODGN011          
C NP IS NUMBER OF PARAMETERS, NPTS IS NUMBER OF DATA POINTS             MODGN012          
C IBLOT IS ARRAY SPECIFYING PARAMETERS NOT TO BE CHANGED                MODGN013          
C IBLOT(I)=0, PARAMETER TO VARY. IBLOT(I)=1, PARAMETER REMAINS FIXED    MODGN014          
C IPRINT IS OPTION ON PRINTOUT FROM MODG. =1 FOR PRINTOUT, 0 OTHERWISE  MODGN015          
C                                                                       MODGN016          
C  FIRST CONSISTENT ESTIMATES                                           MODGN017          
      NC=NPTS/NP                                                        MODGN018          
      I=1                                                               MODGN019          
31    IF(I.GT.NP) GOTO 39                                               MODGN020          
      FSAVE(I)=F(I)                                                     MODGN021          
      I=I+1                                                             MODGN022          
      GOTO 31                                                           MODGN023          
39    CONTINUE                                                          MODGN024          
      I=1                                                               MODGN025          
51    IF(I.GT.NP) GOTO 59                                               MODGN026          
      YSUM=0.0                                                          MODGN027          
      J=1                                                               MODGN028          
71    IF(J.GT.NC) GOTO 79                                               MODGN029          
      K=(I-1)*NC+J                                                      MODGN030          
      YSUM=YSUM+Y(K)                                                    MODGN031          
      J=J+1                                                             MODGN032          
      GOTO 71                                                           MODGN033          
79    CONTINUE                                                          MODGN034          
      YBAR(I)=YSUM/FLOAT(NC)                                            MODGN035          
      I=I+1                                                             MODGN036          
      GOTO 51                                                           MODGN037          
59    CONTINUE                                                          MODGN038          
110   CALL FILL(X,FO,FD,F,NP,NPTS)                                      MODGN039          
      I=1                                                               MODGN040          
121   IF(I.GT.NP) GOTO 129                                              MODGN041          
      FSUM=0.0                                                          MODGN042          
      J=1                                                               MODGN043          
141   IF(J.GT.NC) GOTO 149                                              MODGN044          
      K=(I-1)*NC+J                                                      MODGN045          
      FSUM=FSUM+FO(K)                                                   MODGN046          
      J=J+1                                                             MODGN047          
      GOTO 141                                                          MODGN048          
149   CONTINUE                                                          MODGN049          
      FBAR(I)=FSUM/FLOAT(NC)                                            MODGN050          
      I=I+1                                                             MODGN051          
      GOTO 121                                                          MODGN052          
129   CONTINUE                                                          MODGN053          
      I=1                                                               MODGN054          
181   IF(I.GT.NP) GOTO 189                                              MODGN055          
      J=1                                                               MODGN056          
191   IF(J.GT.NP) GOTO 199                                              MODGN057          
      FSUM=0.0                                                          MODGN058          
      K=1                                                               MODGN059          
211   IF(K.GT.NC) GOTO 219                                              MODGN060          
      L=(I-1)*NPTS+(J-1)*NC+K                                           MODGN061          
      FSUM=FSUM+FD(L)                                                   MODGN062          
      K=K+1                                                             MODGN063          
      GOTO 211                                                          MODGN064          
219   CONTINUE                                                          MODGN065          
      M=(I-1)*NP+J                                                      MODGN066          
      FDBAR(M)=FSUM/FLOAT(NC)                                           MODGN067          
      J=J+1                                                             MODGN068          
      GOTO 191                                                          MODGN069          
199   CONTINUE                                                          MODGN070          
      I=I+1                                                             MODGN071          
      GOTO 181                                                          MODGN072          
189   CONTINUE                                                          MODGN073          
      I=1                                                               MODGN074          
261   IF(I.GT.NP) GOTO 269                                              MODGN075          
      C(I)=0.0                                                          MODGN076          
      J=1                                                               MODGN077          
281   IF(J.GT.NP) GOTO 289                                              MODGN078          
      K=(I-1)*NP+J                                                      MODGN079          
      B(K)=0.0                                                          MODGN080          
      J=J+1                                                             MODGN081          
      GOTO 281                                                          MODGN082          
289   CONTINUE                                                          MODGN083          
      I=I+1                                                             MODGN084          
      GOTO 261                                                          MODGN085          
269   CONTINUE                                                          MODGN086          
      I=1                                                               MODGN087          
311   IF(I.GT.NP) GOTO 319                                              MODGN088          
      J=1                                                               MODGN089          
321   IF(J.GT.NP) GOTO 329                                              MODGN090          
      JI=(J-1)*NP+I                                                     MODGN091          
      C(J)=C(J)+(YBAR(I)-FBAR(I))*FDBAR(JI)                             MODGN092          
      K=1                                                               MODGN093          
351   IF(K.GT.NP) GOTO 359                                              MODGN094          
      JK=(J-1)*NP+K                                                     MODGN095          
      KI=(K-1)*NP+I                                                     MODGN096          
      B(JK)=B(JK)+FDBAR(JI)*FDBAR(KI)                                   MODGN097          
      K=K+1                                                             MODGN098          
      GOTO 351                                                          MODGN099          
359   CONTINUE                                                          MODGN100          
      J=J+1                                                             MODGN101          
      GOTO 321                                                          MODGN102          
329   CONTINUE                                                          MODGN103          
      I=I+1                                                             MODGN104          
      GOTO 311                                                          MODGN105          
319   CONTINUE                                                          MODGN106          
      ICNT=0                                                            MODGN107          
      I=1                                                               MODGN108          
401   IF(I.GT.NP) GOTO 409                                              MODGN109          
      J=1                                                               MODGN110          
411   IF(J.GT.I) GOTO 419                                               MODGN111          
      ICNT=ICNT+1                                                       MODGN112          
      K=(I-1)*NP+J                                                      MODGN113          
      BB(ICNT)=B(K)                                                     MODGN114          
      J=J+1                                                             MODGN115          
      GOTO 411                                                          MODGN116          
419   CONTINUE                                                          MODGN117          
      I=I+1                                                             MODGN118          
      GOTO 401                                                          MODGN119          
409   CONTINUE                                                          MODGN120          
      CALL MATI(BB,C,D,NP,1)                                            MODGN121          
      CALL FILL(X,FO,FD,F,NP,NPTS)                                      MODGN122          
      QSAVE=FUNC(1,Y,YBAR,FBAR,FO,NP,NPTS)                              MODGN123          
      I=1                                                               MODGN124          
481   IF(I.GT.11) GOTO 489                                              MODGN125          
      J=1                                                               MODGN126          
491   IF(J.GT.NP) GOTO 499                                              MODGN127          
      F(J)=F(J)+D(J)/(2.0**(I-1))                                       MODGN128          
      J=J+1                                                             MODGN129          
      GOTO 491                                                          MODGN130          
499   CONTINUE                                                          MODGN131          
      CALL FILL(X,FO,FD,F,NP,NPTS)                                      MODGN132          
      II=1                                                              MODGN133          
521   IF(II.GT.NP) GOTO 529                                             MODGN134          
      FSUM=0.0                                                          MODGN135          
      JJ=1                                                              MODGN136          
541   IF(JJ.GT.NC) GOTO 549                                             MODGN137          
      KK=(II-1)*NC+JJ                                                   MODGN138          
      FSUM=FSUM+FO(KK)                                                  MODGN139          
      JJ=JJ+1                                                           MODGN140          
      GOTO 541                                                          MODGN141          
549   CONTINUE                                                          MODGN142          
      FBAR(II)=FSUM/FLOAT(NC)                                           MODGN143          
      II=II+1                                                           MODGN144          
      GOTO 521                                                          MODGN145          
529   CONTINUE                                                          MODGN146          
      QBAR=FUNC(1,Y,YBAR,FBAR,FO,NP,NPTS)                               MODGN147          
      IF(QSAVE-QBAR) 600,600,610                                        MODGN148          
600   CONTINUE                                                          MODGN149          
      I=I+1                                                             MODGN150          
      GOTO 481                                                          MODGN151          
489   CONTINUE                                                          MODGN152          
610   IF(.NOT.(IPRINT.NE.0)) GOTO 640                                   MODGN153          
      WRITE(61,630)(F(I),I=1,NP),QBAR                                   MODGN154          
630   FORMAT(15X,11E11.4)                                               MODGN155          
640   IF(.NOT.(QBAR.LT.1.0/(10.0**5))) GOTO 110                         MODGN156          
      IF(.NOT.(IPRINT.NE.0)) GOTO 680                                   MODGN157          
      WRITE(61,670)(F(I),I=1,NP)                                        MODGN158          
670   FORMAT(5(/),5X,8HCONSISTE,8HNT FIRST,8H ESTIMAT,6HES ARE/15X,     MODGN159          
     .10E12.5,5(/))                                                     MODGN160          
680   I=1                                                               MODGN161          
681   IF(I.GT.NP) GOTO 689                                              MODGN162          
      IF(.NOT.(IBLOT(I).NE.0)) GOTO 710                                 MODGN163          
      F(I)=FSAVE(I)                                                     MODGN164          
710   CONTINUE                                                          MODGN165          
C                                                                       MODGN166          
C FINAL SOLUTION, USING F AS STARTING VALUES                            MODGN167          
C                                                                       MODGN168          
      I=I+1                                                             MODGN169          
      GOTO 681                                                          MODGN170          
689   CONTINUE                                                          MODGN171          
720   CALL FILL(X,FO,FD,F,NP,NPTS)                                      MODGN172          
      I=1                                                               MODGN173          
731   IF(I.GT.NP) GOTO 739                                              MODGN174          
      C(I)=0.0                                                          MODGN175          
      J=1                                                               MODGN176          
751   IF(J.GT.NP) GOTO 759                                              MODGN177          
      IJ=(I-1)*NP+J                                                     MODGN178          
      B(IJ)=0.0                                                         MODGN179          
      J=J+1                                                             MODGN180          
      GOTO 751                                                          MODGN181          
759   CONTINUE                                                          MODGN182          
      I=I+1                                                             MODGN183          
      GOTO 731                                                          MODGN184          
739   CONTINUE                                                          MODGN185          
      K=1                                                               MODGN186          
781   IF(K.GT.NPTS) GOTO 789                                            MODGN187          
      I=1                                                               MODGN188          
791   IF(I.GT.NP) GOTO 799                                              MODGN189          
      IK=(I-1)*NPTS+K                                                   MODGN190          
      C(I)=C(I)+(Y(K)-FO(K))*FD(IK)                                     MODGN191          
      J=1                                                               MODGN192          
821   IF(J.GT.NP) GOTO 829                                              MODGN193          
      IJ=(I-1)*NP+J                                                     MODGN194          
      JK=(J-1)*NPTS+K                                                   MODGN195          
      B(IJ)=B(IJ)+FD(IK)*FD(JK)                                         MODGN196          
      J=J+1                                                             MODGN197          
      GOTO 821                                                          MODGN198          
829   CONTINUE                                                          MODGN199          
      I=I+1                                                             MODGN200          
      GOTO 791                                                          MODGN201          
799   CONTINUE                                                          MODGN202          
      K=K+1                                                             MODGN203          
      GOTO 781                                                          MODGN204          
789   CONTINUE                                                          MODGN205          
      I=1                                                               MODGN206          
861   IF(I.GT.NP) GOTO 869                                              MODGN207          
      IF(.NOT.(IBLOT(I).NE.0)) GOTO 970                                 MODGN208          
      ICOL=1                                                            MODGN209          
881   IF(ICOL.GT.NP) GOTO 889                                           MODGN210          
      IPOS=(ICOL-1)*NP+IBLOT(I)                                         MODGN211          
      B(IPOS)=0.0                                                       MODGN212          
      ICOL=ICOL+1                                                       MODGN213          
      GOTO 881                                                          MODGN214          
889   CONTINUE                                                          MODGN215          
      JP1=(IBLOT(I)-1)*NP+1                                             MODGN216          
      JP2=IBLOT(I)*NP                                                   MODGN217          
      IPOS=JP1                                                          MODGN218          
931   IF(IPOS.GT.JP2) GOTO 939                                          MODGN219          
      B(IPOS)=0.0                                                       MODGN220          
      IPOS=IPOS+1                                                       MODGN221          
      GOTO 931                                                          MODGN222          
939   CONTINUE                                                          MODGN223          
      IBL=IBLOT(I)                                                      MODGN224          
      C(IBL)=0.0                                                        MODGN225          
970   CONTINUE                                                          MODGN226          
      I=I+1                                                             MODGN227          
      GOTO 861                                                          MODGN228          
869   CONTINUE                                                          MODGN229          
      ICNT=0                                                            MODGN230          
      I=1                                                               MODGN231          
991   IF(I.GT.NP) GOTO 999                                              MODGN232          
      J=1                                                               MODGN233          
1001  IF(J.GT.I) GOTO 1009                                              MODGN234          
      ICNT=ICNT+1                                                       MODGN235          
      K=(I-1)*NP+J                                                      MODGN236          
      BB(ICNT)=B(K)                                                     MODGN237          
      J=J+1                                                             MODGN238          
      GOTO 1001                                                         MODGN239          
1009  CONTINUE                                                          MODGN240          
      I=I+1                                                             MODGN241          
      GOTO 991                                                          MODGN242          
999   CONTINUE                                                          MODGN243          
      CALL MATI(BB,C,D,NP,1)                                            MODGN244          
      CALL FILL(X,FO,FD,F,NP,NPTS)                                      MODGN245          
      QSAVE=FUNC(2,Y,YBAR,FBAR,FO,NP,NPTS)                              MODGN246          
      I=1                                                               MODGN247          
1071  IF(I.GT.11) GOTO 1079                                             MODGN248          
      J=1                                                               MODGN249          
1081  IF(J.GT.NP) GOTO 1089                                             MODGN250          
      F(J)=F(J)+D(J)/(2.0**(I-1))                                       MODGN251          
      J=J+1                                                             MODGN252          
      GOTO 1081                                                         MODGN253          
1089  CONTINUE                                                          MODGN254          
      CALL FILL(X,FO,FD,F,NP,NPTS)                                      MODGN255          
      Q=FUNC(2,Y,YBAR,FBAR,FO,NP,NPTS)                                  MODGN256          
      IF(QSAVE-Q) 1130,1130,1140                                        MODGN257          
1130  CONTINUE                                                          MODGN258          
      I=I+1                                                             MODGN259          
      GOTO 1071                                                         MODGN260          
1079  CONTINUE                                                          MODGN261          
1140  IF(.NOT.(IPRINT.NE.0)) GOTO 1160                                  MODGN262          
      WRITE(61,630)(F(I),I=1,NP),Q                                      MODGN263          
1160  DIF=ABS((Q-QSAVE)/QSAVE)                                          MODGN264          
      IF(100.0*DIF-0.01) 1180,1180,720                                  MODGN265          
1180  IF(.NOT.(IPRINT.NE.0)) GOTO 1290                                  MODGN266          
      WRITE(61,1200)(F(I),I=1,NP)                                       MODGN267          
1200  FORMAT(5(/),5X,8HEFFICIEN,8HT FINAL ,8HESTIMATE,5HS ARE/15X,      MODGN268          
     .10E12.5/)                                                         MODGN269          
      WRITE(61,1220) Q                                                  MODGN270          
1220  FORMAT(5X,8HS.S.R. =,1H ,E14.7,5(/))                              MODGN271          
      WRITE(61,1240)                                                    MODGN272          
1240  FORMAT(15X,1HX,16X,1HY,15X,4HYHAT,13X,3HDIF/)                     MODGN273          
      I=1                                                               MODGN274          
1251  IF(I.GT.NPTS) GOTO 1259                                           MODGN275          
      YDIF=Y(I)-FO(I)                                                   MODGN276          
      WRITE(61,1280) X(I),Y(I),FO(I),YDIF                               MODGN277          
      I=I+1                                                             MODGN278          
      GOTO 1251                                                         MODGN279          
1259  CONTINUE                                                          MODGN280          
1280  FORMAT(10X,4(E12.5,5X))                                           MODGN281          
1290  CONTINUE                                                          MODGN282          
      RETURN                                                            MODGN283          
      END                                                               MODGN284          
      SUBROUTINE MATI(A,B,C,P1,ISOL)                                    MODGN285          
      INTEGER P,P1                                                      MODGN286          
      DIMENSION A(1),B(1),C(1)                                          MODGN287          
C                                                                       MODGN288          
C MATI NEEDED TO ACCOMPANY SUBROUTINE MODGN                             MODGN289          
C***   PROGRAMMED BY D.E. SHAW, C.S.I.R.O. DIVISION OF MATHEMATICAL STATMODGN290          
C***   ALPHA HOUSE, 60 KING ST., NEWTOWN, N.S.W., AUSTRALIA             MODGN291          
C REVISION ON 29/11/68                                                  MODGN292          
C                                                                       MODGN293          
C     INVERSION OF POSITIVE DEFINITE (SYMMETRIC) MATRIX BY MODIFIED     MODGN294          
C     SQUARE ROOT METHOD USING P1(P1+1)/2 STORAGE LOCATIONS (SAZONOV,   MODGN295          
C     GEODEZIYA I AEROFOTOSYEMKA,NO6, 1962)                             MODGN296          
C                                                                       MODGN297          
      P=P1-1                                                            MODGN298          
      I1=1                                                              MODGN299          
      I=2                                                               MODGN300          
41    IF(I.GT.P1) GOTO 49                                               MODGN301          
      ILESS1=I-1                                                        MODGN302          
      I3=1                                                              MODGN303          
      J=2                                                               MODGN304          
71    IF(J.GT.ILESS1) GOTO 79                                           MODGN305          
      I2=I1+J                                                           MODGN306          
      JLESS1=J-1                                                        MODGN307          
      JO=1                                                              MODGN308          
101   IF(JO.GT.JLESS1) GOTO 109                                         MODGN309          
      I4=I3+JO $ I5=I1+JO                                               MODGN310          
      A(I2)=A(I2)-A(I4)*A(I5)                                           MODGN311          
      JO=JO+1                                                           MODGN312          
      GOTO 101                                                          MODGN313          
109   CONTINUE                                                          MODGN314          
      I3=I3+J                                                           MODGN315          
      J=J+1                                                             MODGN316          
      GOTO 71                                                           MODGN317          
79    CONTINUE                                                          MODGN318          
      I3=0                                                              MODGN319          
      I5=I1+I                                                           MODGN320          
      K=1                                                               MODGN321          
171   IF(K.GT.ILESS1) GOTO 179                                          MODGN322          
      I2=I1+K                                                           MODGN323          
      I3=I3+K                                                           MODGN324          
      TEMP=A(I2)/A(I3)                                                  MODGN325          
      A(I5)=A(I5)-A(I2)*TEMP                                            MODGN326          
      A(I2)=TEMP                                                        MODGN327          
      K=K+1                                                             MODGN328          
      GOTO 171                                                          MODGN329          
179   CONTINUE                                                          MODGN330          
      I1=I1+I                                                           MODGN331          
      I=I+1                                                             MODGN332          
      GOTO 41                                                           MODGN333          
49    CONTINUE                                                          MODGN334          
      I1=1                                                              MODGN335          
      I=2                                                               MODGN336          
251   IF(I.GT.P1) GOTO 259                                              MODGN337          
      ILESS1=I-1                                                        MODGN338          
      J=1                                                               MODGN339          
271   IF(J.GT.ILESS1) GOTO 279                                          MODGN340          
      I2=I1+J                                                           MODGN341          
      JPLUS1=J+1                                                        MODGN342          
      K=JPLUS1                                                          MODGN343          
301   IF(K.GT.ILESS1) GOTO 309                                          MODGN344          
      I3=J+K*(K-1)/2                                                    MODGN345          
      I4=I1+K                                                           MODGN346          
      A(I2)=A(I2)+A(I3)*A(I4)                                           MODGN347          
      K=K+1                                                             MODGN348          
      GOTO 301                                                          MODGN349          
309   CONTINUE                                                          MODGN350          
      A(I2)=-A(I2)                                                      MODGN351          
      J=J+1                                                             MODGN352          
      GOTO 271                                                          MODGN353          
279   CONTINUE                                                          MODGN354          
      I1=I1+I                                                           MODGN355          
      I=I+1                                                             MODGN356          
      GOTO 251                                                          MODGN357          
259   CONTINUE                                                          MODGN358          
      I1=0                                                              MODGN359          
      I=1                                                               MODGN360          
371   IF(I.GT.P1) GOTO 379                                              MODGN361          
      I1=I1+I                                                           MODGN362          
      A(I1)=1./A(I1)                                                    MODGN363          
      I=I+1                                                             MODGN364          
      GOTO 371                                                          MODGN365          
379   CONTINUE                                                          MODGN366          
      I=1                                                               MODGN367          
401   IF(I.GT.P) GOTO 409                                               MODGN368          
      IPLUS1=I+1                                                        MODGN369          
      K=IPLUS1                                                          MODGN370          
421   IF(K.GT.P1) GOTO 429                                              MODGN371          
      I1=K*(K-1)/2 $ I2=I1+K $ I3=I1+I                                  MODGN372          
      TEMP=A(I2)*A(I3)                                                  MODGN373          
      KLESSI=K-I                                                        MODGN374          
      J=1                                                               MODGN375          
481   IF(J.GT.KLESSI) GOTO 489                                          MODGN376          
      JO=I+J-1                                                          MODGN377          
      I4=JO*(JO-1)/2+I $ I5=I1+JO                                       MODGN378          
      A(I4)=A(I4)+A(I5)*TEMP                                            MODGN379          
      J=J+1                                                             MODGN380          
      GOTO 481                                                          MODGN381          
489   CONTINUE                                                          MODGN382          
      A(I3)=TEMP                                                        MODGN383          
      K=K+1                                                             MODGN384          
      GOTO 421                                                          MODGN385          
429   CONTINUE                                                          MODGN386          
      I=I+1                                                             MODGN387          
      GOTO 401                                                          MODGN388          
409   CONTINUE                                                          MODGN389          
      IF(.NOT.(ISOL.NE.0)) GOTO 680                                     MODGN390          
      I2=0                                                              MODGN391          
      I=1                                                               MODGN392          
561   IF(I.GT.P1) GOTO 569                                              MODGN393          
      C(I)=0.0                                                          MODGN394          
      K=I2                                                              MODGN395          
      J=1                                                               MODGN396          
591   IF(J.GT.I) GOTO 599                                               MODGN397          
      K=K+1                                                             MODGN398          
      C(I)=C(I)+A(K)*B(J)                                               MODGN399          
      J=J+1                                                             MODGN400          
      GOTO 591                                                          MODGN401          
599   CONTINUE                                                          MODGN402          
      I1=I+1                                                            MODGN403          
      K=K+I                                                             MODGN404          
      J=I1                                                              MODGN405          
641   IF(J.GT.P1) GOTO 649                                              MODGN406          
      C(I)=C(I)+A(K)*B(J)                                               MODGN407          
      K=K+J                                                             MODGN408          
      J=J+1                                                             MODGN409          
      GOTO 641                                                          MODGN410          
649   CONTINUE                                                          MODGN411          
      I2=I2+I                                                           MODGN412          
      I=I+1                                                             MODGN413          
      GOTO 561                                                          MODGN414          
569   CONTINUE                                                          MODGN415          
680   CONTINUE                                                          MODGN416          
      RETURN                                                            MODGN417          
      END                                                               MODGN418          
      FUNCTION FUNC(I,Y,YBAR,FBAR,FO,NP,NPTS)                           MODGN419          
      DIMENSION Y(1),YBAR(1),FBAR(1),FO(1)                              MODGN420          
C                                                                       MODGN421          
C FUNC NEEDED TO ACCOMPANY SUBROUTINE MODGN                             MODGN422          
C***   PROGRAMMED BY D.E. SHAW, C.S.I.R.O. DIVISION OF MATHEMATICAL STATMODGN423          
C***   ALPHA HOUSE, 60 KING ST., NEWTOWN, N.S.W., AUSTRALIA             MODGN424          
C REVISION ON 29/11/18                                                  MODGN425          
C                                                                       MODGN426          
      FUNC=0.0                                                          MODGN427          
      KQZ001=I                                                          MODGN428          
      IF(KQZ001.LT.1) KQZ001=1                                          MODGN429          
      IF(KQZ001.GT.2) KQZ001=2                                          MODGN430          
      GOTO(40,70),KQZ001                                                MODGN431          
40    J=1                                                               MODGN432          
41    IF(J.GT.NP) GOTO 49                                               MODGN433          
      FUNC=FUNC+(YBAR(J)-FBAR(J))**2                                    MODGN434          
      J=J+1                                                             MODGN435          
      GOTO 41                                                           MODGN436          
49    CONTINUE                                                          MODGN437          
      GOTO 100                                                          MODGN438          
70    JK=1                                                              MODGN439          
71    IF(JK.GT.NPTS) GOTO 79                                            MODGN440          
      FUNC=FUNC+(Y(JK)-FO(JK))**2                                       MODGN441          
      JK=JK+1                                                           MODGN442          
      GOTO 71                                                           MODGN443          
79    CONTINUE                                                          MODGN444          
100   RETURN                                                            MODGN445          
      END                                                               MODGN446          
