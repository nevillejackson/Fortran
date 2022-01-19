      SUBROUTINE LIN36(H,MFORM,NFORM,NO,LQ,CO,IFCO,NONQ,OX,OY)          LIN36002          
      INTEGER START,GOING                                               LIN36003          
      DIMENSION H(MFORM,NFORM),LQ(MFORM,1),INEW(18),JNEW(1),NIDLIST(1), LIN36004          
     .JLIST(1),ILIST(1),KK(1),KL(1),CO(1),LEQ(3),KONTOR(48),INN(4),JNN( LIN36005          
     .4)                                                                LIN36006          
      EQUIVALENCE(LEC,LEQ),(INEW(2),JNEW),(INEW(5),NIDLIST),(INEW(12),  LIN36007          
     .JLIST),(INEW(15),ILIST)                                           LIN36008          
C     VERSION 3 - NOV 1968.  J.A.B. PALMER, DIV. OF COMP. RES., CANBRA  LIN36009          
C     H - ARRAY OF HEIGHTS                                              LIN36010          
C     MFORM, NFORM - DIMENSIONS OF H                                    LIN36011          
C     NO -  NUMBER OF CONTOURS, MAX 48                                  LIN36012          
C     LQ - WORKING SPACE, DIMENSION AT LEAST (MFORM, 2*NFORM)           LIN36013          
C     IFCO - 1 IF CONTOUR HEIGHTS ARE PRESET IN CO                      LIN36014          
C          - 0 IF PROGRAM IS TO SET HEIGHTS                             LIN36015          
C     NONQ - 0 IF SQUARE   SEARCH IS TO BE USED FOR PEN-UP MOVEMENT     LIN36016          
C          - 1 IF ARBITRARY SEARCH IS TO BE USED FOR PEN-UP MOVEMENT    LIN36017          
C               THIS MAY BE ADVANTAGEOUS IN VERY SPARSE PLOTS           LIN36018          
C     OX,OY - OFFSET.  IE., ORIGIN OF THE MAP WILL BE AT (OX,OY)        LIN36019          
C               (ORIGIN IS H(1,1), LOWER LEFT CORNER)                   LIN36020          
C     VARIABLE DIMENSIONS OF H AND LQ MAY WITH ADVANTAGE BE FIXED, BUT  LIN36021          
C     DIMENSIONS OF H MUST EQUAL EXACTLY ITS DIMENSIONS IN THE CALLING  LIN36022          
C     PROGRAM.  IT IS SUFFICIENT, HOWEVER, FOR THE DIMENSIONS OF LQ TO  LIN36023          
C     BE OF AT LEAST ADEQUATE SIZE.  IN THIS CASE LQ NEED NOT BE A      LIN36024          
C     FORMAL PARAMETER, BUT CAN BE LOCAL TO THIS ROUTINE.               LIN36025          
      DATA(INEW=1,0,-1,0,1,2,3,4,1,2,3,0,0,0,1,0,0,0)                   LIN36026          
      IF(NO.LT.1) GOTO 1710                                             LIN36027          
C     LOCALISE FORMAL PARAMETERS                                        LIN36028          
      N=NFORM                                                           LIN36029          
      M=MFORM                                                           LIN36030          
      MPLUSN=M+N                                                        LIN36031          
      IP=M*N*2                                                          LIN36032          
C     CREATE MASKING ARRAY                                              LIN36033          
      KONTOR(1)=1                                                       LIN36034          
      I=2                                                               LIN36035          
101   IF(I.GT.NO) GOTO 109                                              LIN36036          
      KONTOR(I)=2*KONTOR(I-1)                                           LIN36037          
      I=I+1                                                             LIN36038          
      GOTO 101                                                          LIN36039          
109   CONTINUE                                                          LIN36040          
      IF(NONQ.NE.0) GOTO 150                                            LIN36041          
      ASSIGN 670 TO LOOSEEK                                             LIN36042          
      GOTO 160                                                          LIN36043          
150   ASSIGN 970 TO LOOSEEK                                             LIN36044          
C     CLEAR STORE                                                       LIN36045          
160   I=1                                                               LIN36046          
161   IF(I.GT.IP) GOTO 169                                              LIN36047          
      LQ(I,1)=0                                                         LIN36048          
C     CALCULATION OF CO.  THIS MAY BE OMMITTED IF CO IS PRESET          LIN36049          
      I=I+1                                                             LIN36050          
      GOTO 161                                                          LIN36051          
169   CONTINUE                                                          LIN36052          
      IF(IFCO.NE.0) GOTO 260                                            LIN36053          
C     FIND MAX AND MIN HEIGHT, EQUALLY DIVIDE INTO NO CONTOURS          LIN36054          
      HMAX=H(1,1)                                                       LIN36055          
      HMIN=HMAX                                                         LIN36056          
      J=1                                                               LIN36057          
201   IF(J.GT.N) GOTO 209                                               LIN36058          
      I=1                                                               LIN36059          
211   IF(I.GT.M) GOTO 219                                               LIN36060          
      HMIN=AMIN1(HMIN,H(I,J))                                           LIN36061          
      HMAX=AMAX1(HMAX,H(I,J))                                           LIN36062          
      I=I+1                                                             LIN36063          
      GOTO 211                                                          LIN36064          
219   CONTINUE                                                          LIN36065          
      J=J+1                                                             LIN36066          
      GOTO 201                                                          LIN36067          
209   CONTINUE                                                          LIN36068          
      IN=1                                                              LIN36069          
241   IF(IN.GT.NO) GOTO 249                                             LIN36070          
      CO(IN)=FLOAT(IN-1)*(HMAX-HMIN)/FLOAT(NO-1)+HMIN                   LIN36071          
C     END OF CALCULATION OF CO                                          LIN36072          
C     SET THE BITS FOR EACH POINT WHERE A CONTOUR CROSSES A SIDE        LIN36073          
C     LOOK AT EACH SIDE IN THE MESH                                     LIN36074          
      IN=IN+1                                                           LIN36075          
      GOTO 241                                                          LIN36076          
249   CONTINUE                                                          LIN36077          
260   COMIN=CO(1)                                                       LIN36078          
      COIN=CO(2)-COMIN                                                  LIN36079          
      K=1                                                               LIN36080          
281   IF(K.GT.2) GOTO 289                                               LIN36081          
      KN=N*(2-K)                                                        LIN36082          
      JS=3-K                                                            LIN36083          
      JK=0                                                              LIN36084          
      J=JS                                                              LIN36085          
321   IF(J.GT.N) GOTO 329                                               LIN36086          
      JKN=M*(JK+KN)                                                     LIN36087          
      JK=JK+1                                                           LIN36088          
      IK=0                                                              LIN36089          
      I=K                                                               LIN36090          
361   IF(I.GT.M) GOTO 369                                               LIN36091          
      IK=IK+1                                                           LIN36092          
C     IF A CONTOUR CROSSES THIS SIDE, SET THE REPRESENTATIVE BIT IN     LIN36093          
C     THE WORD IN LQ THAT REPRESENTS THE SIDE                           LIN36094          
C     HIK=H(I-K+1,J+K-2)                                                LIN36095          
      HIK=H(IK,JK)                                                      LIN36096          
      HIJ=H(I,J)                                                        LIN36097          
      HMAX=AMAX1(HIJ,HIK)                                               LIN36098          
      IF(HMAX.LT.COMIN) GOTO 610                                        LIN36099          
      HMIN=AMIN1(HIJ,HIK)                                               LIN36100          
      IF(NO.NE.1) GOTO 480                                              LIN36101          
      IF(HMIN.GT.COMIN) GOTO 610                                        LIN36102          
      IF(HMAX.LT.COMIN) GOTO 610                                        LIN36103          
      ITO=1                                                             LIN36104          
      IFROM=ITO                                                         LIN36105          
      GOTO 580                                                          LIN36106          
C     SET BITS IN LQ WITHIN THE RANGE OF THE ENDS OF THE LINE SEGMENT   LIN36107          
C     BETWEEN THE POINTS OF HEIGHT HIJ AND HIK                          LIN36108          
480   IV=INT((HMIN-COMIN)/COIN)                                         LIN36109          
      IF(IV.GT.0) GOTO 530                                              LIN36110          
      IF(HMIN.GT.COMIN) GOTO 530                                        LIN36111          
      IFROM=1                                                           LIN36112          
      GOTO 540                                                          LIN36113          
530   IFROM=IV+2                                                        LIN36114          
540   ITO=INT((HMAX-COMIN)/COIN)+1                                      LIN36115          
      IF(.NOT.(ITO.GT.NO)) GOTO 560                                     LIN36116          
      ITO=NO                                                            LIN36117          
560   CONTINUE                                                          LIN36118          
      IF(ITO.LT.IFROM) GOTO 610                                         LIN36119          
580   IJK=IK+JKN                                                        LIN36120          
      ICO=IFROM                                                         LIN36121          
591   IF(ICO.GT.ITO) GOTO 599                                           LIN36122          
      LQ(IJK,1)=LQ(IJK,1)+KONTOR(ICO)                                   LIN36123          
C     LQ(I-K+1, J+K+(2-K)*N-2)                                          LIN36124          
      ICO=ICO+1                                                         LIN36125          
      GOTO 591                                                          LIN36126          
599   CONTINUE                                                          LIN36127          
610   CONTINUE                                                          LIN36128          
C     END OF SETTING UP                                                 LIN36129          
C     SET PARAMETERS PRIOR TO FIRST SEARCH                              LIN36130          
      I=I+1                                                             LIN36131          
      GOTO 361                                                          LIN36132          
369   CONTINUE                                                          LIN36133          
      J=J+1                                                             LIN36134          
      GOTO 321                                                          LIN36135          
329   CONTINUE                                                          LIN36136          
      K=K+1                                                             LIN36137          
      GOTO 281                                                          LIN36138          
289   CONTINUE                                                          LIN36139          
      JLIST(3)=N                                                        LIN36140          
      JLIST(1)=JLIST(3)                                                 LIN36141          
      II=1                                                              LIN36142          
      JJ=II                                                             LIN36143          
      ISP=JJ                                                            LIN36144          
      LOOBACK=0                                                         LIN36145          
C     IF, OWING TO ROUNDING, AN UNBALANCED POINT OCCURS, SET FLAG TO    LIN36146          
C     TO AVOID SEARCH FOR SUCCESSOR LOOPING                             LIN36147          
650   ITRI=2                                                            LIN36148          
C     USE SELECTED METHOD OF SEARCH                                     LIN36149          
      GOTO LOOSEEK                                                      LIN36150          
C     SQUARE PERIPHERAL SEARCH STARTS HERE                              LIN36151          
670   I=0                                                               LIN36152          
C     SEARCH FOR NEAREST UNDRAWN CONTOUR TO CURRENT PEN POSITION  II,JJ LIN36153          
680   I=I+1                                                             LIN36154          
      JNN(3)=I                                                          LIN36155          
      INN(1)=JNN(3)                                                     LIN36156          
      JNN(4)=-I                                                         LIN36157          
      INN(2)=JNN(4)                                                     LIN36158          
      J=0                                                               LIN36159          
      JNN(2)=J                                                          LIN36160          
      JNN(1)=JNN(2)                                                     LIN36161          
      INN(4)=JNN(1)                                                     LIN36162          
      INN(3)=INN(4)                                                     LIN36163          
C     LOOK IN ALL 8 DIRECTIONS CORRESPONDING TO EQUAL PLOTTER DISTANCES LIN36164          
710   LO=1                                                              LIN36165          
711   IF(LO.GT.4) GOTO 719                                              LIN36166          
      IL=II+INN(LO)                                                     LIN36167          
      IF(IL.LT.1.OR.IL.GT.M) GOTO 840                                   LIN36168          
      JOS=1                                                             LIN36169          
      IF(LO.LT.3) GOTO 770                                              LIN36170          
      JOS=3                                                             LIN36171          
770   JOF=JOS+1                                                         LIN36172          
      LI=JOS                                                            LIN36173          
781   IF(LI.GT.JOF) GOTO 789                                            LIN36174          
      JL=JJ+JNN(LI)                                                     LIN36175          
      IF(JL.LT.1.OR.JL.GT.N) GOTO 830                                   LIN36176          
      IF(LQ(IL,JL).NE.0) GOTO 890                                       LIN36177          
      KQZ001=JL+N                                                       LIN36178          
      IF(LQ(IL,KQZ001).NE.0) GOTO 920                                   LIN36179          
830   CONTINUE                                                          LIN36180          
      LI=LI+1                                                           LIN36181          
      GOTO 781                                                          LIN36182          
789   CONTINUE                                                          LIN36183          
840   CONTINUE                                                          LIN36184          
      LO=LO+1                                                           LIN36185          
      GOTO 711                                                          LIN36186          
719   CONTINUE                                                          LIN36187          
      J=J+1                                                             LIN36188          
      INN(3)=J                                                          LIN36189          
      JNN(1)=INN(3)                                                     LIN36190          
      INN(4)=-J                                                         LIN36191          
      JNN(2)=INN(4)                                                     LIN36192          
      IF(J.LE.I) GOTO 710                                               LIN36193          
      IF(I.LT.MPLUSN) GOTO 680                                          LIN36194          
      GOTO 1700                                                         LIN36195          
890   NID=2                                                             LIN36196          
      JS=JL                                                             LIN36197          
      GOTO 940                                                          LIN36198          
920   NID=3                                                             LIN36199          
      JS=JL+N                                                           LIN36200          
940   II=IL                                                             LIN36201          
      JJ=JL                                                             LIN36202          
C     END OF SQUARE PERIPHERAL SEARCH                                   LIN36203          
      GOTO 1060                                                         LIN36204          
C     ARBITRARY SEARCH STARTS HERE                                      LIN36205          
C     LOOK AT NEXT SIDE                                                 LIN36206          
970   IF(LQ(ISP,1).NE.0) GOTO 1000                                      LIN36207          
C     MT, MOVE POINTER DOWN AND TEST IF FINISHED                        LIN36208          
      ISP=ISP+1                                                         LIN36209          
      IF(ISP.LE.IP) GOTO 970                                            LIN36210          
      GOTO 1700                                                         LIN36211          
C     UNSCRAMBLE INDEX - ISP=II+M*(JS-1)                                LIN36212          
1000  JS=ISP/M+1                                                        LIN36213          
      JJ=JS                                                             LIN36214          
      II=ISP-M*(JS-1)                                                   LIN36215          
      NID=2                                                             LIN36216          
      IF(JS.LE.N) GOTO 1060                                             LIN36217          
      JJ=JS-N                                                           LIN36218          
      NID=3                                                             LIN36219          
C     END OF ARBITRARY SEARCH                                           LIN36220          
C     EXIT WITH II, JJ SET TO SQUARE,  NID SET TO SQUARE SIDE           LIN36221          
1060  START=1                                                           LIN36222          
C     FIND WHICH CONTOUR CROSSES AT A POINT IN THIS SIDE                LIN36223          
      LCO=1                                                             LIN36224          
1071  IF(LCO.GT.NO) GOTO 1079                                           LIN36225          
      LEM=LQ(II,JS).AND.KONTOR(LCO)                                     LIN36226          
      IF(LEM.NE.0) GOTO 1110                                            LIN36227          
1100  CONTINUE                                                          LIN36228          
      LCO=LCO+1                                                         LIN36229          
      GOTO 1071                                                         LIN36230          
1079  CONTINUE                                                          LIN36231          
1110  KONLCO=KONTOR(LCO)                                                LIN36232          
C     SET X,Y BY INTERPOLATING                                          LIN36233          
      CALL CONC36(NID,II,JJ,CO(LCO),X,Y,M,H)                            LIN36234          
      CALL PLOT(X+OX,Y+OY,3)                                            LIN36235          
      GOING=0                                                           LIN36236          
      NED=NID                                                           LIN36237          
      GOTO 1380                                                         LIN36238          
1170  START=0                                                           LIN36239          
C     ARE WE GOING OVER THE EDGE.(TEST IF SQ. IS OUTSIDE)               LIN36240          
1180  IF(II.EQ.0.OR.JJ.EQ.0.OR.II.GE.M.OR.JJ.GE.N) GOTO 1620            LIN36241          
C     SET LEQ(J) NE 0 FOR ANY OF THE 3 DESTINATION SIDES THAT HAS AN    LIN36242          
C     UNCANCELLED POINT OF THE CURRENT CONTOUR                          LIN36243          
      K=1                                                               LIN36244          
1191  IF(K.GT.3) GOTO 1199                                              LIN36245          
      KQZ001=NID+K                                                      LIN36246          
      KQZ002=NIDLIST(KQZ001)                                            LIN36247          
      KQZ003=II+ILIST(KQZ002)                                           LIN36248          
      KQZ004=NID+K                                                      LIN36249          
      KQZ005=NIDLIST(KQZ004)                                            LIN36250          
      KQZ006=JJ+JLIST(KQZ005)                                           LIN36251          
      LEQ(K)=LQ(KQZ003,KQZ006).AND.KONLCO                               LIN36252          
C     EXAMINE EACH SIDE OF THE SQUARE FOR A CROSSING POINT              LIN36253          
C     IS THERE A A POINT TO RIGHT                                       LIN36254          
      K=K+1                                                             LIN36255          
      GOTO 1191                                                         LIN36256          
1199  CONTINUE                                                          LIN36257          
      IF(LEQ(3).NE.0) GOTO 1240                                         LIN36258          
C     IS THERE A POINT OPPOSITE                                         LIN36259          
      IF(LEQ(2).NE.0) GOTO 1280                                         LIN36260          
C     IF NOT R OR O THEN IS THERE ONE TO LEFT                           LIN36261          
      IF(LEQ(1).NE.0) GOTO 1350                                         LIN36262          
      GOTO 1620                                                         LIN36263          
C     IS THERE A POINT TO LEFT AS WELL AS TO RIGHT                      LIN36264          
1240  IF(LEQ(1).NE.0) GOTO 1310                                         LIN36265          
C     CHECK WHETHER THERE ARE JUST TWO DESTINATION POINTS               LIN36266          
C     IF THERE ARE, EITHER THIS IS A CLOSURE OR THE LINE IS STARTING    LIN36267          
C     IN THE WRONG DIRECTION                                            LIN36268          
      IF(LEQ(2).NE.0) GOTO 1630                                         LIN36269          
C     O.K., SET RIGHT                                                   LIN36270          
1260  NAD=3                                                             LIN36271          
      GOTO 1360                                                         LIN36272          
1280  IF(LEQ(1).NE.0) GOTO 1630                                         LIN36273          
C     O.K., SET OPPOSITE                                                LIN36274          
      NAD=2                                                             LIN36275          
      GOTO 1360                                                         LIN36276          
1310  IF(.NOT.(LEQ(2).NE.0)) GOTO 1630                                  LIN36277          
C     IF BOTH RIGHT AND LEFT, FIND THE NEARER                           LIN36278          
      CALL CONC36(NIDLIST(NID+1),II,JJ,CO(LCO),XR,YR,M,H)               LIN36279          
      CALL CONC36(NIDLIST(NID+3),II,JJ,CO(LCO),XL,YL,M,H)               LIN36280          
      IF(XL*XL+YL*YL-XR*XR-YR*YR.LE.2E0*X*(XL-XR)+2E0*Y*(YL-YR)) GOTO   LIN36281          
     .1260                                                              LIN36282          
C     SET LEFT                                                          LIN36283          
1350  NAD=1                                                             LIN36284          
1360  IF(LOOBACK.NE.0) GOTO 1480                                        LIN36285          
C     NED IS DESTINATION SIDE IN THE CURRENT SQUARE                     LIN36286          
      KQZ001=NID+NAD                                                    LIN36287          
      NED=NIDLIST(KQZ001)                                               LIN36288          
C     ENTER HERE AT A START TO TEST WHETHER CURRENT SIDE IS AN EDGE     LIN36289          
C     IF IT IS, POINT IS CANCELLED, OTHERWISE NOT                       LIN36290          
1380  IT=II+ILIST(NED)                                                  LIN36291          
      JT=JJ+JLIST(NED)                                                  LIN36292          
      IF(.NOT.(START.NE.0)) GOTO 1530                                   LIN36293          
C     IS THIS LINE AN EDGE, TEST ONLY AT START                          LIN36294          
      IF((IT.EQ.1.AND.JT.GT.N).OR.IT.EQ.M.OR.JT.EQ.1.OR.JT.EQ.N) GOTO   LIN36295          
     .1530                                                              LIN36296          
C     IF STARTING NOT ON AN EDGE, LOOK BACK TO SEE IF THIS IS A         LIN36297          
C     CONTINUATION OF A LOOSE END.  IF IT IS, CANCEL THE POINT.         LIN36298          
C     IF NOT, DONT                                                      LIN36299          
      LOOBACK=1                                                         LIN36300          
      NAD=0                                                             LIN36301          
C     SET SQUARE TO NEXT                                                LIN36302          
      II=II+INEW(NID)                                                   LIN36303          
      JJ=JJ+JNEW(NID)                                                   LIN36304          
      NID=NIDLIST(NID+2)                                                LIN36305          
      GOTO 1180                                                         LIN36306          
C     RESET SQUARE AND NID HAVING RETURNED FROM LOOKING BACK.           LIN36307          
C     IF THERE IS A POINT BEHIND, NAD WILL BE NON ZERO                  LIN36308          
1480  LOOBACK=0                                                         LIN36309          
      II=II+INEW(NID)                                                   LIN36310          
      JJ=JJ+JNEW(NID)                                                   LIN36311          
      NID=NIDLIST(NID+2)                                                LIN36312          
C     IF THERE WAS A POINT BEHIND, LEAVE A LOOSE END FOR SUBSEQUENT     LIN36313          
C     CLOSURE.  OTHERWISE CANCEL POINT                                  LIN36314          
      IF(NAD.NE.0) GOTO 1170                                            LIN36315          
C     CANCEL THIS POINT, SET X,Y, AND DRAW THE LINE                     LIN36316          
1530  LQ(IT,JT)=LQ(IT,JT).AND.(.NOT.KONLCO)                             LIN36317          
      IF(START.NE.0) GOTO 1170                                          LIN36318          
      CALL CONC36(NED,II,JJ,CO(LCO),X,Y,M,H)                            LIN36319          
      CALL PLOT(X+OX,Y+OY,4)                                            LIN36320          
      GOING=1                                                           LIN36321          
      NID=NIDLIST(NED+2)                                                LIN36322          
      II=II+INEW(NED)                                                   LIN36323          
      JJ=JJ+JNEW(NED)                                                   LIN36324          
      GOTO 1170                                                         LIN36325          
1620  IF(LOOBACK.NE.0) GOTO 1480                                        LIN36326          
C     EITHER WE HAVE GONE OFF THE EDGE, OR WE ARE TRYING TO START       LIN36327          
C     THE WRONG WAY                                                     LIN36328          
1630  IF(GOING.NE.0) GOTO 650                                           LIN36329          
C     TRY THE OTHER WAY                                                 LIN36330          
      II=II+INEW(NID)                                                   LIN36331          
      JJ=JJ+JNEW(NID)                                                   LIN36332          
      NID=NIDLIST(NID+2)                                                LIN36333          
C     CANCEL THIS POINT                                                 LIN36334          
      LQ(IT,JT)=LQ(IT,JT).AND.(.NOT.KONLCO)                             LIN36335          
      ITRI=ITRI-1                                                       LIN36336          
      IF(ITRI.GT.0) GOTO 1170                                           LIN36337          
      GOTO 650                                                          LIN36338          
1700  CONTINUE                                                          LIN36339          
1710  RETURN                                                            LIN36340          
      END                                                               LIN36341          
      SUBROUTINE CONC36(NED,IFORM,JFORM,CON,X,Y,MM,H)                   LIN36342          
      DIMENSION H(1),KNED(6),INED(1)                                    LIN36343          
      EQUIVALENCE(KNED(3),INED(1))                                      LIN36344          
C     INTERPOLATE TO FIND POSITION  (X,Y)  IN SIDE  NED                 LIN36345          
C     SQUARE  (IFORM,JFORM)  WHERE IT IS CROSSED BY                     LIN36346          
C     CONTOUR OF HEIGHT  CON                                            LIN36347          
      DATA(KNED=0,1,1,0,1,0)                                            LIN36348          
C     LOCALISE                                                          LIN36349          
      I=IFORM                                                           LIN36350          
      J=JFORM                                                           LIN36351          
      M=MM                                                              LIN36352          
      K=KNED(NED)                                                       LIN36353          
      IF(INED(NED).EQ.0) GOTO 190                                       LIN36354          
      X=I-K                                                             LIN36355          
      IK1J1=I-K+1+J*M                                                   LIN36356          
      IK1J=IK1J1-M                                                      LIN36357          
      DEM=H(IK1J)-H(IK1J1)                                              LIN36358          
      IF(ABS(DEM).LT.1.E-12) GOTO 150                                   LIN36359          
      Y=FLOAT(J)+(H(IK1J1)-CON)/DEM                                     LIN36360          
C     Y = J + (H(I-K+1,J+1)-CON)/(H(I-K+1,J)-H(I-K+1,J+1))              LIN36361          
      GOTO 250                                                          LIN36362          
150   Y=FLOAT(J)-.5                                                     LIN36363          
      GOTO 250                                                          LIN36364          
170   X=FLOAT(I)-.5                                                     LIN36365          
      GOTO 250                                                          LIN36366          
190   Y=J-K                                                             LIN36367          
      IJK1=I+M*(J-K)                                                    LIN36368          
      I1JK1=IJK1+1                                                      LIN36369          
      DEM=H(IJK1)-H(I1JK1)                                              LIN36370          
      IF(ABS(DEM).LT.1.E-12) GOTO 170                                   LIN36371          
      X=FLOAT(I)+(H(I1JK1)-CON)/DEM                                     LIN36372          
C     X = I + (H(I+1,J-K+1)-CON)/(H(I,J-K+1)-H(I+1,J-K+1))              LIN36373          
250   RETURN                                                            LIN36374          
      END                                                               LIN36375          
