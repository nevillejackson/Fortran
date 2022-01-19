      SUBROUTINE MINIM(F,STEP,NOP,FUNC,MAX,IPRINT,A,B,C,STOCRIT,NLOOP,  MINIM002          
     .IQUAD,SIMP,VAR)                                                   MINIM003          
      DIMENSION F(1),STEP(1),G(21,20),H(21),PBAR(20),PSTAR(20),PSTST(20 MINIM004          
     .),AVAL(20),BMAT(210),PMIN(20),VC(210),VAR(1)                      MINIM005          
      EQUIVALENCE(PMIN,PSTAR),(AVAL,PBAR)                               MINIM006          
C                                                                       MINIM007          
C   A PROGRAM FOR FUNCTION MINIMISATION USING THE SIMPLEX METHOD.       MINIM008          
C                                                                       MINIM009          
C   FOR DETAILS, SEE NELDER AND MEAD, THE COMPUTER JOURNAL, JANUARY,1965MINIM010          
C                                                                       MINIM011          
C   PROGRAMMED BY D.E. SHAW, DIV. OF MATH. STATS., C.S.I.R.O.,          MINIM012          
C   60 KING ST., NEWTOWN, N.S.W.                                        MINIM013          
C                                                                       MINIM014          
C   THE PARAMETER LIST IS AS FOLLOWS -                                  MINIM015          
C   F         ON ENTRY, THE STARTING VALUES OF THE PARAMETERS           MINIM016          
C             ON EXIT, THE PARAMETER VALUES SPECIFYING THE MINIMUM POINTMINIM017          
C   STEP      THE INITIAL STEP SIZES                                    MINIM018          
C   NOP       THE NUMBER OF PARAMETERS(INCLUDING THOSE TO BE HELD FIXED)MINIM019          
C   FUNC      ON EXIT, THE FUNCTION VALUE AT THE MINIMUM                MINIM020          
C   MAX       THE MAXIMUM NUMBER OF FUNCTION EVALUATIONS TO BE ALLOWED  MINIM021          
C   IPRINT    PARAMETER TO CONTROL OUTPUT FROM MINIM                    MINIM022          
C      LESS THAN 0    NO OUTPUT                                         MINIM023          
C      EQUAL TO 0     REPORTS OF INITIAL EVIDENCE OF CONVERGENCE,       MINIM024          
C                     FINAL CONVERGENCE (WITH PARAMETER AND FUNCTION    MINIM025          
C                     VALUES THERE), OVERRUNNING OF MAX, AND THE FITTINGMINIM026          
C                     OF THE QUADRATIC SURFACE IF CALLED FOR.           MINIM027          
C      GREATER THAN 0 AS FOR IPRINT = 0, PLUS A PROGRESS REPORT ON THE  MINIM028          
C                     MINIMISATION EVERY IPRINT FUNCTION EVALUATIONS.   MINIM029          
C   A         REFLECTION COEFFICIENT                                    MINIM030          
C   B         CONTRACTION COEFFICIENT                                   MINIM031          
C   C         EXPANSION COEFFICIENT                                     MINIM032          
C   STOPCRIT  STOPPING CRITERION                                        MINIM033          
C   NLOOP  CONVERGENCE IS TESTED FOR EVERY NLOOP TIMES THE PROCESS      MINIM034          
C          CHANGES THE SIMPLEX. AFTER INITIAL CONVERGENCE, NLOOP FURTHERMINIM035          
C          CHANGES ARE ALLOWED BEFORE TESTING FOR FINAL CONVERGENCE     MINIM036          
C          ON EXIT NLOOP=0 IF MAX EXCEEDED                              MINIM037          
C                   =-1 IF INFORMATION MATRIX IS NOT POSITIVE DEFINITE  MINIM038          
C   IQUAD  =1  IF FITTING OF QUADRATIC SURFACE REQUIRED                 MINIM039          
C          =0  IF NOT                                                   MINIM040          
C   SIMP   CRITERION FOR EXPANDING SIMPLEX TO OVERCOME ROUNDING ERRORS  MINIM041          
C          BEFORE FITTING QUADRATIC SURFACE                             MINIM042          
C   VAR    ON EXIT, CONTAINS THE DIAGONAL ELEMENTS OF THE INVERSE OF THEMINIM043          
C          INFORMATION MATRIX                                           MINIM044          
C                                                                       MINIM045          
C F,STEP,NOP,MAX,IPRINT,A(OR A,B AND C - SEE * BELOW ),STOPCRIT,NLOOP,  MINIM046          
C IQUAD AND SIMP ( SEE ** BELOW ) MUST BE SET BY THE CALLING PROGRAMME  MINIM047          
C                                                                       MINIM048          
C  *  IF A = 0.0 , PROGRAMME SETS A = 1.0 , B = 0.5 , C = 2.0           MINIM049          
C     IF A =/ 0.0 , B AND C MUST BE SET IN THE CALLING PROGRAMME        MINIM050          
C                                                                       MINIM051          
C **  IF IQUAD = 0 , SIMP IS NOT USED                                   MINIM052          
C                                                                       MINIM053          
C     F,STEP ( AND VAR IF IQUAD = 1 ) MUST BE DIMENSION AT LEAST NOP IN MINIM054          
C     THE CALLING PROGRAMME                                             MINIM055          
C                                                                       MINIM056          
C   AS THE PROGRAM IS CURRENTLY SET UP, IT WILL DEAL WITH UP TO 20      MINIM057          
C   PARAMETERS                                                          MINIM058          
C                                                                       MINIM059          
C   IF PROGRESS REPORTS DESIRED, PRINT HEADING FOR THEM                 MINIM060          
      IF(IPRINT) 50,50,30                                               MINIM061          
30    WRITE(61,40) IPRINT                                               MINIM062          
40    FORMAT(8H PROGRES,8HS REPORT,6H EVERY,I4,8H FUNCTIO,8HN EVALUA,   MINIM063          
     .5HTIONS//8H EVAL. N,8HO.  FUNC,8H. VALUE ,10X,8HPARAMETE,2HRS)    MINIM064          
C                                                                       MINIM065          
C   APPROX IS USED TO TEST CLOSENESS TO ZERO                            MINIM066          
50    APPROX=1.0E-30                                                    MINIM067          
C                                                                       MINIM068          
C     IF NO VALUES OF A,B AND C ARE SELECTED , I.E. A IS SET = 0.0 ,    MINIM069          
C     THEN THE PROGRAMME SETS A = 1.0 , B = 0.5 , C = 2.0               MINIM070          
      IF(.NOT.(ABS(A).LT.APPROX)) GOTO 100                              MINIM071          
      A=1.0 $ B=0.5 $ C=2.0                                             MINIM072          
C                                                                       MINIM073          
C   NAP IS THE NUMBER OF PARAMETERS TO BE VARIED, I.E. WITH STEP NOT 0  MINIM074          
100   NAP=0                                                             MINIM075          
      IFLAG=0                                                           MINIM076          
      LOOP=IFLAG                                                        MINIM077          
      I=1                                                               MINIM078          
121   IF(I.GT.NOP) GOTO 129                                             MINIM079          
      IF(.NOT.(ABS(STEP(I)).GT.APPROX)) GOTO 150                        MINIM080          
      NAP=NAP+1                                                         MINIM081          
150   CONTINUE                                                          MINIM082          
C                                                                       MINIM083          
C     IF NAP = 0 , EVALUATE FUNCTION AT STARTING POINT AND RETURN       MINIM084          
      I=I+1                                                             MINIM085          
      GOTO 121                                                          MINIM086          
129   CONTINUE                                                          MINIM087          
      IF(NAP.NE.0) GOTO 190                                             MINIM088          
      CALL GIVEF(F,FUNC)                                                MINIM089          
      GOTO 3790                                                         MINIM090          
C                                                                       MINIM091          
C   SET UP INITIAL SIMPLEX                                              MINIM092          
190   I=1                                                               MINIM093          
191   IF(I.GT.NOP) GOTO 199                                             MINIM094          
      G(1,I)=F(I)                                                       MINIM095          
      I=I+1                                                             MINIM096          
      GOTO 191                                                          MINIM097          
199   CONTINUE                                                          MINIM098          
      IROW=2                                                            MINIM099          
      I=1                                                               MINIM100          
221   IF(I.GT.NOP) GOTO 229                                             MINIM101          
      IF(ABS(STEP(I)).LT.APPROX) GOTO 280                               MINIM102          
      J=1                                                               MINIM103          
241   IF(J.GT.NOP) GOTO 249                                             MINIM104          
      G(IROW,J)=F(J)                                                    MINIM105          
      J=J+1                                                             MINIM106          
      GOTO 241                                                          MINIM107          
249   CONTINUE                                                          MINIM108          
      G(IROW,I)=G(IROW,I)+STEP(I)                                       MINIM109          
      IROW=IROW+1                                                       MINIM110          
280   CONTINUE                                                          MINIM111          
      I=I+1                                                             MINIM112          
      GOTO 221                                                          MINIM113          
229   CONTINUE                                                          MINIM114          
      NP1=NAP+1                                                         MINIM115          
      NEVAL=0                                                           MINIM116          
      I=1                                                               MINIM117          
311   IF(I.GT.NP1) GOTO 319                                             MINIM118          
      J=1                                                               MINIM119          
321   IF(J.GT.NOP) GOTO 329                                             MINIM120          
      F(J)=G(I,J)                                                       MINIM121          
      J=J+1                                                             MINIM122          
      GOTO 321                                                          MINIM123          
329   CONTINUE                                                          MINIM124          
      CALL GIVEF(F,H(I))                                                MINIM125          
      NEVAL=NEVAL+1                                                     MINIM126          
C                                                                       MINIM127          
C     ALL POINTS OF THE INITIAL SIMPLEX ARE OUTPUT IF IPRINT> 0         MINIM128          
      IF(IPRINT) 390,390,370                                            MINIM129          
370   WRITE(61,380) NEVAL,H(I),(F(J),J=1,NOP)                           MINIM130          
380   FORMAT(/3X,I4,4X,E13.6,8(1X,E13.6)/24X,8(1X,E13.6)/24X,4(1X,E13.6 MINIM131          
     .))                                                                MINIM132          
390   CONTINUE                                                          MINIM133          
C                                                                       MINIM134          
C   NOW FOLLOWS THE BASIC LOOP, I.E. GIVEN A SIMPLEX, TO DETERMINE THE  MINIM135          
C   NEW SIMPLEX AND TEST FOR CONVERGENCE AS REQUIRED (FOLLOWING THE     MINIM136          
C   FLOW CHART  GIVEN IN NELDER AND MEAD)                               MINIM137          
C                                                                       MINIM138          
C     TO STATEMENT 13 , DETERMINE MAXIMUM AND MINIMUM POINTS OF         MINIM139          
C     CURRENT SIMPLEX (FUNCTION VALUES ARE HMAX AND HMIN)               MINIM140          
      I=I+1                                                             MINIM141          
      GOTO 311                                                          MINIM142          
319   CONTINUE                                                          MINIM143          
400   LOOP=LOOP+1                                                       MINIM144          
      IMIN=1                                                            MINIM145          
      IMAX=IMIN                                                         MINIM146          
      HMIN=H(1)                                                         MINIM147          
      HMAX=HMIN                                                         MINIM148          
      I=2                                                               MINIM149          
431   IF(I.GT.NP1) GOTO 439                                             MINIM150          
      IF(.NOT.(H(I).GT.H(IMAX))) GOTO 470                               MINIM151          
      IMAX=I                                                            MINIM152          
      HMAX=H(I)                                                         MINIM153          
470   IF(.NOT.(H(I).LT.H(IMIN))) GOTO 500                               MINIM154          
      IMIN=I                                                            MINIM155          
      HMIN=H(I)                                                         MINIM156          
500   CONTINUE                                                          MINIM157          
C                                                                       MINIM158          
C TO STATEMENT 18 , FIND CENTROID OF ALL VERTICES, EXCLUDING THE MAXIMUMMINIM159          
      I=I+1                                                             MINIM160          
      GOTO 431                                                          MINIM161          
439   CONTINUE                                                          MINIM162          
      I=1                                                               MINIM163          
511   IF(I.GT.NOP) GOTO 519                                             MINIM164          
      PBAR(I)=0.0                                                       MINIM165          
      I=I+1                                                             MINIM166          
      GOTO 511                                                          MINIM167          
519   CONTINUE                                                          MINIM168          
      I=1                                                               MINIM169          
531   IF(I.GT.NP1) GOTO 539                                             MINIM170          
      IF(I.EQ.IMAX) GOTO 570                                            MINIM171          
      J=1                                                               MINIM172          
551   IF(J.GT.NOP) GOTO 559                                             MINIM173          
      PBAR(J)=PBAR(J)+G(I,J)/FLOAT(NAP)                                 MINIM174          
      J=J+1                                                             MINIM175          
      GOTO 551                                                          MINIM176          
559   CONTINUE                                                          MINIM177          
570   CONTINUE                                                          MINIM178          
C                                                                       MINIM179          
C   REFLECT MAXIMUM THROUGH PBAR TO PSTAR, AND EVALUATE FUNCTION AT     MINIM180          
C   PSTAR  (GIVING HSTAR)                                               MINIM181          
      I=I+1                                                             MINIM182          
      GOTO 531                                                          MINIM183          
539   CONTINUE                                                          MINIM184          
      I=1                                                               MINIM185          
581   IF(I.GT.NOP) GOTO 589                                             MINIM186          
      PSTAR(I)=A*(PBAR(I)-G(IMAX,I))+PBAR(I)                            MINIM187          
      I=I+1                                                             MINIM188          
      GOTO 581                                                          MINIM189          
589   CONTINUE                                                          MINIM190          
      CALL GIVEF(PSTAR,HSTAR)                                           MINIM191          
C                                                                       MINIM192          
C     NEXT 5 STATEMENTS TEST IF PROGRESS REPORT IS REQUIRED AT PRESENT  MINIM193          
C     AND IF SO , PROVIDE ONE                                           MINIM194          
C     THIS PROCEDURE OCCURS FREQUENTLY IN THE PROGRAMME                 MINIM195          
      NEVAL=NEVAL+1                                                     MINIM196          
      IF(IPRINT) 670,670,630                                            MINIM197          
630   J=NEVAL/IPRINT                                                    MINIM198          
      K=NEVAL-J*IPRINT                                                  MINIM199          
      IF(K.NE.0) GOTO 670                                               MINIM200          
      WRITE(61,380) NEVAL,HSTAR,(PSTAR(J),J=1,NOP)                      MINIM201          
670   IF(.NOT.(HSTAR.LT.HMIN)) GOTO 820                                 MINIM202          
C                                                                       MINIM203          
C   IF HSTAR LESS THAN HMIN REFLECT PBAR THROUGH PSTAR TO GIVE PSTST,   MINIM204          
C   AND EVALUATE FUNCTION THERE (GIVING HSTST)                          MINIM205          
      I=1                                                               MINIM206          
681   IF(I.GT.NOP) GOTO 689                                             MINIM207          
      PSTST(I)=C*(PSTAR(I)-PBAR(I))+PSTAR(I)                            MINIM208          
      I=I+1                                                             MINIM209          
      GOTO 681                                                          MINIM210          
689   CONTINUE                                                          MINIM211          
      CALL GIVEF(PSTST,HSTST)                                           MINIM212          
      NEVAL=NEVAL+1                                                     MINIM213          
      IF(IPRINT) 770,770,730                                            MINIM214          
730   J=NEVAL/IPRINT                                                    MINIM215          
      K=NEVAL-J*IPRINT                                                  MINIM216          
      IF(K.NE.0) GOTO 770                                               MINIM217          
      WRITE(61,380) NEVAL,HSTST,(PSTST(J),J=1,NOP)                      MINIM218          
770   IF(.NOT.(HSTST.LT.HMIN)) GOTO 1190                                MINIM219          
C                                                                       MINIM220          
C   IF HSTST LESS THAN HMIN REPLACE MAXIMUM POINT OF CURRENT SIMPLEX BY MINIM221          
C   PSTST AN HMAX BY HSTAR, THEN TEST (STATEMENT 26 ONWARD)             MINIM222          
      I=1                                                               MINIM223          
781   IF(I.GT.NOP) GOTO 789                                             MINIM224          
      G(IMAX,I)=PSTST(I)                                                MINIM225          
      I=I+1                                                             MINIM226          
      GOTO 781                                                          MINIM227          
789   CONTINUE                                                          MINIM228          
      H(IMAX)=HSTST                                                     MINIM229          
      GOTO 1220                                                         MINIM230          
C                                                                       MINIM231          
C   IF HSTAR NOT LESS THAN HMIN, TEST IF HSTAR GREATER THAN FUNCTION    MINIM232          
C   VALUE AT ALL VERTICES OTHER THAN MAXIMUM ONE                        MINIM233          
820   I=1                                                               MINIM234          
821   IF(I.GT.NP1) GOTO 829                                             MINIM235          
      IF(I.EQ.IMAX) GOTO 850                                            MINIM236          
      IF(HSTAR.LT.H(I)) GOTO 1190                                       MINIM237          
850   CONTINUE                                                          MINIM238          
C                                                                       MINIM239          
C   IF IT IS LESS THAN (AT LEAST) ONE OF THESE VERTICES, REPLACE MAXIMUMMINIM240          
C   POINT OF CURRENT SIMPLEX BY PSTAR AND HMAX BY HSTAR, THEN TEST      MINIM241          
C   (STATEMENT 26 ONWARD)                                               MINIM242          
C                                                                       MINIM243          
C   IF HSTAR GREATER THAN ALL FUNCTION VALUES EXCLUDING THE MAXIMUM,    MINIM244          
C   TEST IF HSTAR GREATER THAN HMAX                                     MINIM245          
C   IF NOT, REPLACE MAXIMUM POINT BY PSTAR AND HMAX BY HSTAR FOR        MINIM246          
C   WHICHEVER SIMPLEX NOW IN STORE (I.E. DEPENDING ON WHETHER HSTAR WAS MINIM247          
C   GREATER OR LESS THAN HMAX), CALCULATE THE CONTRACTED POINT PSTST ANDMINIM248          
C   THE FUNCTION VALUE THERE, HSTST                                     MINIM249          
      I=I+1                                                             MINIM250          
      GOTO 821                                                          MINIM251          
829   CONTINUE                                                          MINIM252          
      IF(HSTAR.GT.HMAX) GOTO 900                                        MINIM253          
      I=1                                                               MINIM254          
871   IF(I.GT.NOP) GOTO 879                                             MINIM255          
      G(IMAX,I)=PSTAR(I)                                                MINIM256          
      I=I+1                                                             MINIM257          
      GOTO 871                                                          MINIM258          
879   CONTINUE                                                          MINIM259          
      H(IMAX)=HSTAR                                                     MINIM260          
      HMAX=H(IMAX)                                                      MINIM261          
900   I=1                                                               MINIM262          
901   IF(I.GT.NOP) GOTO 909                                             MINIM263          
      PSTST(I)=B*G(IMAX,I)+(1.0-B)*PBAR(I)                              MINIM264          
      I=I+1                                                             MINIM265          
      GOTO 901                                                          MINIM266          
909   CONTINUE                                                          MINIM267          
      CALL GIVEF(PSTST,HSTST)                                           MINIM268          
      NEVAL=NEVAL+1                                                     MINIM269          
      IF(IPRINT) 990,990,950                                            MINIM270          
950   J=NEVAL/IPRINT                                                    MINIM271          
      K=NEVAL-J*IPRINT                                                  MINIM272          
      IF(K.NE.0) GOTO 990                                               MINIM273          
      WRITE(61,380) NEVAL,HSTST,(PSTST(J),J=1,NOP)                      MINIM274          
990   IF(HSTST.GT.HMAX) GOTO 1040                                       MINIM275          
C                                                                       MINIM276          
C   IF HSTST LESS THAN HMAX, REPLACE MAXIMUM POINT BY PSTST AND HMAX BY MINIM277          
C   HSTST, THEN TEST (STATEMENT 41 ONWARD)                              MINIM278          
      I=1                                                               MINIM279          
1001  IF(I.GT.NOP) GOTO 1009                                            MINIM280          
      G(IMAX,I)=PSTST(I)                                                MINIM281          
      I=I+1                                                             MINIM282          
      GOTO 1001                                                         MINIM283          
1009  CONTINUE                                                          MINIM284          
      H(IMAX)=HSTST                                                     MINIM285          
      GOTO 1220                                                         MINIM286          
C                                                                       MINIM287          
C   IF HSTST NOT LESS THAN HMAX, REPLACE EACH POINT OF THE CURRENT      MINIM288          
C   SIMPLEX BY A POINT MIDWAY BETWEEN ITS CURRENT POSITION AND THE      MINIM289          
C   POSITION OF THE MINIMUM POINT OF THE CURRENT SIMPLEX. EVALUATE      MINIM290          
C  FUNCTION AT EACH NEW VERTEX THEN TEST (STATEMENT 41 ONWARD)          MINIM291          
1040  I=1                                                               MINIM292          
1041  IF(I.GT.NP1) GOTO 1049                                            MINIM293          
      J=1                                                               MINIM294          
1051  IF(J.GT.NOP) GOTO 1059                                            MINIM295          
      G(I,J)=(G(I,J)+G(IMIN,J))/2.0                                     MINIM296          
      J=J+1                                                             MINIM297          
      GOTO 1051                                                         MINIM298          
1059  CONTINUE                                                          MINIM299          
      I=I+1                                                             MINIM300          
      GOTO 1041                                                         MINIM301          
1049  CONTINUE                                                          MINIM302          
      I=1                                                               MINIM303          
1071  IF(I.GT.NP1) GOTO 1079                                            MINIM304          
      J=1                                                               MINIM305          
1081  IF(J.GT.NOP) GOTO 1089                                            MINIM306          
      F(J)=G(I,J)                                                       MINIM307          
      J=J+1                                                             MINIM308          
      GOTO 1081                                                         MINIM309          
1089  CONTINUE                                                          MINIM310          
      CALL GIVEF(F,H(I))                                                MINIM311          
      NEVAL=NEVAL+1                                                     MINIM312          
      IF(IPRINT) 1170,1170,1130                                         MINIM313          
1130  J=NEVAL/IPRINT                                                    MINIM314          
      K=NEVAL-J*IPRINT                                                  MINIM315          
      IF(K.NE.0) GOTO 1170                                              MINIM316          
      WRITE(61,380) NEVAL,H(I),(F(J),J=1,NOP)                           MINIM317          
1170  CONTINUE                                                          MINIM318          
      I=I+1                                                             MINIM319          
      GOTO 1071                                                         MINIM320          
1079  CONTINUE                                                          MINIM321          
      GOTO 1220                                                         MINIM322          
1190  I=1                                                               MINIM323          
1191  IF(I.GT.NOP) GOTO 1199                                            MINIM324          
      G(IMAX,I)=PSTAR(I)                                                MINIM325          
      I=I+1                                                             MINIM326          
      GOTO 1191                                                         MINIM327          
1199  CONTINUE                                                          MINIM328          
      H(IMAX)=HSTAR                                                     MINIM329          
C                                                                       MINIM330          
C   IF LOOP = NLOOP, BEGIN TESTS FOR CONVERGENCE                        MINIM331          
C   OTHERWISE, GO BACK TO BEGINNING OF BASIC LOOP                       MINIM332          
1220  IF(.NOT.(LOOP.EQ.NLOOP)) GOTO 400                                 MINIM333          
C                                                                       MINIM334          
C   TESTS FOR CONVERGENCE                                               MINIM335          
C   CALCULATE MEAN AND STANDARD DEVIATION OF FUNCTION VALUES            MINIM336          
C   OF CURRENT SIMPLEX                                                  MINIM337          
      HMEAN=0.0                                                         MINIM338          
      HSTD=HMEAN                                                        MINIM339          
      I=1                                                               MINIM340          
1241  IF(I.GT.NP1) GOTO 1249                                            MINIM341          
      HSTD=HSTD+H(I)*H(I)                                               MINIM342          
      HMEAN=HMEAN+H(I)                                                  MINIM343          
      I=I+1                                                             MINIM344          
      GOTO 1241                                                         MINIM345          
1249  CONTINUE                                                          MINIM346          
      HMEAN=HMEAN/FLOAT(NP1)                                            MINIM347          
      HSTD=(HSTD-FLOAT(NP1)*HMEAN*HMEAN)/FLOAT(NP1)                     MINIM348          
      HSTD=SQRT(HSTD)                                                   MINIM349          
C                                                                       MINIM350          
C   CALCULATE CENTROID OF CURRENT SIMPLEX, F, AND FUNCTION              MINIM351          
C   VALUE THERE,FUNC                                                    MINIM352          
      I=1                                                               MINIM353          
1301  IF(I.GT.NOP) GOTO 1309                                            MINIM354          
      F(I)=0.0                                                          MINIM355          
      J=1                                                               MINIM356          
1321  IF(J.GT.NP1) GOTO 1329                                            MINIM357          
      F(I)=F(I)+G(J,I)                                                  MINIM358          
      J=J+1                                                             MINIM359          
      GOTO 1321                                                         MINIM360          
1329  CONTINUE                                                          MINIM361          
      F(I)=F(I)/FLOAT(NP1)                                              MINIM362          
      I=I+1                                                             MINIM363          
      GOTO 1301                                                         MINIM364          
1309  CONTINUE                                                          MINIM365          
      CALL GIVEF(F,FUNC)                                                MINIM366          
      NEVAL=NEVAL+1                                                     MINIM367          
C                                                                       MINIM368          
C   IF NUMBER OF FUNCTION EVALUATIONS TO DATE HAS OVERRUN THE           MINIM369          
C   LIMIT SET (MAX) SET NLOOP = 0 AND RETURN                            MINIM370          
      IF(.NOT.(NEVAL.GT.MAX)) GOTO 1500                                 MINIM371          
      IF(IPRINT) 1480,1400,1400                                         MINIM372          
1400  WRITE(61,1410) MAX                                                MINIM373          
1410  FORMAT(8H NUMBER ,8HOF FUNCT,8HION EVAL,8HUATIONS ,8HEXCEEDS ,I4) MINIM374          
      WRITE(61,1430) HSTD                                               MINIM375          
1430  FORMAT(8H STANDAR,8HD ERROR ,8HOF FUNCT,8HION VALU,8HES OF LA,    MINIM376          
     .8HST SIMPL,3HEX ,E13.6)                                           MINIM377          
      WRITE(61,1450)(F(I),I=1,NOP)                                      MINIM378          
1450  FORMAT(8H  CENTRO,8HID OF LA,8HST SIMPL,4HEX  ,8E13.5,(/28X,      MINIM379          
     .8E13.5))                                                          MINIM380          
C    1/27X,4(1X,E13.6)) ALTERED, PALMER, DCR, 1.4.70                    MINIM381          
      WRITE(61,1470) FUNC                                               MINIM382          
1470  FORMAT(8H  FUNCTI,8HON VALUE,8H AT CENT,7HROID   ,E13.6)          MINIM383          
1480  NLOOP=0                                                           MINIM384          
      GOTO 3790                                                         MINIM385          
1500  IF(HSTD.LT.STOCRIT) GOTO 1540                                     MINIM386          
C                                                                       MINIM387          
C   IF THE STANDARD DEVIATION CALCULATED ABOVE IS NOT LESS THAN         MINIM388          
C   THE CRITERION (STOPCRIT), SET IFLAG AND LOOP TO ZERO AND            MINIM389          
C   BEGIN BASIC LOOP AGAIN                                              MINIM390          
      IFLAG=0                                                           MINIM391          
      LOOP=0                                                            MINIM392          
      GOTO 400                                                          MINIM393          
1540  IF(IPRINT) 1590,1550,1550                                         MINIM394          
1550  WRITE(61,1560)                                                    MINIM395          
1560  FORMAT(2H */8H  INITIA,8HL EVIDEN,8HCE OF CO,8HNVERGENC,1HE)      MINIM396          
      WRITE(61,1450)(F(I),I=1,NOP)                                      MINIM397          
      WRITE(61,1470) FUNC                                               MINIM398          
C                                                                       MINIM399          
C   IF THE STANDARD DEVIATION IS LESS THAN THE CRITERION TEST IFLAG     MINIM400          
C   IFLAG = 0 THERE WAS NO EVIDENCE OF CONVERGENCE ON LAST TEST         MINIM401          
C         = 1 THERE WAS EVIDENCE OF CONVERGENCE ON LAST TEST            MINIM402          
1590  IF(IFLAG.NE.0) GOTO 1640                                          MINIM403          
C                                                                       MINIM404          
C   IF IFLAG = 0, SET IFLAG = 1 AND SAVE MEAN OF FUNCTION VALUES OF     MINIM405          
C   CURRENT SIMPLEX. GO TO BEGINNING OF BASIC LOOP.                     MINIM406          
      IFLAG=1                                                           MINIM407          
      SAVMEAN=HMEAN                                                     MINIM408          
      LOOP=0                                                            MINIM409          
      GOTO 400                                                          MINIM410          
C                                                                       MINIM411          
C   IF IFLAG = 1, TEST IF CHANGE IN MEAN IS LESS THAN THE CRITERION     MINIM412          
C   (STOPCRIT) IF IT IS, PROCESS HAS CONVERGED                          MINIM413          
C   IF NOT, SET IFLAG = LOOP = 0, AND GO TO BEGINNING OF BASIC LOOP     MINIM414          
1640  TEST=SAVMEAN-HMEAN                                                MINIM415          
      IF(TEST.LT.STOCRIT) GOTO 1690                                     MINIM416          
      IFLAG=0                                                           MINIM417          
      LOOP=0                                                            MINIM418          
      GOTO 400                                                          MINIM419          
1690  IF(IPRINT) 1780,1700,1700                                         MINIM420          
1700  WRITE(61,1710) NEVAL                                              MINIM421          
1710  FORMAT(5(/),8H PROCESS,8H CONVERG,8HES ON MI,8HNIMUM AF,4HTER ,I4 MINIM422          
     .,8H FUNCTIO,8HN EVALUA,5HTIONS///)                                MINIM423          
      WRITE(61,1730)(F(I),I=1,NOP)                                      MINIM424          
1730  FORMAT(8H MINIMUM,6H AT   ,8(1X,E13.6)/14X,8(1X,E13.6)/14X,4(1X,  MINIM425          
     .E13.6))                                                           MINIM426          
      WRITE(61,1750) FUNC                                               MINIM427          
1750  FORMAT(//8H MINIMUM,8H FUNCTIO,8HN VALUE ,2H  ,E13.6)             MINIM428          
      WRITE(61,1770)                                                    MINIM429          
1770  FORMAT(///8H END  OF,8H  SEARCH/1X,15(1H*))                       MINIM430          
1780  CONTINUE                                                          MINIM431          
C                                                                       MINIM432          
C                                                                       MINIM433          
C***********************************************************************MINIM434          
C                                                                       MINIM435          
C                                                                       MINIM436          
C     IF THE USER DOES NOT REQUIRE THE FITTING OF A QUADRIC             MINIM437          
C     SURFACE IN THE REGION OF THE MINIMUM ( FOR ESTIMATION             MINIM438          
C     OF THE VARIANCE - COVARIANCE MATRIX ), THE REMAINDER OF           MINIM439          
C     THE PROGRAMME DECK ( INCLUDING AND AFTER THE CARD OF ***S )       MINIM440          
C     MAY BE DETACHED . AN END CARD MUST BE INCLUDED IN THE             MINIM441          
C     SHORTENED DECK .                                                  MINIM442          
      IF(IQUAD.NE.0) GOTO 1810                                          MINIM443          
      GOTO 3790                                                         MINIM444          
1810  IF(IPRINT) 1840,1820,1820                                         MINIM445          
1820  WRITE(61,1830)                                                    MINIM446          
1830  FORMAT(10(/),8H FITTING,8H OF QUAD,8HRATIC SU,8HRFACE IN,         MINIM447          
     .8H REGION ,8HOF MINIM,2HUM///)                                    MINIM448          
C                                                                       MINIM449          
C     THE FITTING OF THE QUADRATIC SURFACE FOLLOWS EXACTLY THE          MINIM450          
C     PROCEDURE OUTLINED BY NELDER AND MEAD , AND WHERE                 MINIM451          
C     POSSIBLE THE NOTATION IN THE COMMENT CARDS WILL CORRESPOND        MINIM452          
C     TO THAT OF NELDER AND MEAD                                        MINIM453          
1840  NEVAL=0                                                           MINIM454          
C                                                                       MINIM455          
C     FURTHER FUNCTION EVALUATIONS REQUIRED ARE COUNTED IN NEVAL        MINIM456          
C                                                                       MINIM457          
C     THE FINAL SIMPLEX IS EXPANDED TO OVERCOME ROUNDING ERRORS         MINIM458          
      I=1                                                               MINIM459          
1851  IF(I.GT.NP1) GOTO 1859                                            MINIM460          
1860  TEST=ABS(H(I)-FUNC)                                               MINIM461          
      IF(.NOT.(TEST.LT.SIMP)) GOTO 1940                                 MINIM462          
      J=1                                                               MINIM463          
1881  IF(J.GT.NOP) GOTO 1889                                            MINIM464          
      G(I,J)=(G(I,J)-F(J))+G(I,J)                                       MINIM465          
      PSTST(J)=G(I,J)                                                   MINIM466          
      J=J+1                                                             MINIM467          
      GOTO 1881                                                         MINIM468          
1889  CONTINUE                                                          MINIM469          
      CALL GIVEF(PSTST,H(I))                                            MINIM470          
      NEVAL=NEVAL+1                                                     MINIM471          
      GOTO 1860                                                         MINIM472          
1940  CONTINUE                                                          MINIM473          
      I=I+1                                                             MINIM474          
      GOTO 1851                                                         MINIM475          
1859  CONTINUE                                                          MINIM476          
      AO=H(1)                                                           MINIM477          
C                                                                       MINIM478          
C     THE FUNCTION VALUES Y0(I) ARE CALCULATED AND STORED IN AVAL       MINIM479          
      I=1                                                               MINIM480          
1961  IF(I.GT.NAP) GOTO 1969                                            MINIM481          
      I1=I+1                                                            MINIM482          
      J=1                                                               MINIM483          
1981  IF(J.GT.NOP) GOTO 1989                                            MINIM484          
      PSTAR(J)=(G(1,J)+G(I1,J))/2.0                                     MINIM485          
      J=J+1                                                             MINIM486          
      GOTO 1981                                                         MINIM487          
1989  CONTINUE                                                          MINIM488          
      CALL GIVEF(PSTAR,AVAL(I))                                         MINIM489          
      NEVAL=NEVAL+1                                                     MINIM490          
C                                                                       MINIM491          
C     THE MATRIX B(I,J) IS CALCULATED , AND THE LOWER DIAGONAL SECTION  MINIM492          
C     STORED IN THE VECTOR BMAT                                         MINIM493          
      I=I+1                                                             MINIM494          
      GOTO 1961                                                         MINIM495          
1969  CONTINUE                                                          MINIM496          
      I=1                                                               MINIM497          
2031  IF(I.GT.NAP) GOTO 2039                                            MINIM498          
      I1=I-1                                                            MINIM499          
      I2=I+1                                                            MINIM500          
      J=1                                                               MINIM501          
2061  IF(J.GT.I1) GOTO 2069                                             MINIM502          
      J1=J+1                                                            MINIM503          
      K=1                                                               MINIM504          
2081  IF(K.GT.NOP) GOTO 2089                                            MINIM505          
      PSTST(K)=(G(I2,K)+G(J1,K))/2.0                                    MINIM506          
      K=K+1                                                             MINIM507          
      GOTO 2081                                                         MINIM508          
2089  CONTINUE                                                          MINIM509          
      CALL GIVEF(PSTST,HSTST)                                           MINIM510          
      NEVAL=NEVAL+1                                                     MINIM511          
      L=I*(I-1)/2+J                                                     MINIM512          
      BMAT(L)=2.0*(HSTST+AO-AVAL(I)-AVAL(J))                            MINIM513          
      J=J+1                                                             MINIM514          
      GOTO 2061                                                         MINIM515          
2069  CONTINUE                                                          MINIM516          
      I=I+1                                                             MINIM517          
      GOTO 2031                                                         MINIM518          
2039  CONTINUE                                                          MINIM519          
      L=0                                                               MINIM520          
      I=1                                                               MINIM521          
2171  IF(I.GT.NAP) GOTO 2179                                            MINIM522          
      I1=I+1                                                            MINIM523          
      L=L+I                                                             MINIM524          
      BMAT(L)=2.0*(H(I1)+AO-2.0*AVAL(I))                                MINIM525          
C                                                                       MINIM526          
C     THE VECTOR A(I) IS CALCULATED , AND STORED IN AVAL                MINIM527          
      I=I+1                                                             MINIM528          
      GOTO 2171                                                         MINIM529          
2179  CONTINUE                                                          MINIM530          
      I=1                                                               MINIM531          
2221  IF(I.GT.NAP) GOTO 2229                                            MINIM532          
      I1=I+1                                                            MINIM533          
      AVAL(I)=2.0*AVAL(I)-(H(I1)+3.0*AO)/2.0                            MINIM534          
C                                                                       MINIM535          
C     THE MATRIX Q IS CALCULATED , AND STORED IN THE MATRIX G           MINIM536          
C     (IN FACT, CONSIDERING THE USUAL ORIENTATION OF ROWS AND COLUMNS,  MINIM537          
C     TRANS(Q) IS STORED IN G )                                         MINIM538          
      I=I+1                                                             MINIM539          
      GOTO 2221                                                         MINIM540          
2229  CONTINUE                                                          MINIM541          
      I=1                                                               MINIM542          
2251  IF(I.GT.NOP) GOTO 2259                                            MINIM543          
      PMIN(I)=G(1,I)                                                    MINIM544          
      I=I+1                                                             MINIM545          
      GOTO 2251                                                         MINIM546          
2259  CONTINUE                                                          MINIM547          
      I=1                                                               MINIM548          
2271  IF(I.GT.NAP) GOTO 2279                                            MINIM549          
      I1=I+1                                                            MINIM550          
      J=1                                                               MINIM551          
2291  IF(J.GT.NOP) GOTO 2299                                            MINIM552          
      G(I1,J)=G(I1,J)-G(1,J)                                            MINIM553          
      J=J+1                                                             MINIM554          
      GOTO 2291                                                         MINIM555          
2299  CONTINUE                                                          MINIM556          
      I=I+1                                                             MINIM557          
      GOTO 2271                                                         MINIM558          
2279  CONTINUE                                                          MINIM559          
      I=1                                                               MINIM560          
2321  IF(I.GT.NAP) GOTO 2329                                            MINIM561          
      I1=I+1                                                            MINIM562          
      J=1                                                               MINIM563          
2341  IF(J.GT.NOP) GOTO 2349                                            MINIM564          
      G(I,J)=G(I1,J)                                                    MINIM565          
C                                                                       MINIM566          
C     THE MATRIX B IS INVERTED , USING THE MODIFIED SQUARE ROOT METHOD  MINIM567          
C     ( SEE SAZONOV , GEODEZIYA I AEROFOTOSYEMKA , NO.6 , 1962 )        MINIM568          
      J=J+1                                                             MINIM569          
      GOTO 2341                                                         MINIM570          
2349  CONTINUE                                                          MINIM571          
      I=I+1                                                             MINIM572          
      GOTO 2321                                                         MINIM573          
2329  CONTINUE                                                          MINIM574          
      NP1=NAP                                                           MINIM575          
      NLESS1=NAP-1                                                      MINIM576          
      I1=1                                                              MINIM577          
      I=2                                                               MINIM578          
2401  IF(I.GT.NP1) GOTO 2409                                            MINIM579          
      ILESS1=I-1                                                        MINIM580          
      I3=1                                                              MINIM581          
      J=2                                                               MINIM582          
2431  IF(J.GT.ILESS1) GOTO 2439                                         MINIM583          
      I2=I1+J                                                           MINIM584          
      JLESS1=J-1                                                        MINIM585          
      J0=1                                                              MINIM586          
2461  IF(J0.GT.JLESS1) GOTO 2469                                        MINIM587          
      I4=I3+J0                                                          MINIM588          
      I5=I1+J0                                                          MINIM589          
      BMAT(I2)=BMAT(I2)-BMAT(I4)*BMAT(I5)                               MINIM590          
      J0=J0+1                                                           MINIM591          
      GOTO 2461                                                         MINIM592          
2469  CONTINUE                                                          MINIM593          
      I3=I3+J                                                           MINIM594          
      J=J+1                                                             MINIM595          
      GOTO 2431                                                         MINIM596          
2439  CONTINUE                                                          MINIM597          
      I3=0                                                              MINIM598          
      I5=I1+I                                                           MINIM599          
      K=1                                                               MINIM600          
2531  IF(K.GT.ILESS1) GOTO 2539                                         MINIM601          
      I2=I1+K                                                           MINIM602          
      I3=I3+K                                                           MINIM603          
      TEMP=BMAT(I2)/BMAT(I3)                                            MINIM604          
      BMAT(I5)=BMAT(I5)-BMAT(I2)*TEMP                                   MINIM605          
      BMAT(I2)=TEMP                                                     MINIM606          
C                                                                       MINIM607          
C     IF THE MATRIX B IS NOT POSITIVE DEFINITE , SET NLOOP=-1 AND RETURNMINIM608          
      K=K+1                                                             MINIM609          
      GOTO 2531                                                         MINIM610          
2539  CONTINUE                                                          MINIM611          
      IF(BMAT(I5)) 2600,2600,2640                                       MINIM612          
2600  WRITE(61,2610)                                                    MINIM613          
2610  FORMAT(8H MATRIX ,8HTO BE IN,8HVERTED N,8HOT POSIT,8HIVE DEFI,    MINIM614          
     .4HNITE)                                                           MINIM615          
      NLOOP=-1                                                          MINIM616          
      GOTO 3790                                                         MINIM617          
2640  I1=I1+I                                                           MINIM618          
      I=I+1                                                             MINIM619          
      GOTO 2401                                                         MINIM620          
2409  CONTINUE                                                          MINIM621          
      I1=1                                                              MINIM622          
      I=2                                                               MINIM623          
2661  IF(I.GT.NP1) GOTO 2669                                            MINIM624          
      ILESS1=I-1                                                        MINIM625          
      J=1                                                               MINIM626          
2681  IF(J.GT.ILESS1) GOTO 2689                                         MINIM627          
      I2=I1+J                                                           MINIM628          
      JPLUS1=J+1                                                        MINIM629          
      K=JPLUS1                                                          MINIM630          
2711  IF(K.GT.ILESS1) GOTO 2719                                         MINIM631          
      I3=J+K*(K-1)/2                                                    MINIM632          
      I4=I1+K                                                           MINIM633          
      BMAT(I2)=BMAT(I2)+BMAT(I3)*BMAT(I4)                               MINIM634          
      K=K+1                                                             MINIM635          
      GOTO 2711                                                         MINIM636          
2719  CONTINUE                                                          MINIM637          
      BMAT(I2)=-BMAT(I2)                                                MINIM638          
      J=J+1                                                             MINIM639          
      GOTO 2681                                                         MINIM640          
2689  CONTINUE                                                          MINIM641          
      I1=I1+I                                                           MINIM642          
      I=I+1                                                             MINIM643          
      GOTO 2661                                                         MINIM644          
2669  CONTINUE                                                          MINIM645          
      I1=0                                                              MINIM646          
      I=1                                                               MINIM647          
2781  IF(I.GT.NP1) GOTO 2789                                            MINIM648          
      I1=I1+I                                                           MINIM649          
      BMAT(I1)=1.0/BMAT(I1)                                             MINIM650          
      I=I+1                                                             MINIM651          
      GOTO 2781                                                         MINIM652          
2789  CONTINUE                                                          MINIM653          
      I=1                                                               MINIM654          
2811  IF(I.GT.NLESS1) GOTO 2819                                         MINIM655          
      IPLUS1=I+1                                                        MINIM656          
      K=IPLUS1                                                          MINIM657          
2831  IF(K.GT.NP1) GOTO 2839                                            MINIM658          
      I1=K*(K-1)/2                                                      MINIM659          
      I2=I1+K                                                           MINIM660          
      I3=I1+I                                                           MINIM661          
      TEMP=BMAT(I2)*BMAT(I3)                                            MINIM662          
      KLESSI=K-I                                                        MINIM663          
      J=1                                                               MINIM664          
2891  IF(J.GT.KLESSI) GOTO 2899                                         MINIM665          
      J0=I+J-1                                                          MINIM666          
      I4=J0*(J0-1)/2+I                                                  MINIM667          
      I5=I1+J0                                                          MINIM668          
      BMAT(I4)=BMAT(I4)+BMAT(I5)*TEMP                                   MINIM669          
      J=J+1                                                             MINIM670          
      GOTO 2891                                                         MINIM671          
2899  CONTINUE                                                          MINIM672          
      BMAT(I3)=TEMP                                                     MINIM673          
C                                                                       MINIM674          
C     (B**-1)*A IS CALCULATED , AND STORED IN H                         MINIM675          
      K=K+1                                                             MINIM676          
      GOTO 2831                                                         MINIM677          
2839  CONTINUE                                                          MINIM678          
      I=I+1                                                             MINIM679          
      GOTO 2811                                                         MINIM680          
2819  CONTINUE                                                          MINIM681          
      I=1                                                               MINIM682          
2951  IF(I.GT.NAP) GOTO 2959                                            MINIM683          
      H(I)=0.0                                                          MINIM684          
      J=1                                                               MINIM685          
2971  IF(J.GT.NAP) GOTO 2979                                            MINIM686          
      IF(J.GT.I) GOTO 3010                                              MINIM687          
      IJ=I*(I-1)/2+J                                                    MINIM688          
      GOTO 3020                                                         MINIM689          
3010  IJ=J*(J-1)/2+I                                                    MINIM690          
3020  H(I)=H(I)+BMAT(IJ)*AVAL(J)                                        MINIM691          
      J=J+1                                                             MINIM692          
      GOTO 2971                                                         MINIM693          
2979  CONTINUE                                                          MINIM694          
C                                                                       MINIM695          
C     YMIN ( THE ESTIMATED MINIMUM VALUE ) AND PMIN ( ITS POSITION )    MINIM696          
C     ARE CALCULATED                                                    MINIM697          
      I=I+1                                                             MINIM698          
      GOTO 2951                                                         MINIM699          
2959  CONTINUE                                                          MINIM700          
      YMIN=0                                                            MINIM701          
      I=1                                                               MINIM702          
3061  IF(I.GT.NAP) GOTO 3069                                            MINIM703          
      YMIN=YMIN+H(I)*AVAL(I)                                            MINIM704          
      I=I+1                                                             MINIM705          
      GOTO 3061                                                         MINIM706          
3069  CONTINUE                                                          MINIM707          
      YMIN=AO-YMIN                                                      MINIM708          
      I=1                                                               MINIM709          
3091  IF(I.GT.NOP) GOTO 3099                                            MINIM710          
      PSTST(I)=0                                                        MINIM711          
      J=1                                                               MINIM712          
3111  IF(J.GT.NAP) GOTO 3119                                            MINIM713          
      PSTST(I)=PSTST(I)+H(J)*G(J,I)                                     MINIM714          
      J=J+1                                                             MINIM715          
      GOTO 3111                                                         MINIM716          
3119  CONTINUE                                                          MINIM717          
      I=I+1                                                             MINIM718          
      GOTO 3091                                                         MINIM719          
3099  CONTINUE                                                          MINIM720          
      I=1                                                               MINIM721          
3131  IF(I.GT.NOP) GOTO 3139                                            MINIM722          
      PMIN(I)=PMIN(I)-PSTST(I)                                          MINIM723          
      I=I+1                                                             MINIM724          
      GOTO 3131                                                         MINIM725          
3139  CONTINUE                                                          MINIM726          
      IF(IPRINT) 3220,3160,3160                                         MINIM727          
3160  WRITE(61,3170) YMIN,(PMIN(I),I=1,NOP)                             MINIM728          
3170  FORMAT(8H MINIMUM,8H OF FITT,8HED QUADR,8HATIC SUR,8HFACE IS ,    MINIM729          
     .2H  ,E13.6,4H  AT/8(1X,E13.6)/8(1X,E13.6)/4(1X,E13.6)//)          MINIM730          
      WRITE(61,3190) FUNC,(F(I),I=1,NOP)                                MINIM731          
3190  FORMAT(/8H COMPARE,8H WITH MI,8HNIMUM FO,8HUND BY I,8HTERATION,   MINIM732          
     .2H  ,E13.6,4H  AT/8(1X,E13.6)/8(1X,E13.6)/4(1X,E13.6)//)          MINIM733          
      WRITE(61,3210)                                                    MINIM734          
3210  FORMAT(/8H IF DIFF,8HERENCE I,8HS LARGE ,8H, INFORM,8HATION MA,   MINIM735          
     .8HTRIX IS ,8HINACCURA,2HTE///)                                    MINIM736          
C                                                                       MINIM737          
C     Q*(B**-1)*TRANS(Q) IS CALCULATED , AND ITS LOWER DIAGONAL         MINIM738          
C     SECTION STORED IN THE VECTOR VC                                   MINIM739          
3220  I=1                                                               MINIM740          
3221  IF(I.GT.NOP) GOTO 3229                                            MINIM741          
      J=1                                                               MINIM742          
3231  IF(J.GT.NAP) GOTO 3239                                            MINIM743          
      H(J)=0.0                                                          MINIM744          
      K=1                                                               MINIM745          
3251  IF(K.GT.NAP) GOTO 3259                                            MINIM746          
      IF(K.GT.J) GOTO 3290                                              MINIM747          
      JK=J*(J-1)/2+K                                                    MINIM748          
      GOTO 3300                                                         MINIM749          
3290  JK=K*(K-1)/2+J                                                    MINIM750          
3300  H(J)=H(J)+BMAT(JK)*G(K,I)                                         MINIM751          
      K=K+1                                                             MINIM752          
      GOTO 3251                                                         MINIM753          
3259  CONTINUE                                                          MINIM754          
      J=J+1                                                             MINIM755          
      GOTO 3231                                                         MINIM756          
3239  CONTINUE                                                          MINIM757          
      J=I                                                               MINIM758          
3331  IF(J.GT.NOP) GOTO 3339                                            MINIM759          
      IJ=I*(I-1)/2+J                                                    MINIM760          
      VC(IJ)=0.0                                                        MINIM761          
      K=1                                                               MINIM762          
3361  IF(K.GT.NAP) GOTO 3369                                            MINIM763          
      VC(IJ)=VC(IJ)+H(K)*G(K,J)                                         MINIM764          
      K=K+1                                                             MINIM765          
      GOTO 3361                                                         MINIM766          
3369  CONTINUE                                                          MINIM767          
      J=J+1                                                             MINIM768          
      GOTO 3331                                                         MINIM769          
3339  CONTINUE                                                          MINIM770          
C                                                                       MINIM771          
C     THE DIAGONAL ELEMENTS OF VC ARE STORED IN VAR FOR RETURN TO THE   MINIM772          
C     CALLING PROGRAMME                                                 MINIM773          
      I=I+1                                                             MINIM774          
      GOTO 3221                                                         MINIM775          
3229  CONTINUE                                                          MINIM776          
      I=1                                                               MINIM777          
3401  IF(I.GT.NOP) GOTO 3409                                            MINIM778          
      J=I*(I+1)/2                                                       MINIM779          
      VAR(I)=VC(J)                                                      MINIM780          
      I=I+1                                                             MINIM781          
      GOTO 3401                                                         MINIM782          
3409  CONTINUE                                                          MINIM783          
      IF(IPRINT) 3780,3440,3440                                         MINIM784          
3440  WRITE(61,3450)                                                    MINIM785          
3450  FORMAT(8H INVERSE,8H OF INFO,8HRMATION ,6HMATRIX/)                MINIM786          
      I=1                                                               MINIM787          
3461  IF(I.GT.NOP) GOTO 3469                                            MINIM788          
      NU=I*(I+1)/2                                                      MINIM789          
      NL=I*(I-1)/2+1                                                    MINIM790          
      WRITE(61,3500)(VC(J),J=NL,NU)                                     MINIM791          
3500  FORMAT(8(1X,E13.6)/4X,8(1X,E13.6)/8X,4(1X,E13.6))                 MINIM792          
      I=I+1                                                             MINIM793          
      GOTO 3461                                                         MINIM794          
3469  CONTINUE                                                          MINIM795          
      WRITE(61,3530)                                                    MINIM796          
3530  FORMAT(///8H IF THE ,8HFUNCTION,8H MINIMIS,8HED WAS -,8HLOG(LIKE, MINIM797          
     .8HLIHOOD) ,8H, THIS M,8HATRIX IS,8H THE VAR,8HIANCE-CO,8HVARIANCE MINIM798          
     .,8H MATRIX ,8HOF THE P,8HARAMETER,1HS/)                           MINIM799          
      WRITE(61,3550)                                                    MINIM800          
3550  FORMAT(8H IF THE ,8HFUNCTION,8H MINIMIS,8HED WAS T,8HHE SUM O,    MINIM801          
     .8HF SQUARE,8HS OF RES,8HIDUALS ,,8H THIS MA,8HTRIX MUS,8HT BE MUL MINIM802          
     .,8HTIPLIED ,8HBY TWICE,8H THE EST,8HIMATE OF,8H THE RES,6HIDUAL , MINIM803          
     ./8H VARIANC,8HE TO GIV,8HE THE VA,8HRIANCE C,8HOVARIANC,          MINIM804          
     .8HE MATRIX,8H OF THE ,8HPARAMETE,2HRS///)                         MINIM805          
      I=1                                                               MINIM806          
3561  IF(I.GT.NOP) GOTO 3569                                            MINIM807          
      I1=I-1                                                            MINIM808          
      J=1                                                               MINIM809          
3581  IF(J.GT.I1) GOTO 3589                                             MINIM810          
      IJ=I*(I-1)/2+J                                                    MINIM811          
      II=J*(J+1)/2                                                      MINIM812          
      JJ=I*(I+1)/2                                                      MINIM813          
      DIV=SQRT(VC(II)*VC(JJ))                                           MINIM814          
      VC(IJ)=VC(IJ)/DIV                                                 MINIM815          
      J=J+1                                                             MINIM816          
      GOTO 3581                                                         MINIM817          
3589  CONTINUE                                                          MINIM818          
      I=I+1                                                             MINIM819          
      GOTO 3561                                                         MINIM820          
3569  CONTINUE                                                          MINIM821          
      WRITE(61,3660)                                                    MINIM822          
3660  FORMAT(///8H CORRELA,8HTION MAT,3HRIX/)                           MINIM823          
      I=1                                                               MINIM824          
3671  IF(I.GT.NOP) GOTO 3679                                            MINIM825          
      II=I*(I+1)/2                                                      MINIM826          
      VC(II)=1.0                                                        MINIM827          
      NU=II                                                             MINIM828          
      NL=II-I+1                                                         MINIM829          
      WRITE(61,3500)(VC(J),J=NL,NU)                                     MINIM830          
      I=I+1                                                             MINIM831          
      GOTO 3671                                                         MINIM832          
3679  CONTINUE                                                          MINIM833          
      WRITE(61,3750) NEVAL                                              MINIM834          
3750  FORMAT(8H A FURTH,3HER ,I4,8H FUNCTIO,8HN EVALUA,8HTIONS HA,      MINIM835          
     .8HVE BEEN ,4HUSED///)                                             MINIM836          
      WRITE(61,3770)                                                    MINIM837          
3770  FORMAT(8H END  OF,8H  QUADRA,8HTIC  SUR,8HFACE  FI,5HTTING/1X,36( MINIM838          
     .1H*))                                                             MINIM839          
3780  CONTINUE                                                          MINIM840          
3790  RETURN                                                            MINIM841          
      END                                                               MINIM842          
