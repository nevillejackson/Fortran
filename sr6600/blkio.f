      SUBROUTINE OPENWS(B,LU,LF,KB,KI)                                  BLKIO002          
      DIMENSION B(10),IP(1)                                             BLKIO003          
      EQUIVALENCE(PI,IP)                                                BLKIO004          
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC  BLKIO005          
C                                                                       BLKIO006          
C     BLKIO WAS INITIALLY RELEASED IN FEBRUARY 1966 BY E.H.KINNEY.      BLKIO007          
C     THIS VERSION WAS RELEASED IN JANUARY 1969. IT IS FULLY 3200/3600  BLKIO008          
C     COMPATIBLE. THE ORIGINAL WRITE-UP STILL APPLIES TO THIS VERSION.  BLKIO009          
C     (USER GROUP IDENTIFICATION - I5)                                  BLKIO010          
C     MAGNETIC TAPES CREATED ON THE 3600 BY THE EARLIER VERSION OF BLKIOBLKIO011          
C     ARE STILL ACCEPTABLE AS INPUT TO THIS VERSION                     BLKIO012          
C                                                                       BLKIO013          
C     HOWEVER MAGNETIC TAPES CREATED ON THE 3200 BY THE EARLIER VERSION BLKIO014          
C     OF BLKIO ARE NOT ACCEPTABLE TO THIS VERSION. SUCH TAPES MAY BE    BLKIO015          
C     SENT TO B. SAVVAS DCR ADELAIDE TO BE CONVERTED.                   BLKIO016          
C                                                                       BLKIO017          
C                                 R.RATTLEY , B. SAVVAS                 BLKIO018          
C                                    DCR ADELAIDE                       BLKIO019          
C                                            JANUARY  1969              BLKIO020          
C                                                                       BLKIO021          
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC  BLKIO022          
      K=0                                                               BLKIO023          
      GOTO 60                                                           BLKIO024          
      ENTRY OPENWD                                                      BLKIO025          
      K=9                                                               BLKIO026          
60    I26=1                                                             BLKIO027          
      PI=0.                                                             BLKIO028          
      B(1)=PI                                                           BLKIO029          
C                            B(1) = ZERO                                BLKIO030          
      IP(I26)=KI                                                        BLKIO031          
      B(2)=PI                                                           BLKIO032          
C                            B(2) = ITEM SIZE KI                        BLKIO033          
      IP(I26)=KB                                                        BLKIO034          
      B(3)=PI                                                           BLKIO035          
C                            B(3) = BLOCK SIZE KB                       BLKIO036          
      JLF=LF                                                            BLKIO037          
      IP(I26)=JLF                                                       BLKIO038          
      B(4)=PI                                                           BLKIO039          
C                            B(4) = LOGICAL FILE NUMBER LF              BLKIO040          
      JLU=LU                                                            BLKIO041          
      IP(I26)=JLU                                                       BLKIO042          
      B(5)=PI                                                           BLKIO043          
C                            B(5) = LOGICAL UNIT NUMBER LU              BLKIO044          
      IP(I26)=8                                                         BLKIO045          
      B(8)=PI                                                           BLKIO046          
      B(6)=B(8)                                                         BLKIO047          
C                            B(6) = CURRENT ITEM INDEX                  BLKIO048          
C                            B(8) = LAST-WORD INDEX OF TRANSMISSION     BLKIO049          
      IP(I26)=K                                                         BLKIO050          
      B(7)=PI                                                           BLKIO051          
C                            B(7) = CURRENT-BUFFER INDEX                BLKIO052          
C                            ZERO INDICATES SINGLE BUFFERING            BLKIO053          
C                                                                       BLKIO054          
C**** WRITE IDENTIFICATION BLOCK, HEADING IT WITH E.O.F. UNLESS LF=0    BLKIO055          
C                                                                       BLKIO056          
      IF(JLF.EQ.0) GOTO 220                                             BLKIO057          
      ENDFILE JLU                                                       BLKIO058          
220   BUFFEROUT(JLU,1)(B(1),B(8))                                       BLKIO059          
      RETURN                                                            BLKIO060          
      END                                                               BLKIO061          
      SUBROUTINE OUTPUT(B,A)                                            BLKIO062          
      DIMENSION B(10),A(1),IP(1)                                        BLKIO063          
      EQUIVALENCE(PI,IP)                                                BLKIO064          
      I26=1                                                             BLKIO065          
      PI=B(2)                                                           BLKIO066          
      KIGIVEN=IP(I26)                                                   BLKIO067          
      KI=KIGIVEN                                                        BLKIO068          
C                            KI = ITEM SIZE                             BLKIO069          
      PI=B(3)                                                           BLKIO070          
      KB=IP(I26)                                                        BLKIO071          
C                            KB = BLOCK SIZE                            BLKIO072          
C                                                                       BLKIO073          
C**** IF ITEM SIZE IS GIVEN AS ZERO, GET ITEM SIZE FROM ITEM  -  LEFT 24BLKIO074          
C         BITS OF A(1)  -  AND CHANGE A(1) TEMPORARILY (FOR OUTPUTTING) BLKIO075          
C         TO SHOW THAT ITEM SIZE IS FROM A                              BLKIO076          
C                                                                       BLKIO077          
      IF(.NOT.(KI.EQ.0)) GOTO 120                                       BLKIO078          
      PI=A(1)                                                           BLKIO079          
      A1=PI                                                             BLKIO080          
      KI=IP(1)                                                          BLKIO081          
      IP(1)=IP(1)*8                                                     BLKIO082          
      A(1)=PI                                                           BLKIO083          
C**** IF ITEM IS LARGER THAN BLOCK, TRUNCATE                            BLKIO084          
120   IF(.NOT.(KI.GT.KB)) GOTO 140                                      BLKIO085          
      KI=KB                                                             BLKIO086          
140   PI=B(6)                                                           BLKIO087          
      ITEMIND=IP(I26)                                                   BLKIO088          
C                            ITEMIND = ITEM INDEX                       BLKIO089          
      PI=B(7)                                                           BLKIO090          
      IBUFF=IP(I26)                                                     BLKIO091          
C                            IBUFF = CURRENT BUFFER INDEX               BLKIO092          
C                                                                       BLKIO093          
C**** SET M=1 FOR DOUBLE BUFFERING                                      BLKIO094          
C         M=2 FOR SINGLE BUFFERING                                      BLKIO095          
C                                                                       BLKIO096          
      M=1                                                               BLKIO097          
      IF(.NOT.(IBUFF.EQ.0)) GOTO 220                                    BLKIO098          
      IBUFF=9                                                           BLKIO099          
      M=2                                                               BLKIO100          
C                                                                       BLKIO101          
C**** COMPARE ITEM SIZE WITH AVAILABLE BUFFER SPACE                     BLKIO102          
C                                                                       BLKIO103          
220   IF(.NOT.(KI.LT.KB+IBUFF-ITEMIND)) GOTO 300                        BLKIO104          
C                                                                       BLKIO105          
C**** PUT ITEM IN BUFFER                                                BLKIO106          
C                                                                       BLKIO107          
      N=1                                                               BLKIO108          
231   IF(N.GT.KI) GOTO 239                                              BLKIO109          
      ITEMIND=ITEMIND+1                                                 BLKIO110          
      B(ITEMIND)=A(N)                                                   BLKIO111          
C                                                                       BLKIO112          
C**** ADVANCE ITEM INDEX                                                BLKIO113          
C                                                                       BLKIO114          
      N=N+1                                                             BLKIO115          
      GOTO 231                                                          BLKIO116          
239   CONTINUE                                                          BLKIO117          
      IP(I26)=ITEMIND                                                   BLKIO118          
      B(6)=PI                                                           BLKIO119          
C                                                                       BLKIO120          
C**** RESET A(1) (IN CASE ITEM SIZE WAS ZERO). RETURN                   BLKIO121          
C                                                                       BLKIO122          
      A(1)=A1                                                           BLKIO123          
      GOTO 380                                                          BLKIO124          
C                                                                       BLKIO125          
C**** EMPTY THE BUFFER                                                  BLKIO126          
C                                                                       BLKIO127          
300   KQZ001=M                                                          BLKIO128          
      IF(KQZ001.LT.1) KQZ001=1                                          BLKIO129          
      IF(KQZ001.GT.2) KQZ001=2                                          BLKIO130          
      GOTO(330,310),KQZ001                                              BLKIO131          
C                                                                       BLKIO132          
C**** IF SINGLE BUFFER, AND ONLY THE ID-BLOCK HAS BEEN WRITTEN SO FAR,  BLKIO133          
C         THEN MUST CHECK...                                            BLKIO134          
C                                                                       BLKIO135          
310   PI=B(8)                                                           BLKIO136          
      IF(.NOT.(IP(I26).EQ.8)) GOTO 340                                  BLKIO137          
C                                                                       BLKIO138          
C**** OTHERWISE CHECK THEN BLOCK IF DOUBLE BUFFERING,                   BLKIO139          
C               BLOCK THEN CHECK IF SINGLE BUFFERING                    BLKIO140          
C                                                                       BLKIO141          
330   CALL CHECKO(B)                                                    BLKIO142          
340   CALL BLOCKO(B)                                                    BLKIO143          
      KQZ001=M                                                          BLKIO144          
      IF(KQZ001.LT.1) KQZ001=1                                          BLKIO145          
      IF(KQZ001.GT.2) KQZ001=2                                          BLKIO146          
      GOTO(140,360),KQZ001                                              BLKIO147          
360   CALL CHECKO(B)                                                    BLKIO148          
      GOTO 140                                                          BLKIO149          
380   RETURN                                                            BLKIO150          
      END                                                               BLKIO151          
      SUBROUTINE CLOSE(B)                                               BLKIO152          
      DIMENSION B(10),IP(1)                                             BLKIO153          
      EQUIVALENCE(PI,IP)                                                BLKIO154          
      I26=1                                                             BLKIO155          
      PI=B(7)                                                           BLKIO156          
      IBUFF=IP(I26)                                                     BLKIO157          
C                            IBUFF = CURRENT-BUFFER INDEX               BLKIO158          
C                                                                       BLKIO159          
C**** IF SINGLE BUFFERING, AND ONLY THE ID-BLOCK HAS BEEN WRITTEN, OR IFBLKIO160          
C         DOUBLE BUFFERING, CHECK OUTPUT                                BLKIO161          
C                                                                       BLKIO162          
      IF(.NOT.(IBUFF.EQ.0)) GOTO 90                                     BLKIO163          
      IBUFF=9                                                           BLKIO164          
      PI=B(8)                                                           BLKIO165          
      IF(.NOT.(IP(I26).EQ.8)) GOTO 100                                  BLKIO166          
90    CALL CHECKO(B)                                                    BLKIO167          
C                                                                       BLKIO168          
C**** UNLESS CURRENT ITEM IS BEFORE CURRENT BUFFER,                     BLKIO169          
C                                                                       BLKIO170          
100   PI=B(6)                                                           BLKIO171          
      IF(IP(I26).LT.IBUFF) GOTO 140                                     BLKIO172          
C                                                                       BLKIO173          
C**** BLOCK, AND CHECK AGAIN                                            BLKIO174          
C                                                                       BLKIO175          
      CALL BLOCKO(B)                                                    BLKIO176          
      CALL CHECKO(B)                                                    BLKIO177          
C                                                                       BLKIO178          
C**** WRITE DOUBLE E.O.F., BACKSPACE OVER THEM                          BLKIO179          
C                                                                       BLKIO180          
140   PI=B(5)                                                           BLKIO181          
      LU=IP(I26)                                                        BLKIO182          
      ENDFILE LU                                                        BLKIO183          
      ENDFILE LU                                                        BLKIO184          
      BACKSPACE LU                                                      BLKIO185          
      BACKSPACE LU                                                      BLKIO186          
      RETURN                                                            BLKIO187          
      END                                                               BLKIO188          
      SUBROUTINE BLOCKO(B)                                              BLKIO189          
      DIMENSION B(10),IP(1)                                             BLKIO190          
      EQUIVALENCE(PI,IP)                                                BLKIO191          
      I26=1                                                             BLKIO192          
C                                                                       BLKIO193          
C**** GET NEW BUFFER INDEX                                              BLKIO194          
C         FULL BUFFER INDEX                                             BLKIO195          
C         CURRENT ITEM INDEX (OTHER END OF FULL BUFFER)                 BLKIO196          
C         NEW ITEM INDEX                                                BLKIO197          
C         NEW ITEM INDEX (ONE WORD BEFORE BEGINNING OF NEW BUFFER)      BLKIO198          
C                                                                       BLKIO199          
      PI=B(7)                                                           BLKIO200          
C         (CURRENT BUFFER)                                              BLKIO201          
      IBUFNEW=IP(I26)                                                   BLKIO202          
      IBUFULL=9                                                         BLKIO203          
      IF(.NOT.(IBUFNEW.EQ.9)) GOTO 100                                  BLKIO204          
      PI=B(3)                                                           BLKIO205          
      IBUFULL=IP(I26)+9                                                 BLKIO206          
C         (BUFFER SIZE + 9)                                             BLKIO207          
      GOTO 120                                                          BLKIO208          
100   IF(.NOT.(IBUFNEW.EQ.0)) GOTO 120                                  BLKIO209          
      IBUFNEW=9                                                         BLKIO210          
C         (SINGLE BUFFERING CASE)                                       BLKIO211          
120   PI=B(6)                                                           BLKIO212          
      B(8)=PI                                                           BLKIO213          
C         (CURRENT ITEM INDEX BECOMES LAST WORD TRANSMITTED)            BLKIO214          
      KURITIN=IP(I26)                                                   BLKIO215          
      IP(I26)=IBUFNEW-1                                                 BLKIO216          
      B(6)=PI                                                           BLKIO217          
C         (NEW ITEM INDEX)                                              BLKIO218          
      PI=B(5)                                                           BLKIO219          
      LU=IP(I26)                                                        BLKIO220          
      BUFFEROUT(LU,1)(B(IBUFULL),B(KURITIN))                            BLKIO221          
      RETURN                                                            BLKIO222          
      END                                                               BLKIO223          
      SUBROUTINE CHECKO(B)                                              BLKIO224          
      INTEGER UNITSTF                                                   BLKIO225          
      DIMENSION B(10),IP(1)                                             BLKIO226          
      EQUIVALENCE(PI,IP)                                                BLKIO227          
      I26=1                                                             BLKIO228          
      PI=B(5)                                                           BLKIO229          
      LU=IP(I26)                                                        BLKIO230          
      PI=B(3)                                                           BLKIO231          
      KB=IP(I26)                                                        BLKIO232          
C                                                                       BLKIO233          
C**** CHECK STATUS OF PREVIOUS WRITE, AND INITIALISE NEW BUFFER         BLKIO234          
C                                                                       BLKIO235          
70    L=UNITSTF(LU)                                                     BLKIO236          
      GOTO(70,90,90,90),L                                               BLKIO237          
90    PI=B(7)                                                           BLKIO238          
C         (CURRENT-BUFFER INDEX)                                        BLKIO239          
      IF(IP(I26).EQ.0) GOTO 300                                         BLKIO240          
C         (THAT IS ALL, FOR SINGLE BUFFERING)                           BLKIO241          
C                                                                       BLKIO242          
C**** SWITCH BUFFERS                                                    BLKIO243          
C                                                                       BLKIO244          
      KURUFIN=IP(I26)                                                   BLKIO245          
      IP(1)=0                                                           BLKIO246          
      IP(I26)=9                                                         BLKIO247          
      IF(.NOT.(KURUFIN.EQ.9)) GOTO 160                                  BLKIO248          
      IP(I26)=KB+9                                                      BLKIO249          
160   CONTINUE                                                          BLKIO250          
      B(7)=PI                                                           BLKIO251          
300   RETURN                                                            BLKIO252          
      END                                                               BLKIO253          
      SUBROUTINE OPENRS(B,LU,LF,LS)                                     BLKIO254          
      DIMENSION B(10),IP(1)                                             BLKIO255          
      EQUIVALENCE(PI,IP)                                                BLKIO256          
      K=0                                                               BLKIO257          
      GOTO 60                                                           BLKIO258          
      ENTRY OPENRD                                                      BLKIO259          
      K=9                                                               BLKIO260          
60    I26=1                                                             BLKIO261          
      IP(1)=0                                                           BLKIO262          
      IP(I26)=K                                                         BLKIO263          
      B(7)=PI                                                           BLKIO264          
C                            B(7) = CURRENT-BUFFER INDEX                BLKIO265          
C                            ZERO INDICATES SINGLE BUFFER               BLKIO266          
      IP(I26)=4                                                         BLKIO267          
      B(8)=PI                                                           BLKIO268          
C                            B(8) = LAST-WORD INDEX FOR TRANSMISSION    BLKIO269          
      JLU=LU                                                            BLKIO270          
      IP(I26)=JLU                                                       BLKIO271          
      B(5)=PI                                                           BLKIO272          
C                            B(5) = LOGICAL UNIT NUMBER                 BLKIO273          
      JLF=LF                                                            BLKIO274          
C                                                                       BLKIO275          
C**** START SEARCH FOR LOGICAL FILE NUMBERED LF                         BLKIO276          
C                                                                       BLKIO277          
      M=1                                                               BLKIO278          
C         (FOR FIRST PASS)                                              BLKIO279          
      IF(JLF.EQ.0) GOTO 180                                             BLKIO280          
C         (NO E.O.F. HEADER IF LF=0)                                    BLKIO281          
170   CALL SKIP(JLU)                                                    BLKIO282          
180   BUFFERIN(JLU,1)(B(1),B(4))                                        BLKIO283          
      CALL CHECKI(B)                                                    BLKIO284          
      PI=B(6)                                                           BLKIO285          
C         (SET BY CHECKI)                                               BLKIO286          
      IF(IP(I26).EQ.2) GOTO 300                                         BLKIO287          
C         (B(6)=2 MEANS PHYSICAL E.O.F.)                                BLKIO288          
C                                                                       BLKIO289          
C**** COMPARE LOGICAL FILE NUMBERS                                      BLKIO290          
C                                                                       BLKIO291          
      PI=B(4)                                                           BLKIO292          
      IF(IP(I26)-JLF) 170,250,240                                       BLKIO293          
C         (KEEP LOOKING/FOUND/TRY SECOND PASS)                          BLKIO294          
240   KQZ001=M                                                          BLKIO295          
      IF(KQZ001.LT.1) KQZ001=1                                          BLKIO296          
      IF(KQZ001.GT.2) KQZ001=2                                          BLKIO297          
      GOTO(310,170),KQZ001                                              BLKIO298          
250   CALL BLOCKI(B)                                                    BLKIO299          
      IP(I26)=1                                                         BLKIO300          
      B(6)=PI                                                           BLKIO301          
C         (CURRENT ITEM IS NUMBER 1)                                    BLKIO302          
C  12 LS=1                                                              BLKIO303          
      LS=1                                                              BLKIO304          
C         (NORMAL RETURN)                                               BLKIO305          
      GOTO 380                                                          BLKIO306          
C                                                                       BLKIO307          
C**** PHYSICAL E.O.F. - ENTER SECOND PASS, OR, IF ALREADY IN SECOND     BLKIO308          
C         PASS, SEARCH FAILS                                            BLKIO309          
C                                                                       BLKIO310          
300   KQZ001=M                                                          BLKIO311          
      IF(KQZ001.LT.1) KQZ001=1                                          BLKIO312          
      IF(KQZ001.GT.2) KQZ001=2                                          BLKIO313          
      GOTO(310,340),KQZ001                                              BLKIO314          
310   M=2                                                               BLKIO315          
C         (FOR SECOND PASS)                                             BLKIO316          
      REWIND JLU                                                        BLKIO317          
      GOTO 170                                                          BLKIO318          
C                                                                       BLKIO319          
C**** SEARCH FAILS                                                      BLKIO320          
C                                                                       BLKIO321          
340   LS=2                                                              BLKIO322          
C         (END-OF-FILE RETURN)                                          BLKIO323          
      WRITE(61,360) JLU,JLF                                             BLKIO324          
360   FORMAT(8H0OPEN-EO,5HF LUN,I3,6H  FILE,I6)                         BLKIO325          
380   RETURN                                                            BLKIO326          
      END                                                               BLKIO327          
      SUBROUTINE INPUT(B,A,LS)                                          BLKIO328          
      DIMENSION B(10),A(1),IP(1)                                        BLKIO329          
      EQUIVALENCE(PI,IP)                                                BLKIO330          
      I26=1                                                             BLKIO331          
      PI=B(7)                                                           BLKIO332          
      IBUFF=IP(I26)                                                     BLKIO333          
C                            IBUFF = CURRENT-BUFFER INDEX               BLKIO334          
      M=1                                                               BLKIO335          
C                            M=1 FOR DOUBLE BUFFER                      BLKIO336          
      IF(.NOT.(IBUFF.EQ.0)) GOTO 70                                     BLKIO337          
      M=2                                                               BLKIO338          
C                            M=2 FOR SINGLE BUFFER                      BLKIO339          
70    CONTINUE                                                          BLKIO340          
      PI=B(6)                                                           BLKIO341          
C         (ITEM INDEX, ACTING ALSO AS STATUS FLAG)                      BLKIO342          
      IF(IP(I26)-2) 100,410,150                                         BLKIO343          
100   CALL CHECKI(B)                                                    BLKIO344          
C                            CHECK PREVIOUS READ                        BLKIO345          
      PI=B(6)                                                           BLKIO346          
      IF(IP(I26).EQ.2) GOTO 410                                         BLKIO347          
C                            CHECK FOR LOGICAL EOF                      BLKIO348          
      KQZ001=M                                                          BLKIO349          
      IF(KQZ001.LT.1) KQZ001=1                                          BLKIO350          
      IF(KQZ001.GT.2) KQZ001=2                                          BLKIO351          
      GOTO(140,150),KQZ001                                              BLKIO352          
140   CALL BLOCKI(B)                                                    BLKIO353          
C                            READ NEXT BLOCK, IF DOUBLE BUFFER          BLKIO354          
150   ITEMIND=IP(I26)                                                   BLKIO355          
      PI=B(1)                                                           BLKIO356          
      NLEFT=IP(I26)                                                     BLKIO357          
      KQZ001=M                                                          BLKIO358          
      IF(KQZ001.LT.1) KQZ001=1                                          BLKIO359          
      IF(KQZ001.GT.2) KQZ001=2                                          BLKIO360          
      GOTO(190,200),KQZ001                                              BLKIO361          
190   IF(NLEFT.EQ.0) GOTO 100                                           BLKIO362          
C                            CHECK FOR ITEM AVAILABLE                   BLKIO363          
200   PI=B(2)                                                           BLKIO364          
      ITEMSIZ=IP(I26)                                                   BLKIO365          
      IF(.NOT.(ITEMSIZ.EQ.0)) GOTO 270                                  BLKIO366          
      L=ITEMIND+1                                                       BLKIO367          
      PI=B(L)                                                           BLKIO368          
C                            GET ITEM SIZE FROM ITEM                    BLKIO369          
      IP(1)=IP(1)/8                                                     BLKIO370          
      ITEMSIZ=IP(1)                                                     BLKIO371          
      B(L)=PI                                                           BLKIO372          
270   IP(1)=0                                                           BLKIO373          
      NLEFT=NLEFT-ITEMSIZ                                               BLKIO374          
      IP(I26)=NLEFT                                                     BLKIO375          
      B(1)=PI                                                           BLKIO376          
      N=1                                                               BLKIO377          
301   IF(N.GT.ITEMSIZ) GOTO 309                                         BLKIO378          
      ITEMIND=ITEMIND+1                                                 BLKIO379          
      A(N)=B(ITEMIND)                                                   BLKIO380          
      N=N+1                                                             BLKIO381          
      GOTO 301                                                          BLKIO382          
309   CONTINUE                                                          BLKIO383          
      KQZ001=M                                                          BLKIO384          
      IF(KQZ001.LT.1) KQZ001=1                                          BLKIO385          
      IF(KQZ001.GT.2) KQZ001=2                                          BLKIO386          
      GOTO(370,340),KQZ001                                              BLKIO387          
340   IF(.NOT.(NLEFT.EQ.0)) GOTO 370                                    BLKIO388          
      CALL BLOCKI(B)                                                    BLKIO389          
C                            READ NEXT BLOCK IF POSSIBLE                BLKIO390          
      ITEMIND=1                                                         BLKIO391          
370   IP(I26)=ITEMIND                                                   BLKIO392          
      B(6)=PI                                                           BLKIO393          
      LS=1                                                              BLKIO394          
C                            SET LOGICAL STATUS                         BLKIO395          
      GOTO 480                                                          BLKIO396          
C                                                                       BLKIO397          
C**** E.O.F.                                                            BLKIO398          
C                                                                       BLKIO399          
410   PI=B(4)                                                           BLKIO400          
      M=IP(I26)                                                         BLKIO401          
C         (LOGICAL FILE NUMBER)                                         BLKIO402          
      PI=B(5)                                                           BLKIO403          
      WRITE(61,450) IP(I26),M                                           BLKIO404          
450   FORMAT(8H0EOF LUN,I3,6H  FILE,I6)                                 BLKIO405          
      LS=2                                                              BLKIO406          
480   RETURN                                                            BLKIO407          
      END                                                               BLKIO408          
      SUBROUTINE BLOCKI(B)                                              BLKIO409          
      DIMENSION B(10),IP(1)                                             BLKIO410          
      EQUIVALENCE(PI,IP)                                                BLKIO411          
      I26=1                                                             BLKIO412          
      PI=B(7)                                                           BLKIO413          
      IBUFF=IP(I26)                                                     BLKIO414          
C                            IBUFF = CURRENT-BUFFER INDEX               BLKIO415          
      PI=B(3)                                                           BLKIO416          
      KB=IP(I26)                                                        BLKIO417          
C                            KB = BLOCK SIZE                            BLKIO418          
      J=9                                                               BLKIO419          
      IF(.NOT.(IBUFF.EQ.9)) GOTO 100                                    BLKIO420          
      J=KB+9                                                            BLKIO421          
C                            J = READ-BUFFER INDEX                      BLKIO422          
100   IP(I26)=KB+J-1                                                    BLKIO423          
      LWI=IP(I26)                                                       BLKIO424          
C                            LWI = LAST-WORD INDEX FOR TRANSMISSION     BLKIO425          
      B(8)=PI                                                           BLKIO426          
      PI=B(5)                                                           BLKIO427          
      LU=IP(I26)                                                        BLKIO428          
      BUFFERIN(LU,1)(B(J),B(LWI))                                       BLKIO429          
C                            READ NEXT BLOCK                            BLKIO430          
      RETURN                                                            BLKIO431          
      END                                                               BLKIO432          
      SUBROUTINE CHECKI(B)                                              BLKIO433          
      INTEGER UNITSTF                                                   BLKIO434          
      DIMENSION B(10),IP(1)                                             BLKIO435          
      EQUIVALENCE(PI,IP)                                                BLKIO436          
      I26=1                                                             BLKIO437          
      L=5                                                               BLKIO438          
C                            L = READ-RETRY COUNT                       BLKIO439          
      PI=B(3)                                                           BLKIO440          
      KB=IP(I26)                                                        BLKIO441          
C                            KB = BLOCK SIZE                            BLKIO442          
      PI=B(5)                                                           BLKIO443          
      LU=IP(I26)                                                        BLKIO444          
C                            LU = LOGICAL UNIT NUMBER                   BLKIO445          
      PI=B(8)                                                           BLKIO446          
      LWI=IP(I26)                                                       BLKIO447          
C                            LWI = LAST-WORD INDEX FOR TRANSMISSION     BLKIO448          
100   ISTATUS=UNITSTF(LU)                                               BLKIO449          
      KQZ001=ISTATUS                                                    BLKIO450          
      GOTO(100,120,250,280),KQZ001                                      BLKIO451          
C                            CHECK PREVIOUS READ                        BLKIO452          
120   IF(LWI.EQ.4) GOTO 230                                             BLKIO453          
C                            OMIT SWITCHING ON ID SEARCH                BLKIO454          
      IP(I26)=LENGTH(LU)/I26                                            BLKIO455          
      B(1)=PI                                                           BLKIO456          
C                            SET B(1) = NO. WORDS IN BLOCK              BLKIO457          
      PI=B(7)                                                           BLKIO458          
      KURBUF=IP(I26)                                                    BLKIO459          
      NEWBUF=9                                                          BLKIO460          
      IF(KURBUF-9) 220,190,200                                          BLKIO461          
190   NEWBUF=KB+9                                                       BLKIO462          
200   IP(I26)=NEWBUF                                                    BLKIO463          
      B(7)=PI                                                           BLKIO464          
C                            SWITCH BUFFERS                             BLKIO465          
220   IP(I26)=NEWBUF-1                                                  BLKIO466          
230   B(6)=PI                                                           BLKIO467          
C                            INITIALISE ITEM INDEX                      BLKIO468          
      GOTO 410                                                          BLKIO469          
C                                                                       BLKIO470          
C**** E.O.F.                                                            BLKIO471          
C                                                                       BLKIO472          
250   BACKSPACE LU                                                      BLKIO473          
      IP(I26)=2                                                         BLKIO474          
C                            BACKSPACE OVER EOF                         BLKIO475          
      GOTO 230                                                          BLKIO476          
C                                                                       BLKIO477          
C**** PARITY ERROR                                                      BLKIO478          
C                                                                       BLKIO479          
280   IF(.NOT.(L.EQ.0)) GOTO 320                                        BLKIO480          
      WRITE(61,300) LU                                                  BLKIO481          
300   FORMAT(8H0PARITY ,7HERR LUN,I3)                                   BLKIO482          
      GOTO 120                                                          BLKIO483          
320   L=L-1                                                             BLKIO484          
      BACKSPACE LU                                                      BLKIO485          
      IBUFF=1                                                           BLKIO486          
      IF(LWI.EQ.4) GOTO 390                                             BLKIO487          
      IBUFF=KB+9                                                        BLKIO488          
      IF(.NOT.(LWI.LT.IBUFF)) GOTO 380                                  BLKIO489          
      IBUFF=9                                                           BLKIO490          
380   CONTINUE                                                          BLKIO491          
390   BUFFERIN(LU,1)(B(IBUFF),B(LWI))                                   BLKIO492          
C                            BACKSPACE, READ AGAIN                      BLKIO493          
      GOTO 100                                                          BLKIO494          
410   RETURN                                                            BLKIO495          
      END                                                               BLKIO496          
      SUBROUTINE SKIP(LU)                                               BLKIO497          
      INTEGER UNITSTF                                                   BLKIO498          
5     BUFFERIN(LU,1) (A,A)                                              BLKIO499          
10    I=UNITSTF(LU)                                                     BLKIO500          
      GOTO(10,5,20,5),I                                                 BLKIO501          
20    RETURN                                                            BLKIO502          
      END                                                               BLKIO503          
