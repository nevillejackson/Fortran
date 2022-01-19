*TEXT                                                                                     
      PROGRAM NESREG(TAPE7=/6000,TAPE4=/6000,INPUT,OUTPUT,TAPE5=INPUT,                    
     1 TAPE6=OUTPUT)                                                                      
      COMMON A(35000)                                                                     
C***********************************************************************NSG30004          
C                                                                       NSG30005          
C            ---------------------------------------------              NSG30006          
C                       PROGRAM PACKAGE NESREG                          NSG30007          
C                       VERSION 3 FOR CDC 6600                          NSG30008          
C            ---------------------------------------------              NSG30009          
C                                                                       NSG30010          
C PROGRAM PACKAGE TO PARTITION VARIANCES AND ESTIMATE GENETIC PARAMETERSNSG30011          
C BY NESTED AOV AND/OR O-P REGRESSION.                                  NSG30012          
C                                                                       NSG30013          
C AUTHORS - K.HAMMOND AND D.H.MILLER, 1971.                             NSG30014          
C           DEPT. OF ANIMAL HUSBANDRY, UNIV. OF SYDNEY.                 NSG30015          
C                                                                       NSG30016          
C ADAPTED TO CDC 6600 BY - N.JACKSON, 1971.                             NSG30017          
C                          DIVISION OF ANIMAL GENETICS.                 NSG30018          
C                          C.S.I.R.O.                                   NSG30019          
C                                                                       NSG30020          
C***********************************************************************NSG30021          
C                                                                       NSG30022          
C PACKAGE CONSISTS OF A MAIN PROGRAM --NESREG-- AND  TEN  SUBROUTINES --NSG30023          
C --ADDRES,STEPS,STEP1,FULSIB,STEP2,MIXTUR,OPREG,PROUT,READX,AND GETFL  NSG30024          
C                                                                       NSG30025          
C DECK STRUCTURE FOR COMPILE AND EXECUTE WITH SOURCE DECK AND DATA ON   NSG30026          
C FILE INPUT IS --                                                      NSG30027          
C         JOBNAM(CM60000,CL100000,T--,P-)                               NSG30028          
C         FTN.                                                          NSG30029          
C         RFL(100000)                                                   NSG30030          
C         REDUCE.                                                       NSG30031          
C         LGO.                                                          NSG30032          
C         7/8/9                                                         NSG30033          
C           --                                                          NSG30034          
C           --                                                          NSG30035          
C           SOURCE DECK                                                 NSG30036          
C           --                                                          NSG30037          
C           --                                                          NSG30038          
C         7/8/9                                                         NSG30039          
C           --                                                          NSG30040          
C           --                                                          NSG30041          
C           DATA DECK                                                   NSG30042          
C           --                                                          NSG30043          
C           --                                                          NSG30044          
C         7/8/9                                                         NSG30045          
C         6/7/8/9                                                       NSG30046          
C                                                                       NSG30047          
C STRUCTURE OF DATA DECK IS --                                          NSG30048          
C         PARAMETER CARD TYPE 1 (NO OF PROBLEMS)                        NSG30049          
C         PARAMETER CARD TYPE 2 FOR PROBLEM NO 1 (SIZE OF PROBLEM)      NSG30050          
C         PARAMETER CARD TYPE 3 FOR PROBLEM NO 1 (TITLE)                NSG30051          
C         PARAMETER CARD(S) TYPE 4 FOR PROBLEM NO 1 (VARIABLE NAMES)    NSG30052          
C         DATA CARDS FOR PROBLEM NO 1                                   NSG30053          
C         BLANK TRAILER CARD                                            NSG30054          
C         PARAMETER CARD TYPE 2 FOR PROBLEM NO 2                        NSG30055          
C         PARAMETER CARD TYPE 3 FOR PROBLEM NO 2                        NSG30056          
C         PARAMETER CARD(S) TYPE 4 FOR PROBLEM NO 2                     NSG30057          
C         DATA DECK FOR PROBLEM NO 2                                    NSG30058          
C         BLANK TRAILER CARD                                            NSG30059          
C         --                                                            NSG30060          
C         -- ETC                                                        NSG30061          
C                                                                       NSG30062          
C PUNCHING SCHEDULE FOR PARAMETER CARD TYPE 1 IS --                     NSG30063          
C                                                                       NSG30064          
C         COLUMNS   SYMBOL              INFORMATION                     NSG30065          
C         -------   ------              -----------                     NSG30066          
C             1-4    NPROB    NUMBER OF PROBLEMS                        NSG30067          
C                                                                       NSG30068          
C PUNCHING SCHEDULE FOR PARAMETER CARDS TYPE 2 IS --                    NSG30069          
C                                                                       NSG30070          
C         COLUMNS   SYMBOL              INFORMATION                     NSG30071          
C         -------   ------              -----------                     NSG30072          
C                                                                       NSG30073          
C             1-5    ICHEK    START                                     NSG30074          
C               6        -    BLANK COLUMN                              NSG30075          
C             7-9       I1    NUMBER OF PSEUDO-VARIABLES                NSG30076          
C              10       I2    NUMBER OF FOLD (=NUMBER OF LEVELS - 1)    NSG30077          
C                             (MAXIMUM IS FIVE-FOLD)                    NSG30078          
C              11       N3    ANALYSIS REQUIRED                         NSG30079          
C                                  1 = COMPLETE PROBLEM                 NSG30080          
C                                  2 = NESTED PROBLEM ONLY              NSG30081          
C                                  3 = O-P PROBLEM ONLY                 NSG30082          
C              12       N4    OPTION FOR LISTING RAW DATA               NSG30083          
C                                  1 = LISTING REQUIRED                 NSG30084          
C                                  2 = LISTING NOT REQUIRED             NSG30085          
C                                  3 = LISTING OF FIRST THREE DATA SETS NSG30086          
C           13-16       N5    MAXIMUM NUMBER OF SIRES                   NSG30087          
C           17-20       N6    MAXIMUM NUMBER OF DAMS PER SIRE           NSG30088          
C           21-23       IS    NUMBER OF SIRE VARIABLES                  NSG30089          
C           24-26       ID    NUMBER OF DAM VARIABLES                   NSG30090          
C           27-29       IM    NUMBER OF MALE OFFSPRING VARIABLES        NSG30091          
C           30-32       IF    NUMBER OF FEMALE OFFSPRING VARIABLES      NSG30092          
C           33-34     ICYC    OPTION FOR SINGLE PASS OR CYCLIC OPERATIONNSG30093          
C                                  1 = SINGLE PASS                      NSG30094          
C                                  2 = CYCLIC SINGLE VARIABLES ONLY     NSG30095          
C                                  3 = CYCLIC SINGLE VARIABLES AND PAIRSNSG30096          
C                                  4 = CYCLIC PAIRS OF VARIABLES ONLY   NSG30097          
C           35-36      LUI    LOGICAL UNIT NUMBER FOR INPUT OF DATA SETSNSG30098          
C                                  PUNCH 05 FOR CARD INPUT              NSG30099          
C                                  PUNCH 07 FOR OTHER THAN CARD INPUT   NSG30100          
C           37-38      LUS    LOGICAL UNIT NUMBER FOR SCRATCH UNIT      NSG30101          
C                                  PUNCH 00 IF SINGLE PASS MODE         NSG30102          
C                                  PUNCH 04 IF IN CYCLIC MODE           NSG30103          
C                                                                       NSG30104          
C PUNCHING SCHEDULE FOR PARAMETER CARDS TYPE 3 IS --                    NSG30105          
C                                                                       NSG30106          
C         COLUMNS   SYMBOL              INFORMATION                     NSG30107          
C         -------   ------              -----------                     NSG30108          
C            1-72     ITLE    PROBLEM TITLE (ALPHAMERIC)                NSG30109          
C                                                                       NSG30110          
C PUNCHING SCHEDULE FOR PARAMETER CARDS TYPE 4 IS --                    NSG30111          
C                                                                       NSG30112          
C         COLUMNS   SYMBOL              INFORMATION                     NSG30113          
C         -------   ------              -----------                     NSG30114          
C             1-8  NAMVAR(1)  ALPHAMERIC NAME OF FIRST PSEUDO-VARIABLE  NSG30115          
C            9-16  NAMVAR(2)  ALPHAMERIC NAME OF SECOND PSEUDO-VARIABLE NSG30116          
C           17-24        --   --                                        NSG30117          
C           25-32        ETC  CONTINUE UP TO COLUMN 72 OF FIRST CARD ,  NSG30118          
C                             AND AS MANY SUBSEQUENT CARDS AS REQUIRED. NSG30119          
C                                                                       NSG30120          
C THE USER MUST PROVIDE A SUBROUTINE READX TO READ DATA CARDS.  THIS    NSG30121          
C PROVIDES FLEXIBILITY IN USE OF INPUT UNIT NUMBERS, AND MAKES DATA     NSG30122          
C EDITING AND TRANSFORMATIONS POSSIBLE.  A READX SUBROUTINE TO READ TESTNSG30123          
C DATA SET NO. 1 IS INCLUDED IN THE SOURCE DECK.                        NSG30124          
C         USAGE -                                                       NSG30125          
C                   CALL READX ( XID , X )                              NSG30126          
C         PARAMETERS-                                                   NSG30127          
C                   XID       FOLD IDENTIFICATION VECTOR. XID(1) IS     NSG30128          
C                             HIGHEST LEVEL , XID(5) IS LOWEST BUT ONE, NSG30129          
C                             ASSUMING ALL SIX LEVELS ARE PRESENT.      NSG30130          
C                   X         DATA VECTOR OF LENGTH I1.                 NSG30131          
C                                                                       NSG30132          
C         DATA MUST BE SORTED IN ORDER OF THE HEIRARCHY AND SOME FORM OFNSG30133          
C         IDENTIFICATION FOR EACH LEVEL USED MUST BE TRANSMITTED TO THE NSG30134          
C         MAIN PROGRAM VIA XID.  READX IS CALLED REPEATEDLY UNTIL A ZERONSG30135          
C         OR BLANK IS TRANSMITTED TO THE CALLING ROUTINE VIA XID(1).    NSG30136          
C                                                                       NSG30137          
C         A BLANK TRAILER CARD MUST BE PLACED AT THE END OF EACH DATA   NSG30138          
C         DECK TO ENABLE SUBROUTINE READX TO TRANSMIT A BLANK VIA XID(1)NSG30139          
C         THUS PREVENTING THE PARAMETER CARDS FOR THE NEXT PROBLEM      NSG30140          
C         (IF ANY) BEING READ AS DATA FOR THE CURRENT PROBLEM           NSG30141          
C                                                                       NSG30142          
C                                                                       NSG30143          
C         FOR BINOMIAL DATA X CONTAINS THE TRANSFORMED PERCENTAGES      NSG30144          
C         (ARCSIN SQRT PERCENT IN RADIANS) AND THE TOTAL NUMBER OF      NSG30145          
C         EVENTS FROM WHICH THE PERCENTAGES ARE CALCULATED IS           NSG30146          
C         TRANSMITTED VIA THE VARIABLE Y IN COMMON BLOCK/CMBLK1/        NSG30147          
C                                                                       NSG30148          
C                                                                       NSG30149          
C **********************************************************************NSG30150          
C                                                                       NSG30151          
C                                                                       NSG30152          
      COMMON/CMBLK1/I1,I2,N3,N4,N5,N6,IS,ID,IM,IF,N1,ITLE(9),Y,KPUT     NSG30153          
      COMMON/CMBLK2/IT2,ICOMEN,IWFSEN,IDFQ,IIDFQ,ISS,ISM,IFQ,IXMN,IVC1, NSG30154          
     1  IVC2,IVC3,IHS,IHD,IHSD,ISE1,ISE2,ISE3,ITHETA,IRTHET,ISER,ITHETG,NSG30155          
     3  ITHETP,IHDFS,IVAR1,INAMVA,LEN26                                 NSG30156          
      COMMON/CMBLK3/IX,IXID,IC2,IC3,ISX,IRXS,IOID,IPROD1,IPROD2,IL,ITEMPNSG30157          
     1  ,IPRODS,IBV1,IBV2,IBN,IAN,IPROD3,LEN44                          NSG30158          
      COMMON/CMBLK4/IREGSS,IDEVSS,ISE4,IHB,IHR,IRB,ISRR,IDEVSM,IFREG,   NSG30159          
     1  IB,IFDEV,ICORS,ICORD,ICHS,IVA,IVD,IVAA,ISL1,ISL2,ISL3,ISCR1,    NSG30160          
     2  ISESC1,ISCR2,ISESC2,IA,IFKH,ILS,IXMAT1,IXMAT2,ITS,LEN56         NSG30161          
      COMMON/CMBLK5/NPROB,Q1,Q2,Q3,BIVAR1,BIVAR2,BIVAR3,I6,I7,I8,I9     NSG30162          
      COMMON/CMBLK6/ DFS,DFD,IDFS,IDFD                                  NSG30163          
      COMMON/CMBLK7/ MI1,MIS,MID,MIM,MIF,M1,M2,ICYC,LUI,LUS             NSG30164          
      DATA(ISTART=5HSTART)                                              NSG30165          
C-----                                                                  NSG30166          
      WRITE(6,1)                                                        NSG30167          
    1 FORMAT(1H1,30X,14HPROGRAM NESREG//1H0,7X,70HANALYSIS OF QUANTITATINSG30168          
     1VE VARIATION BY NESTED AOV AND/OR O-P REGRESSION)                 NSG30169          
      READ(5,2) NPROB                                                   NSG30170          
    2 FORMAT(20I4)                                                      NSG30171          
      WRITE(6,6) NPROB                                                  NSG30172          
    6 FORMAT(//1H0,23X,21HNUMBER OF PROBLEMS = ,I4)                     NSG30173          
C-----                                                                  NSG30174          
C-----LOOP OVER PROBLEMS                                                NSG30175          
C-----                                                                  NSG30176          
      DO 7 N1=1,NPROB                                                   NSG30177          
      WRITE(6,8) N1                                                     NSG30178          
    8 FORMAT(1H1,20X,45HLISTING OF PARAMETER CARDS FOR PROBLEM NUMBER,I4NSG30179          
     1)                                                                 NSG30180          
      KPUT=0                                                            NSG30181          
    3 READ(5,4) ICHEK,I1,I2,N3,N4,N5,N6,IS,ID,IM,IF,ICYC,LUI,LUS        NSG30182          
    4 FORMAT(A5,1X,I3,3I1,I4,I4,4I3,3I2)                                NSG30183          
      IF(ICHEK-ISTART) 3,9,3                                            NSG30184          
    9 WRITE(6,20) ICHEK,I1,I2,N3,N4,N5,N6,IS,ID,IM,IF,ICYC,LUI,LUS      NSG30185          
   20 FORMAT(//71H0ICHEK   I1   I2   N3   N4   N5   N6   IS   ID   IM   NSG30186          
     1IF ICYC  LUI  LUS//1H ,A5,13I5)                                   NSG30187          
      READ(5,5) (ITLE(I),I=1,9)                                         NSG30188          
    5 FORMAT(9A8)                                                       NSG30189          
      WRITE(6,21) (ITLE(I),I=1,9)                                       NSG30190          
   21 FORMAT(//14H0PROBLEM TITLE/1H0,9A8)                               NSG30191          
      IF(ICYC-1) 30,30,31                                               NSG30192          
   31 REWIND LUS                                                        NSG30193          
   30 CONTINUE                                                          NSG30194          
C-----                                                                  NSG30195          
C-----CALCULATE FORMAL PARAMETERS REQUIRED FOR VARIABLE DIMENSIONS      NSG30196          
C-----                                                                  NSG30197          
      IIA=I1                                                            NSG30198          
      IIB=I2+1                                                          NSG30199          
      IIC=I2+2                                                          NSG30200          
      IID=N5                                                            NSG30201          
C-----MAX NO OF TRAITS                                                  NSG30202          
      MI1=I1                                                            NSG30203          
      MIS=IS                                                            NSG30204          
      MID=ID                                                            NSG30205          
      MIM=IM                                                            NSG30206          
      MIF=IF                                                            NSG30207          
C-----MAX NO OF TRAITS PER RELATIVE                                     NSG30208          
C-----AT LEAST ONE RELATIVE MUST HAVE ALL TRAITS MEASURED               NSG30209          
      IMAX=MAX0(IS,ID,IM,IF)                                            NSG30210          
C-----                                                                  NSG30211          
C-----                                                                  NSG30212          
C-----                                                                  NSG30213          
C-----FIND PROGRAM FIELD LENGTH AS LOADED                               NSG30214          
C-----                                                                  NSG30215          
      CALL GETFL(LPROG)                                                 NSG30216          
      WRITE(6,11) LPROG,LPROG                                           NSG30217          
   11 FORMAT(26H0FIELD LENGTH OF PROGRAM = ,I8,13H (DECIMAL) = ,O8,     NSG30218          
     1 8H (OCTAL))                                                      NSG30219          
C-----                                                                  NSG30220          
C-----                                                                  NSG30221          
C-----CALCULATE ADDRESSES FOR CYCLIC MODE                               NSG30222          
C-----MODIFY IIA ACCODDINGLY                                            NSG30223          
C-----                                                                  NSG30224          
      GO TO (55,56,57,57),ICYC                                          NSG30225          
   56 IS=MIN0(MIS,1)                                                    NSG30226          
      ID=MIN0(MID,1)                                                    NSG30227          
      IM=MIN0(MIM,1)                                                    NSG30228          
      IF=MIN0(MIF,1)                                                    NSG30229          
      I1=IS+ID+IM+IF                                                    NSG30230          
      IIA=I1                                                            NSG30231          
      GO TO 58                                                          NSG30232          
   57 IS=MIN0(MIS,2)                                                    NSG30233          
      ID=MIN0(MID,2)                                                    NSG30234          
      IM=MIN0(MIM,2)                                                    NSG30235          
      IF=MIN0(MIF,2)                                                    NSG30236          
      I1=IS+ID+IM+IF                                                    NSG30237          
      IIA=I1                                                            NSG30238          
   58 CALL ADDRES(IIA,IIB,IIC,IID)                                      NSG30239          
C-----                                                                  NSG30240          
C-----CALCULATE FIELD LENGTH FOR STEPS ONE AND TWO                      NSG30241          
C-----                                                                  NSG30242          
      LFIELD=LPROG+MAX0(LEN44,LEN56)                                    NSG30243          
      WRITE(6,13) LFIELD,LFIELD                                         NSG30244          
   13 FORMAT(55H0FIELD LENGTH OF PROGRAM PLUS ARRAYS FOR CYCLIC MODE = ,NSG30245          
     1I8,13H (DECIMAL) = ,O8,8H (OCTAL))                                NSG30246          
C-----                                                                  NSG30247          
C-----SET FIELD LENGTH FOR CYCLIC MODE                                  NSG30248          
C-----                                                                  NSG30249          
      CALL SETFL(LFIELD)                                                NSG30250          
C-----                                                                  NSG30251          
C-----IF IN CYCLIC MODE READ VARIABLE NAMES PARAMETER CARD AND COPY     NSG30252          
C-----DATA DECK TO UNIT 4.                                              NSG30253          
C-----                                                                  NSG30254          
      M1=0                                                              NSG30255          
      M2=0                                                              NSG30256          
      CALL STEP1 (IIA,IIB,IIC,IID,                                      NSG30257          
     1A(IT2),A(ICOMEN),A(IWFSEN),A(IDFQ),A(IIDFQ),A(ISS),A(ISM),A(IFQ), NSG30258          
     2A(IXMN),A(IVC1),A(IVC2),A(IVC3),A(IHS),A(IHD),A(IHSD),A(ISE1),    NSG30259          
     3A(ISE2),A(ISE3),A(ITHETA),A(IRTHET),A(ISER),A(ITHETG),A(ITHETP),  NSG30260          
     4A(IHDFS),A(IVAR1),A(INAMVA),                                      NSG30261          
     5 A)                                                               NSG30262          
   55 CONTINUE                                                          NSG30263          
C-----CYCLIC OR SINGLE PASS OPERATION                                   NSG30264          
      GO TO (51,52,52,53),ICYC                                          NSG30265          
C-----SINGLE PASS BRANCH                                                NSG30266          
   51 CALL ADDRES(IIA,IIB,IIC,IID)                                      NSG30267          
      M1=0                                                              NSG30268          
      M2=0                                                              NSG30269          
      CALL STEPS(IIA,IIB,IIC,IID,LPROG,A)                               NSG30270          
      GO TO 7                                                           NSG30271          
C-----CYCLIC BRANCH                                                     NSG30272          
C-----SINGLE TRAITS                                                     NSG30273          
   52 IS=MIN0(MIS,1)                                                    NSG30274          
      ID=MIN0(MID,1)                                                    NSG30275          
      IM=MIN0(MIM,1)                                                    NSG30276          
      IF=MIN0(MIF,1)                                                    NSG30277          
      I1=IS+ID+IM+IF                                                    NSG30278          
      WRITE(6,60)                                                       NSG30279          
   60 FORMAT(1H1,10X,29HANALYSES FOR SINGLE VARIABLES)                  NSG30280          
      DO 50 I=1,IMAX                                                    NSG30281          
      M1=I                                                              NSG30282          
      M2=I                                                              NSG30283          
      REWIND LUS                                                        NSG30284          
      KPUT=0                                                            NSG30285          
   50 CALL STEPS(IIA,IIB,IIC,IID,LPROG,A)                               NSG30286          
      GO TO (7,7,53,53),ICYC                                            NSG30287          
C-----PAIRS OF TRAITS                                                   NSG30288          
   53 IMIX=IMAX-1                                                       NSG30289          
      IS=MIN0(MIS,2)                                                    NSG30290          
      ID=MIN0(MID,2)                                                    NSG30291          
      IM=MIN0(MIM,2)                                                    NSG30292          
      IF=MIN0(MIF,2)                                                    NSG30293          
      I1=IS+ID+IM+IF                                                    NSG30294          
      WRITE(6,61)                                                       NSG30295          
   61 FORMAT(1H1,10X,31HANALYSES FOR PAIRS OF VARIABLES)                NSG30296          
      DO 54 I=1,IMIX                                                    NSG30297          
      M1=I                                                              NSG30298          
      JMIN=I+1                                                          NSG30299          
      DO 54 J=JMIN,IMAX                                                 NSG30300          
      M2=J                                                              NSG30301          
      REWIND LUS                                                        NSG30302          
      KPUT=0                                                            NSG30303          
   54 CALL STEPS(IIA,IIB,IIC,IID,LPROG,A)                               NSG30304          
    7 CONTINUE                                                          NSG30305          
      STOP                                                              NSG30306          
      END                                                               NSG30307          
*ENDTEXT                                                                                  
