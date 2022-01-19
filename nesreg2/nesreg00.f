*TEXT                                                                                     
      PROGRAM NESREG(TAPE7,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                         
      COMMON A(17000)                                                                     
C***********************************************************************NSG20003          
C                                                                       NSG20004          
C            ---------------------------------------------              NSG20005          
C                       PROGRAM PACKAGE NESREG                          NSG20006          
C                       VERSION 2 FOR CDC 6600                          NSG20007          
C            ---------------------------------------------              NSG20008          
C                                                                       NSG20009          
C PROGRAM PACKAGE TO PARTITION VARIANCES AND ESTIMATE GENETIC PARAMETERSNSG20010          
C BY NESTED AOV AND/OR O-P REGRESSION.                                  NSG20011          
C                                                                       NSG20012          
C AUTHORS - K.HAMMOND AND D.H.MILLER, 1971.                             NSG20013          
C           DEPT. OF ANIMAL HUSBANDRY, UNIV. OF SYDNEY.                 NSG20014          
C                                                                       NSG20015          
C ADAPTED TO CDC 6600 BY - N.JACKSON, 1971.                             NSG20016          
C                          DIVISION OF ANIMAL GENETICS.                 NSG20017          
C                          C.S.I.R.O.                                   NSG20018          
C                                                                       NSG20019          
C***********************************************************************NSG20020          
C                                                                       NSG20021          
C PACKAGE CONSISTS OF A MAIN PROGRAM --NESREG-- AND EIGHT SUBROUTINES --NSG20022          
C --STEP1,FULSIB,STEP2,MIXTUR,OPREG,PROUT,GETFL,AND READX               NSG20023          
C                                                                       NSG20024          
C DECK STRUCTURE FOR COMPILE AND EXECUTE WITH SOURCE DECK AND DATA ON   NSG20025          
C FILE INPUT IS --                                                      NSG20026          
C         JOBNAM(CM60000,CL100000,T--,P-)                               NSG20027          
C         FTN.                                                          NSG20028          
C         RFL(100000)                                                   NSG20029          
C         REDUCE.                                                       NSG20030          
C         LGO.                                                          NSG20031          
C         7/8/9                                                         NSG20032          
C           --                                                          NSG20033          
C           --                                                          NSG20034          
C           SOURCE DECK                                                 NSG20035          
C           --                                                          NSG20036          
C           --                                                          NSG20037          
C         7/8/9                                                         NSG20038          
C           --                                                          NSG20039          
C           --                                                          NSG20040          
C           DATA DECK                                                   NSG20041          
C           --                                                          NSG20042          
C           --                                                          NSG20043          
C         7/8/9                                                         NSG20044          
C         6/7/8/9                                                       NSG20045          
C                                                                       NSG20046          
C STRUCTURE OF DATA DECK IS --                                          NSG20047          
C         PARAMETER CARD TYPE 1 (NO OF PROBLEMS)                        NSG20048          
C         PARAMETER CARD TYPE 2 FOR PROBLEM NO 1 (SIZE OF PROBLEM)      NSG20049          
C         PARAMETER CARD TYPE 3 FOR PROBLEM NO 1 (TITLE)                NSG20050          
C         PARAMETER CARD(S) TYPE 4 FOR PROBLEM NO 1 (VARIABLE NAMES)    NSG20051          
C         DATA CARDS FOR PROBLEM NO 1                                   NSG20052          
C         BLANK TRAILER CARD                                            NSG20053          
C         PARAMETER CARD TYPE 2 FOR PROBLEM NO 2                        NSG20054          
C         PARAMETER CARD TYPE 3 FOR PROBLEM NO 2                        NSG20055          
C         PARAMETER CARD(S) TYPE 4 FOR PROBLEM NO 2                     NSG20056          
C         DATA DECK FOR PROBLEM NO 2                                    NSG20057          
C         BLANK TRAILER CARD                                            NSG20058          
C         --                                                            NSG20059          
C         -- ETC                                                        NSG20060          
C                                                                       NSG20061          
C PUNCHING SCHEDULE FOR PARAMETER CARD TYPE 1 IS --                     NSG20062          
C                                                                       NSG20063          
C         COLUMNS   SYMBOL              INFORMATION                     NSG20064          
C         -------   ------              -----------                     NSG20065          
C             1-4    NPROB    NUMBER OF PROBLEMS                        NSG20066          
C                                                                       NSG20067          
C PUNCHING SCHEDULE FOR PARAMETER CARDS TYPE 2 IS --                    NSG20068          
C                                                                       NSG20069          
C         COLUMNS   SYMBOL              INFORMATION                     NSG20070          
C         -------   ------              -----------                     NSG20071          
C                                                                       NSG20072          
C             1-5    ICHEK    START                                     NSG20073          
C               6        -    BLANK COLUMN                              NSG20074          
C             7-9       I1    NUMBER OF PSEUDO-VARIABLES                NSG20075          
C              10       I2    NUMBER OF FOLD (=NUMBER OF LEVELS - 1)    NSG20076          
C                             (MAXIMUM IS FIVE-FOLD)                    NSG20077          
C              11       N3    ANALYSIS REQUIRED                         NSG20078          
C                                  1 = COMPLETE PROBLEM                 NSG20079          
C                                  2 = NESTED PROBLEM ONLY              NSG20080          
C                                  3 = O-P PROBLEM ONLY                 NSG20081          
C              12       N4    OPTION FOR LISTING RAW DATA               NSG20082          
C                                  1 = LISTING REQUIRED                 NSG20083          
C                                  2 = LISTING NOT REQUIRED             NSG20084          
C                                  3 = LISTING OF FIRST THREE DATA SETS NSG20085          
C           13-16       N5    MAXIMUM NUMBER OF SIRES                   NSG20086          
C           17-20       N6    MAXIMUM NUMBER OF DAMS PER SIRE           NSG20087          
C           21-23       IS    NUMBER OF SIRE VARIABLES                  NSG20088          
C           24-26       ID    NUMBER OF DAM VARIABLES                   NSG20089          
C           27-29       IM    NUMBER OF MALE OFFSPRING VARIABLES        NSG20090          
C           30-32       IF    NUMBER OF FEMALE OFFSPRING VARIABLES      NSG20091          
C                                                                       NSG20092          
C PUNCHING SCHEDULE FOR PARAMETER CARDS TYPE 3 IS --                    NSG20093          
C                                                                       NSG20094          
C         COLUMNS   SYMBOL              INFORMATION                     NSG20095          
C         -------   ------              -----------                     NSG20096          
C            1-72     ITLE    PROBLEM TITLE (ALPHAMERIC)                NSG20097          
C                                                                       NSG20098          
C PUNCHING SCHEDULE FOR PARAMETER CARDS TYPE 4 IS --                    NSG20099          
C                                                                       NSG20100          
C         COLUMNS   SYMBOL              INFORMATION                     NSG20101          
C         -------   ------              -----------                     NSG20102          
C             1-8  NAMVAR(1)  ALPHAMERIC NAME OF FIRST PSEUDO-VARIABLE  NSG20103          
C            9-16  NAMVAR(2)  ALPHAMERIC NAME OF SECOND PSEUDO-VARIABLE NSG20104          
C           17-24        --   --                                        NSG20105          
C           25-32        ETC  CONTINUE UP TO COLUMN 72 OF FIRST CARD ,  NSG20106          
C                             AND AS MANY SUBSEQUENT CARDS AS REQUIRED. NSG20107          
C                                                                       NSG20108          
C THE USER MUST PROVIDE A SUBROUTINE READX TO READ DATA CARDS.  THIS    NSG20109          
C PROVIDES FLEXIBILITY IN USE OF INPUT UNIT NUMBERS, AND MAKES DATA     NSG20110          
C EDITING AND TRANSFORMATIONS POSSIBLE.  A READX SUBROUTINE TO READ TESTNSG20111          
C DATA SET NO. 1 IS INCLUDED IN THE SOURCE DECK.                        NSG20112          
C         USAGE -                                                       NSG20113          
C                   CALL READX ( XID , X )                              NSG20114          
C         PARAMETERS-                                                   NSG20115          
C                   XID       FOLD IDENTIFICATION VECTOR. XID(1) IS     NSG20116          
C                             HIGHEST LEVEL , XID(5) IS LOWEST BUT ONE, NSG20117          
C                             ASSUMING ALL SIX LEVELS ARE PRESENT.      NSG20118          
C                   X         DATA VECTOR OF LENGTH I1.                 NSG20119          
C                                                                       NSG20120          
C         DATA MUST BE SORTED IN ORDER OF THE HEIRARCHY AND SOME FORM OFNSG20121          
C         IDENTIFICATION FOR EACH LEVEL USED MUST BE TRANSMITTED TO THE NSG20122          
C         MAIN PROGRAM VIA XID.  READX IS CALLED REPEATEDLY UNTIL A ZERONSG20123          
C         OR BLANK IS TRANSMITTED TO THE CALLING ROUTINE VIA XID(1).    NSG20124          
C                                                                       NSG20125          
C         A BLANK TRAILER CARD MUST BE PLACED AT THE END OF EACH DATA   NSG20126          
C         DECK TO ENABLE SUBROUTINE READX TO TRANSMIT A BLANK VIA XID(1)NSG20127          
C         THUS PREVENTING THE PARAMETER CARDS FOR THE NEXT PROBLEM      NSG20128          
C         (IF ANY) BEING READ AS DATA FOR THE CURRENT PROBLEM           NSG20129          
C                                                                       NSG20130          
C                                                                       NSG20131          
C         FOR BINOMIAL DATA X CONTAINS THE TRANSFORMED PERCENTAGES      NSG20132          
C         (ARCSIN SQRT PERCENT IN RADIANS) AND THE TOTAL NUMBER OF      NSG20133          
C         EVENTS FROM WHICH THE PERCENTAGES ARE CALCULATED IS           NSG20134          
C         TRANSMITTED VIA THE VARIABLE Y IN COMMON BLOCK/CMBLK1/        NSG20135          
C                                                                       NSG20136          
C                                                                       NSG20137          
C **********************************************************************NSG20138          
C                                                                       NSG20139          
C                                                                       NSG20140          
      COMMON/CMBLK1/I1,I2,N3,N4,N5,N6,IS,ID,IM,IF,N1,ITLE(9),Y,KPUT     NSG20141          
      COMMON/CMBLK2/IT2,ICOMEN,IWFSEN,IDFQ,IIDFQ,ISS,ISM,IFQ,IXMN,IVC1, NSG20142          
     1  IVC2,IVC3,IHS,IHD,IHSD,ISE1,ISE2,ISE3,ITHETA,IRTHET,ISER,ITHETG,NSG20143          
     3  ITHETP,IHDFS,IVAR1,INAMVA,LEN26                                 NSG20144          
      COMMON/CMBLK3/IX,IXID,IC2,IC3,ISX,IRXS,IOID,IPROD1,IPROD2,IL,ITEMPNSG20145          
     1  ,IPRODS,IBV1,IBV2,IBN,IAN,IPROD3,LEN44                          NSG20146          
      COMMON/CMBLK4/IREGSS,IDEVSS,ISE4,IHB,IHR,IRB,ISRR,IDEVSM,IFREG,   NSG20147          
     1  IB,IFDEV,ICORS,ICORD,ICHS,IVA,IVD,IVAA,ISL1,ISL2,ISL3,ISCR1,    NSG20148          
     2  ISESC1,ISCR2,ISESC2,IA,IFKH,ILS,IXMAT1,IXMAT2,ITS,LEN56         NSG20149          
      COMMON/CMBLK5/NPROB,Q1,Q2,Q3,BIVAR1,BIVAR2,BIVAR3,I6,I7,I8,I9     NSG20150          
      COMMON/CMBLK6/ DFS,DFD,IDFS,IDFD                                  NSG20151          
      DATA(ISTART=5HSTART)                                              NSG20152          
C-----                                                                  NSG20153          
      WRITE(6,1)                                                        NSG20154          
    1 FORMAT(1H1,30X,14HPROGRAM NESREG//1H0,7X,70HANALYSIS OF QUANTITATINSG20155          
     1VE VARIATION BY NESTED AOV AND/OR O-P REGRESSION)                 NSG20156          
      READ(5,2) NPROB                                                   NSG20157          
    2 FORMAT(20I4)                                                      NSG20158          
      WRITE(6,6) NPROB                                                  NSG20159          
    6 FORMAT(//1H0,23X,21HNUMBER OF PROBLEMS = ,I4)                     NSG20160          
C-----                                                                  NSG20161          
C-----LOOP OVER PROBLEMS                                                NSG20162          
C-----                                                                  NSG20163          
      DO 7 N1=1,NPROB                                                   NSG20164          
      WRITE(6,8) N1                                                     NSG20165          
    8 FORMAT(1H1,20X,45HLISTING OF PARAMETER CARDS FOR PROBLEM NUMBER,I4NSG20166          
     1)                                                                 NSG20167          
      KPUT=0                                                            NSG20168          
    3 READ(5,4) ICHEK,I1,I2,N3,N4,N5,N6,IS,ID,IM,IF                     NSG20169          
    4 FORMAT(A5,1X,I3,3I1,I4,I4,4I3)                                    NSG20170          
      IF(ICHEK-ISTART) 3,9,3                                            NSG20171          
    9 WRITE(6,20) ICHEK,I1,I2,N3,N4,N5,N6,IS,ID,IM,IF                   NSG20172          
   20 FORMAT(//56H0ICHEK   I1   I2   N3   N4   N5   N6   IS   ID   IM   NSG20173          
     1IF//1H ,A5,10I5)                                                  NSG20174          
      READ(5,5) (ITLE(I),I=1,9)                                         NSG20175          
    5 FORMAT(9A8)                                                       NSG20176          
      WRITE(6,21) (ITLE(I),I=1,9)                                       NSG20177          
   21 FORMAT(//14H0PROBLEM TITLE/1H0,9A8)                               NSG20178          
C-----                                                                  NSG20179          
C-----CALCULATE FORMAL PARAMETERS REQUIRED FOR VARIABLE DIMENSIONS      NSG20180          
C-----                                                                  NSG20181          
      IIA=I1                                                            NSG20182          
      IIB=I2+1                                                          NSG20183          
      IIC=I2+2                                                          NSG20184          
      IID=N5                                                            NSG20185          
C-----                                                                  NSG20186          
C-----                                                                  NSG20187          
C-----                                                                  NSG20188          
C-----FIND PROGRAM FIELD LENGTH AS LOADED                               NSG20189          
C-----                                                                  NSG20190          
      CALL GETFL(LPROG)                                                 NSG20191          
      WRITE(6,11) LPROG,LPROG                                           NSG20192          
   11 FORMAT(26H0FIELD LENGTH OF PROGRAM = ,I8,13H (DECIMAL) = ,O8,     NSG20193          
     1 8H (OCTAL))                                                      NSG20194          
C-----                                                                  NSG20195          
C-----CALCULATE STARTING POSITION OF ALL ARRAYS COMMON TO STEPS 1 AND 2 NSG20196          
C-----                                                                  NSG20197          
      J1=I1*I1                                                          NSG20198          
      IT2=1                                                             NSG20199          
      ICOMEN=IT2+21                                                     NSG20200          
      IWFSEN=ICOMEN+J1                                                  NSG20201          
      IDFQ=IWFSEN+J1                                                    NSG20202          
      IIDFQ=IDFQ+I2+3                                                   NSG20203          
      ISS=IIDFQ+I2+3                                                    NSG20204          
      ISM=ISS+(I2+2)*J1                                                 NSG20205          
      IFQ=ISM+(I2+1)*J1                                                 NSG20206          
      IXMN=IFQ+(I2+1)*J1                                                NSG20207          
      IVC1=IXMN+J1                                                      NSG20208          
      IVC2=IVC1+J1                                                      NSG20209          
      IVC3=IVC2+J1                                                      NSG20210          
      IHS=IVC3+J1                                                       NSG20211          
      IHD=IHS+J1                                                        NSG20212          
      IHSD=IHD+J1                                                       NSG20213          
      ISE1=IHSD+J1                                                      NSG20214          
      ISE2=ISE1+J1                                                      NSG20215          
      ISE3=ISE2+J1                                                      NSG20216          
      ITHETA=ISE3+J1                                                    NSG20217          
      IRTHET=ITHETA+7*J1                                                NSG20218          
      ISER=IRTHET+7*J1                                                  NSG20219          
      ITHETG=ISER+7*J1                                                  NSG20220          
      ITHETP=ITHETG+J1                                                  NSG20221          
      IHDFS=ITHETP+J1                                                   NSG20222          
      IVAR1=IHDFS+J1                                                    NSG20223          
      INAMVA=IVAR1+J1                                                   NSG20224          
      LEN26=INAMVA+I1-1                                                 NSG20225          
C-----                                                                  NSG20226          
C-----CALCULATE STARTING POSITIONS OF ALL ARRAYS SPECIAL TO STEP 1      NSG20227          
C-----                                                                  NSG20228          
      IX=LEN26+1                                                        NSG20229          
      IXID=IX+I1                                                        NSG20230          
      IC2=IXID+I2+1                                                     NSG20231          
      IC3=IC2+I2+3                                                      NSG20232          
      ISX=IC3+I2+3                                                      NSG20233          
      IRXS=ISX+(I2+1)*I1                                                NSG20234          
      IOID=IRXS+I1                                                      NSG20235          
      IPROD1=IOID+I2                                                    NSG20236          
      IPROD2=IPROD1+J1                                                  NSG20237          
      IL=IPROD2+J1                                                      NSG20238          
      ITEMP=IL+7                                                        NSG20239          
      IPRODS=ITEMP+7                                                    NSG20240          
      IBV1=IPRODS+J1                                                    NSG20241          
      IBV2=IBV1+N5                                                      NSG20242          
      IBN=IBV2+N5*N6                                                    NSG20243          
      IAN=IBN+N5                                                        NSG20244          
      IPROD3=IAN+N5*N6                                                  NSG20245          
      LEN44=IPROD3+J1-1                                                 NSG20246          
      WRITE(6,10) LEN44,LEN44                                           NSG20247          
   10 FORMAT(43H0FIELD LENGTH OF ARRAYS USED BY STEP ONE = ,I8,         NSG20248          
     1 13H (DECIMAL) = ,O8,8H (OCTAL))                                  NSG20249          
C-----                                                                  NSG20250          
C-----CALCULATE STARTING POSITIONS OF ALL ARRAYS SPECIAL TO STEP 2      NSG20251          
      J4=4*J1                                                           NSG20252          
      IREGSS=LEN26+1                                                    NSG20253          
      IDEVSS=IREGSS+J4                                                  NSG20254          
      ISE4=IDEVSS+J4                                                    NSG20255          
      IHB=ISE4+J4                                                       NSG20256          
      IHR=IHB+J4                                                        NSG20257          
      IRB=IHR+J4                                                        NSG20258          
      ISRR=IRB+J4                                                       NSG20259          
      IDEVSM=ISRR+J4                                                    NSG20260          
      IFREG=IDEVSM+J4                                                   NSG20261          
      IB=IFREG+J4                                                       NSG20262          
      IFDEV=IB+J4                                                       NSG20263          
      ICORS=IFDEV+J4                                                    NSG20264          
      ICORD=ICORS+J1                                                    NSG20265          
      ICHS=ICORD+J1                                                     NSG20266          
      IVA=ICHS+J1                                                       NSG20267          
      IVD=IVA+J1                                                        NSG20268          
      IVAA=IVD+J1                                                       NSG20269          
      ISL1=IVAA+J1                                                      NSG20270          
      ISL2=ISL1+J1                                                      NSG20271          
      ISL3=ISL2+J1                                                      NSG20272          
      ISCR1=ISL3+J1                                                     NSG20273          
      ISESC1=ISCR1+3*J1                                                 NSG20274          
      ISCR2=ISESC1+3*J1                                                 NSG20275          
      ISESC2=ISCR2+4*J1                                                 NSG20276          
      IA=ISESC2+4*J1                                                    NSG20277          
      IFKH=IA+4*J1                                                      NSG20278          
      ILS=IFKH+3                                                        NSG20279          
      IXMAT1=ILS+3                                                      NSG20280          
      IXMAT2=IXMAT1+J1                                                  NSG20281          
      ITS=IXMAT2+J1                                                     NSG20282          
      LEN56=ITS+21-1                                                    NSG20283          
C-----                                                                  NSG20284          
      WRITE(6,12) LEN56,LEN56                                           NSG20285          
   12 FORMAT(43H0FIELD LENGTH OF ARRAYS USED BY STEP TWO = ,I8,         NSG20286          
     1  13H (DECIMAL) = ,O8,8H (OCTAL))                                 NSG20287          
C-----                                                                  NSG20288          
C-----CALCULATE TOTAL FIELD LENGTH FOR STEP 1                           NSG20289          
C-----                                                                  NSG20290          
      LFIELD=LPROG+LEN44                                                NSG20291          
      WRITE(6,13) LFIELD,LFIELD                                         NSG20292          
   13 FORMAT(52H0FIELD LENGTH OF PROGRAM PLUS ARRAYS FOR STEP ONE = ,   NSG20293          
     1  I8,13H (DECIMAL) = ,O8,8H (OCTAL))                              NSG20294          
C-----                                                                  NSG20295          
C-----SET FIELD LENGTH FOR STEP ONE                                     NSG20296          
C-----                                                                  NSG20297          
      CALL SETFL(LFIELD)                                                NSG20298          
C-----                                                                  NSG20299          
C-----STEP ONE                                                          NSG20300          
C-----BRANCH TO SUBROUTINE STEP1                                        NSG20301          
C-----                                                                  NSG20302          
      CALL STEP1 (  IIA,IIB,IIC,IID,                                    NSG20303          
     1A(IT2),A(ICOMEN),A(IWFSEN),A(IDFQ),A(IIDFQ),A(ISS),A(ISM),A(IFQ), NSG20304          
     2A(IXMN),A(IVC1),A(IVC2),A(IVC3),A(IHS),A(IHD),A(IHSD),A(ISE1),    NSG20305          
     3A(ISE2),A(ISE3),A(ITHETA),A(IRTHET),A(ISER),A(ITHETG),A(ITHETP),  NSG20306          
     4A(IHDFS),A(IVAR1),A(INAMVA),                                      NSG20307          
     5 A)                                                               NSG20308          
C-----                                                                  NSG20309          
      IF(KPUT) 7,31,30                                                  NSG20310          
C-----                                                                  NSG20311          
C-----CALCULATE TOTAL FIELD LENGTH FOR STEP 2                           NSG20312          
C-----                                                                  NSG20313          
   31 LFIELD=LPROG+LEN56                                                NSG20314          
      WRITE(6,14) LFIELD,LFIELD                                         NSG20315          
   14 FORMAT(52H0FIELD LENGTH OF PROGRAM PLUS ARRAYS FOR STEP TWO = ,   NSG20316          
     1 I8,13H (DECIMAL) = ,O8,8H (OCTAL))                               NSG20317          
C-----                                                                  NSG20318          
C-----SET FIELD LENGTH FOR STEP TWO                                     NSG20319          
C-----                                                                  NSG20320          
      CALL SETFL(LFIELD)                                                NSG20321          
C-----                                                                  NSG20322          
C-----STEP TWO                                                          NSG20323          
C-----BRANCH TO SUBROUTINE STEP2                                        NSG20324          
C-----                                                                  NSG20325          
      CALL STEP2 ( IIA,IIB,IIC,                                         NSG20326          
     1A(IT2),A(ICOMEN),A(IWFSEN),A(IDFQ),A(IIDFQ),A(ISS),A(ISM),A(IFQ), NSG20327          
     2A(IXMN),A(IVC1),A(IVC2),A(IVC3),A(IHS),A(IHD),A(IHSD),A(ISE1),    NSG20328          
     3A(ISE2),A(ISE3),A(ITHETA),A(IRTHET),A(ISER),A(ITHETG),A(ITHETP),  NSG20329          
     4A(IHDFS),A(IVAR1),A(INAMVA),                                      NSG20330          
     5 A)                                                               NSG20331          
      GO TO 7                                                           NSG20332          
C-----                                                                  NSG20333          
C-----FLUSH THRU OBSERVATION SETS FOR THIS PROBLEM                      NSG20334          
   30 CALL READX(A,X)                                                   NSG20335          
      IF(A(1)) 30,7,30                                                  NSG20336          
C-----                                                                  NSG20337          
    7 CONTINUE                                                          NSG20338          
      STOP                                                              NSG20339          
      END                                                               NSG20340          
*ENDTEXT                                                                                  
