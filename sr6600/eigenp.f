      SUBROUTINE EIGENP(N,NM,A,EVR,EVI,VECR,VECI,INDIC)                 EIGEN002          
      REAL T                                                            EIGEN003          
      INTEGER I,IVEC,J,K,K1,KON,L,L1,M,N,NM                             EIGEN004          
      DOUBLEPRECISION R,R1,D1,D2,D3,PRFACT,WORK,WORK1,WORK2,SUBDIA,     EIGEN005          
     .ENORM,EPS,EX,DSQRT                                                EIGEN006          
      DIMENSION A(NM,1),VECR(NM,1),VECI(NM,1),EVR(NM),EVI(NM),INDIC(NM) EIGEN007          
     .,IWORK(100),LOCAL(100),PRFACT(100),SUBDIA(100),WORK1(100),WORK2(  EIGEN008          
     .100),WORK(100)                                                    EIGEN009          
C                                                                       EIGEN010          
C A.C.M. ALGORITHM NUMBER 343                                           EIGEN011          
C REVISED JULY, 1970 BY N.R.PUMMEROY, DCR, CSIRO, CANBERRA              EIGEN012          
C THE FOLLOWING VARIABLES WERE CHANGED                                  EIGEN013          
C FROM SINGLE TO DOUBLE PRECISION IN EIGENP:                            EIGEN014          
C R,R1,ENORM,EPS,EX,WORK,WORK1,WORK2,SUBDIA                             EIGEN015          
C SEE ALSO COMMENTS FOR ROUTINES SCALE, HESQR, REALVE, COMPVE           EIGEN016          
C                                                                       EIGEN017          
C                                                                       EIGEN018          
C THIS SUBROUTINE FINDS ALL THE EIGENVALUES AND THE                     EIGEN019          
C EIGENVECTORS OF A REAL GENERAL MATRIX OF ORDER N.                     EIGEN020          
C                                                                       EIGEN021          
C FIRST IN THE SUBROUTINE SCALE THE MATRIX IS SCALED SO THAT            EIGEN022          
C THE CORRESPONDING ROWS AND COLUMNS ARE APPROXIMATELY                  EIGEN023          
C BALANCED AND THEN THE MATRIX IS NORMALISED SO THAT THE                EIGEN024          
C VALUE OF THE EUCLIDIAN NORM OF THE MATRIX IS EQUAL TO ONE.            EIGEN025          
C                                                                       EIGEN026          
C THE EIGENVALUES ARE COMPUTED BY THE QR DOUBLE-STEP METHOD             EIGEN027          
C IN THE SUBROUTINE HESQR.                                              EIGEN028          
C THE EIGENVECTORS ARE COMPUTED BY INVERSE ITERATION IN                 EIGEN029          
C THE SUBROUTINE REALVE,FOR THE REAL EIGENVALUES,OR IN THE              EIGEN030          
C SUBROUTINE COMPVE,FOR THE COMPLEX EIGENVALUES.                        EIGEN031          
C                                                                       EIGEN032          
C THE ELEMENTS OF THE MATRIX ARE TO BE STORED IN THE FIRST N            EIGEN033          
C ROWS AND COLUMNS OF THE TWO DIMENSIONAL ARRAY A. THE                  EIGEN034          
C ORIGINAL MATRIX IS DESTROYED BY THE SUBROUTINE.                       EIGEN035          
C N IS THE ORDER OF THE MATRIX.                                         EIGEN036          
C NM DEFINES THE FIRST DIMENSION OF THE TWO DIMENSIONAL                 EIGEN037          
C ARRAYS A,VECR,VECI AND THE DIMENSION OF THE ONE                       EIGEN038          
C DIMENSIONAL ARRAYS EVR,EVI AND INDIC. THEREFORE THE                   EIGEN039          
C CALLING PROGRAM SHOULD CONTAIN THE FOLLOWING DECLARATION              EIGEN040          
C     DIMENSION A(NM,NN),VECR(NM,NN),VECI(NM,NN),                       EIGEN041          
C    1EVR(NM),EVI(NM),INDIC(NM)                                         EIGEN042          
C WHERE NM AND NN ARE ANY NUMBERS EQUAL TO OR GREATER THAN N            EIGEN043          
C THE UPPER LIMIT FOR NM IS EQUAL TO 100 BUT MAY BE                     EIGEN044          
C INCREASED TO THE VALUE MAX BY REPLACING THE DIMENSION                 EIGEN045          
C STATEMENT                                                             EIGEN046          
C     DIMENSION IWORK(100),LOCAL(100), ... ,WORK(100)                   EIGEN047          
C IN THE SUBROUTINE EIGENP WITH                                         EIGEN048          
C     DIMENSION IWORK(MAX),LOCAL(MAX), ... ,WORK(MAX)                   EIGEN049          
C NM AND NN ARE OF COURSE BOUNDED BY THE SIZE OF THE STORE.             EIGEN050          
C                                                                       EIGEN051          
C THE REAL VARIABLE T MUST BE SET EQUAL TO THE NUMBER OF BINARY DIGITS  EIGEN052          
C IN THE MANTISSA OF A SINGLE PRECISION FLOATING-POINT NUMBER.          EIGEN053          
C                                                                       EIGEN054          
      T=36.0                                                            EIGEN055          
C THE REAL PARTS OF THE N COMPUTED EIGENVALUES WILL BE FOUND            EIGEN056          
C IN THE FIRST N PLACES OF THE ARRAY EVR AND THE IMAGINARY              EIGEN057          
C PARTS IN THE FIRST N PLACES OF THE ARRAY EVI.                         EIGEN058          
C THE REAL COMPONENTS OF THE NORMALISED EIGENVECTOR I                   EIGEN059          
C (I=1,2,...,N) CORRESPONDING TO THE EIGENVALUE STORED IN               EIGEN060          
C EVR(I) AND EVI(I) WILL BE FOUND IN THE FIRST N PLACES OF              EIGEN061          
C THE COLUMN I OF THE TWO DIMENSIONAL ARRAY VECR AND THE                EIGEN062          
C IMAGINARY COMPONENTS IN THE FIRST N PLACES OF THE COLUMN I            EIGEN063          
C OF THE TWO DIMENSIONAL ARRAY VECI.                                    EIGEN064          
C                                                                       EIGEN065          
C THE REAL EIGENVECTOR IS NORMALISED SO THAT THE SUM OF THE             EIGEN066          
C SQUARES OF THE COMPONENTS IS EQUAL TO ONE.                            EIGEN067          
C THE COMPLEX EIGENVECTOR IS NORMALISED SO THAT THE                     EIGEN068          
C COMPONENT WITH THE LARGEST VALUE IN MODULUS HAS ITS REAL              EIGEN069          
C PART EQUAL TO ONE AND THE IMAGINARY PART EQUAL TO ZERO.               EIGEN070          
C                                                                       EIGEN071          
C THE ARRAY INDIC INDICATES THE SUCCESS OF THE SUBROUTINE               EIGEN072          
C EIGENP AS FOLLOWS                                                     EIGEN073          
C     VALUE OF INDIC(I)   EIGENVALUE I   EIGENVECTOR I                  EIGEN074          
C            0              NOT FOUND      NOT FOUND                    EIGEN075          
C            1              FOUND          NOT FOUND                    EIGEN076          
C            2              FOUND          FOUND                        EIGEN077          
C                                                                       EIGEN078          
C                                                                       EIGEN079          
      IF(N.NE.1) GOTO 110                                               EIGEN080          
      EVR(1)=A(1,1)                                                     EIGEN081          
      EVI(1)=0.0                                                        EIGEN082          
      VECR(1,1)=1.0                                                     EIGEN083          
      VECI(1,1)=0.0                                                     EIGEN084          
      INDIC(1)=2                                                        EIGEN085          
      GOTO 1330                                                         EIGEN086          
110   CALL SCALE(N,NM,A,VECI,PRFACT,ENORM)                              EIGEN087          
C THE COMPUTATION OF THE EIGENVALUES OF THE NORMALISED                  EIGEN088          
C MATRIX.                                                               EIGEN089          
      EX=EXP(-T*ALOG(2.0))                                              EIGEN090          
      CALL HESQR(N,NM,A,VECI,EVR,EVI,SUBDIA,INDIC,EPS,EX)               EIGEN091          
C                                                                       EIGEN092          
C THE POSSIBLE DECOMPOSITION OF THE UPPER-HESSENBERG MATRIX             EIGEN093          
C INTO THE SUBMATRICES OF LOWER ORDER IS INDICATED IN THE               EIGEN094          
C ARRAY LOCAL. THE DECOMPOSITION OCCURS WHEN SOME                       EIGEN095          
C SUBDIAGONAL ELEMENTS ARE IN MODULUS LESS THAN A SMALL                 EIGEN096          
C POSITIVE NUMBER EPS DEFINED IN THE SUBROUTINE HESQR . THE             EIGEN097          
C AMOUNT OF WORK IN THE EIGENVECTOR PROBLEM MAY BE                      EIGEN098          
C MINIMISED IN THIS WAY.                                                EIGEN099          
      J=N                                                               EIGEN100          
      I=1                                                               EIGEN101          
      LOCAL(1)=1                                                        EIGEN102          
      IF(J.EQ.1) GOTO 270                                               EIGEN103          
190   IF(DABS(SUBDIA(J-1)).GT.EPS) GOTO 230                             EIGEN104          
      I=I+1                                                             EIGEN105          
      LOCAL(I)=0                                                        EIGEN106          
230   J=J-1                                                             EIGEN107          
      LOCAL(I)=LOCAL(I)+1                                               EIGEN108          
      IF(J.NE.1) GOTO 190                                               EIGEN109          
C                                                                       EIGEN110          
C THE EIGENVECTOR PROBLEM.                                              EIGEN111          
270   K=1                                                               EIGEN112          
      KON=0                                                             EIGEN113          
      L=LOCAL(1)                                                        EIGEN114          
      M=N                                                               EIGEN115          
      I=1                                                               EIGEN116          
311   IF(I.GT.N) GOTO 319                                               EIGEN117          
      IVEC=N-I+1                                                        EIGEN118          
      IF(I.LE.L) GOTO 380                                               EIGEN119          
      K=K+1                                                             EIGEN120          
      M=N-L                                                             EIGEN121          
      L=L+LOCAL(K)                                                      EIGEN122          
380   IF(INDIC(IVEC).EQ.0) GOTO 570                                     EIGEN123          
      IF(EVI(IVEC).NE.0.0) GOTO 510                                     EIGEN124          
C                                                                       EIGEN125          
C TRANSFER OF AN UPPER-HESSENBERG MATRIX OF THE ORDER M FROM            EIGEN126          
C THE ARRAYS VECI AND SUBDIA INTO THE ARRAY A.                          EIGEN127          
      K1=1                                                              EIGEN128          
421   IF(K1.GT.M) GOTO 429                                              EIGEN129          
      L1=K1                                                             EIGEN130          
431   IF(L1.GT.M) GOTO 439                                              EIGEN131          
      A(K1,L1)=VECI(K1,L1)                                              EIGEN132          
      L1=L1+1                                                           EIGEN133          
      GOTO 431                                                          EIGEN134          
439   CONTINUE                                                          EIGEN135          
      IF(K1.EQ.1) GOTO 480                                              EIGEN136          
      A(K1,K1-1)=SUBDIA(K1-1)                                           EIGEN137          
480   CONTINUE                                                          EIGEN138          
C                                                                       EIGEN139          
C THE COMPUTATION OF THE REAL ENGENVECTOR IVEC OF THE UPPER-            EIGEN140          
C HESSENBERG MATRIX CORRESPONDING TO THE REAL EIGENVALUE                EIGEN141          
C EVR(IVEC).                                                            EIGEN142          
      K1=K1+1                                                           EIGEN143          
      GOTO 421                                                          EIGEN144          
429   CONTINUE                                                          EIGEN145          
      CALL REALVE(N,NM,M,IVEC,A,VECR,EVR,EVI,IWORK,WORK,INDIC,EPS,EX)   EIGEN146          
      GOTO 570                                                          EIGEN147          
C                                                                       EIGEN148          
C THE COMPUTATION OF THE COMPLEX EIGENVECTOR IVEC OF THE                EIGEN149          
C UPPER-HESSENBERG MATRIX CORRESPONDING TO THE COMPLEX                  EIGEN150          
C EIGENVALUE EVR(IVEC) + I*EVI(IVEC). IF THE VALUE OF KON IS            EIGEN151          
C NOT EQUAL TO ZERO THEN THIS COMPLEX EIGENVECTOR HAS                   EIGEN152          
C ALREADY BEEN FOUND FROM ITS CONJUGATE.                                EIGEN153          
510   IF(KON.NE.0) GOTO 560                                             EIGEN154          
      KON=1                                                             EIGEN155          
      CALL COMPVE(N,NM,M,IVEC,A,VECR,VECI,EVR,EVI,INDIC,IWORK,SUBDIA,   EIGEN156          
     .WORK1,WORK2,WORK,EPS,EX)                                          EIGEN157          
      GOTO 570                                                          EIGEN158          
560   KON=0                                                             EIGEN159          
570   CONTINUE                                                          EIGEN160          
C                                                                       EIGEN161          
C THE RECONSTRUCTION OF THE MATRIX USED IN THE REDUCTION OF             EIGEN162          
C MATRIX A TO AN UPPER-HESSENBERG FORM BY HOUSEHOLDER METHOD            EIGEN163          
      I=I+1                                                             EIGEN164          
      GOTO 311                                                          EIGEN165          
319   CONTINUE                                                          EIGEN166          
      I=1                                                               EIGEN167          
581   IF(I.GT.N) GOTO 589                                               EIGEN168          
      J=I                                                               EIGEN169          
591   IF(J.GT.N) GOTO 599                                               EIGEN170          
      A(I,J)=0.0                                                        EIGEN171          
      A(J,I)=0.0                                                        EIGEN172          
      J=J+1                                                             EIGEN173          
      GOTO 591                                                          EIGEN174          
599   CONTINUE                                                          EIGEN175          
      A(I,I)=1.0                                                        EIGEN176          
      I=I+1                                                             EIGEN177          
      GOTO 581                                                          EIGEN178          
589   CONTINUE                                                          EIGEN179          
      IF(N.LE.2) GOTO 750                                               EIGEN180          
      M=N-2                                                             EIGEN181          
      K=1                                                               EIGEN182          
661   IF(K.GT.M) GOTO 669                                               EIGEN183          
      L=K+1                                                             EIGEN184          
      J=2                                                               EIGEN185          
681   IF(J.GT.N) GOTO 689                                               EIGEN186          
      D1=0.0                                                            EIGEN187          
      I=L                                                               EIGEN188          
701   IF(I.GT.N) GOTO 709                                               EIGEN189          
      D2=VECI(I,K)                                                      EIGEN190          
      D1=D1+D2*DBLE(A(J,I))                                             EIGEN191          
      I=I+1                                                             EIGEN192          
      GOTO 701                                                          EIGEN193          
709   CONTINUE                                                          EIGEN194          
      I=L                                                               EIGEN195          
731   IF(I.GT.N) GOTO 739                                               EIGEN196          
      A(J,I)=DBLE(A(J,I))-DBLE(VECI(I,K))*D1                            EIGEN197          
C                                                                       EIGEN198          
C THE COMPUTATION OF THE EIGENVECTORS OF THE ORIGINAL NON-              EIGEN199          
C SCALED MATRIX.                                                        EIGEN200          
      I=I+1                                                             EIGEN201          
      GOTO 731                                                          EIGEN202          
739   CONTINUE                                                          EIGEN203          
      J=J+1                                                             EIGEN204          
      GOTO 681                                                          EIGEN205          
689   CONTINUE                                                          EIGEN206          
      K=K+1                                                             EIGEN207          
      GOTO 661                                                          EIGEN208          
669   CONTINUE                                                          EIGEN209          
750   KON=1                                                             EIGEN210          
      I=1                                                               EIGEN211          
761   IF(I.GT.N) GOTO 769                                               EIGEN212          
      L=0                                                               EIGEN213          
      IF(EVI(I).EQ.0.0) GOTO 850                                        EIGEN214          
      L=1                                                               EIGEN215          
      IF(KON.EQ.0) GOTO 850                                             EIGEN216          
      KON=0                                                             EIGEN217          
      GOTO 1320                                                         EIGEN218          
850   J=1                                                               EIGEN219          
851   IF(J.GT.N) GOTO 859                                               EIGEN220          
      D2=0.0                                                            EIGEN221          
      D1=D2                                                             EIGEN222          
      K=1                                                               EIGEN223          
871   IF(K.GT.N) GOTO 879                                               EIGEN224          
      D3=A(J,K)                                                         EIGEN225          
      D1=D1+D3*DBLE(VECR(K,I))                                          EIGEN226          
      IF(L.EQ.0) GOTO 930                                               EIGEN227          
      D2=D2+D3*DBLE(VECR(K,I-1))                                        EIGEN228          
930   CONTINUE                                                          EIGEN229          
      K=K+1                                                             EIGEN230          
      GOTO 871                                                          EIGEN231          
879   CONTINUE                                                          EIGEN232          
      WORK(J)=D1/PRFACT(J)                                              EIGEN233          
      IF(L.EQ.0) GOTO 980                                               EIGEN234          
      SUBDIA(J)=D2/PRFACT(J)                                            EIGEN235          
980   CONTINUE                                                          EIGEN236          
C                                                                       EIGEN237          
C THE NORMALISATION OF THE EIGENVECTORS AND THE COMPUTATION             EIGEN238          
C OF THE EIGENVALUES OF THE ORIGINAL NON-NORMALISED MATRIX.             EIGEN239          
      J=J+1                                                             EIGEN240          
      GOTO 851                                                          EIGEN241          
859   CONTINUE                                                          EIGEN242          
      IF(L.EQ.1) GOTO 1100                                              EIGEN243          
      D1=0.0                                                            EIGEN244          
      M=1                                                               EIGEN245          
1021  IF(M.GT.N) GOTO 1029                                              EIGEN246          
      D1=D1+WORK(M)**2                                                  EIGEN247          
      M=M+1                                                             EIGEN248          
      GOTO 1021                                                         EIGEN249          
1029  CONTINUE                                                          EIGEN250          
      D1=DSQRT(D1)                                                      EIGEN251          
      M=1                                                               EIGEN252          
1051  IF(M.GT.N) GOTO 1059                                              EIGEN253          
      VECI(M,I)=0.0                                                     EIGEN254          
      VECR(M,I)=WORK(M)/D1                                              EIGEN255          
      M=M+1                                                             EIGEN256          
      GOTO 1051                                                         EIGEN257          
1059  CONTINUE                                                          EIGEN258          
      EVR(I)=DBLE(EVR(I))*ENORM                                         EIGEN259          
      GOTO 1320                                                         EIGEN260          
C                                                                       EIGEN261          
1100  KON=1                                                             EIGEN262          
      EVR(I)=DBLE(EVR(I))*ENORM                                         EIGEN263          
      EVR(I-1)=EVR(I)                                                   EIGEN264          
      EVI(I)=DBLE(EVI(I))*ENORM                                         EIGEN265          
      EVI(I-1)=-EVI(I)                                                  EIGEN266          
      R=0.0                                                             EIGEN267          
      J=1                                                               EIGEN268          
1161  IF(J.GT.N) GOTO 1169                                              EIGEN269          
      R1=WORK(J)**2+SUBDIA(J)**2                                        EIGEN270          
      IF(R.GE.R1) GOTO 1220                                             EIGEN271          
      R=R1                                                              EIGEN272          
      L=J                                                               EIGEN273          
1220  CONTINUE                                                          EIGEN274          
      J=J+1                                                             EIGEN275          
      GOTO 1161                                                         EIGEN276          
1169  CONTINUE                                                          EIGEN277          
      D3=WORK(L)                                                        EIGEN278          
      R1=SUBDIA(L)                                                      EIGEN279          
      J=1                                                               EIGEN280          
1251  IF(J.GT.N) GOTO 1259                                              EIGEN281          
      D1=WORK(J)                                                        EIGEN282          
      D2=SUBDIA(J)                                                      EIGEN283          
      VECR(J,I)=(D1*D3+D2*R1)/R                                         EIGEN284          
      VECI(J,I)=(D2*D3-D1*R1)/R                                         EIGEN285          
      VECR(J,I-1)=VECR(J,I)                                             EIGEN286          
      VECI(J,I-1)=-VECI(J,I)                                            EIGEN287          
      J=J+1                                                             EIGEN288          
      GOTO 1251                                                         EIGEN289          
1259  CONTINUE                                                          EIGEN290          
1320  CONTINUE                                                          EIGEN291          
C                                                                       EIGEN292          
      I=I+1                                                             EIGEN293          
      GOTO 761                                                          EIGEN294          
769   CONTINUE                                                          EIGEN295          
1330  CONTINUE                                                          EIGEN296          
      RETURN                                                            EIGEN297          
      END                                                               EIGEN298          
      SUBROUTINE COMPVE(N,NM,M,IVEC,A,VECR,H,EVR,EVI,INDIC,IWORK,SUBDIA EIGEN299          
     .,WORK1,WORK2,WORK,EPS,EX)                                         EIGEN300          
      INTEGER I,I1,I2,ITER,IVEC,J,K,L,M,N,NM,NS                         EIGEN301          
      DOUBLEPRECISION D,D1,WORK,WORK1,WORK2,SUBDIA,B,BOUND,EPS,ETA,EX,  EIGEN302          
     .FKSI,PREVIS,R,S,U,V,DSQRT                                         EIGEN303          
      DIMENSION A(NM,1),VECR(NM,1),H(NM,1),EVR(NM),EVI(NM),INDIC(NM),   EIGEN304          
     .IWORK(NM),SUBDIA(NM),WORK1(NM),WORK2(NM),WORK(NM)                 EIGEN305          
C                                                                       EIGEN306          
C THE FOLLOWING DOUBLE PRECISION VARIABLES WERE INITIALLY SINGLE-       EIGEN307          
C SUBDIA,WORK,WORK1,WORK2,B,BOUND,EPS,ETA,EX,FKSI,PREVIS,R,S,U,V        EIGEN308          
C                                                                       EIGEN309          
C THIS SUBROUTINE FINDS THE COMPLEX EIGENVECTOR OF THE REAL             EIGEN310          
C UPPER-HESSENBERG MATRIX OF ORDER N CORRESPONDING TO THE               EIGEN311          
C COMPLEX EIGENVALUE WITH THE REAL PART IN EVR(IVEC) AND THE            EIGEN312          
C CORRESPONDING IMAGINARY PART IN EVI(IVEC). THE INVERSE                EIGEN313          
C ITERATION METHOD IS USED MODIFIED TO AVOID THE USE OF                 EIGEN314          
C COMPLEX ARITHMETIC.                                                   EIGEN315          
C THE MATRIX ON WHICH THE INVERSE ITERATION IS PERFORMED IS             EIGEN316          
C BUILT UP IN THE ARRAY A BY USING THE UPPER-HESSENBERG                 EIGEN317          
C MATRIX PRESERVED IN THE UPPER HALF OF THE ARRAY H AND IN              EIGEN318          
C THE ARRAY SUBDIA.                                                     EIGEN319          
C NM DEFINES THE FIRST DIMENSION OF THE TWO DIMENSIONAL                 EIGEN320          
C ARRAYS A,VECR AND H. NM MUST BE EQUAL TO OR GREATER                   EIGEN321          
C THAN N.                                                               EIGEN322          
C M IS THE ORDER OF THE SUBMATRIX OBTAINED BY A SUITABLE                EIGEN323          
C DECOMPOSITION OF THE UPPER-HESSENBERG MATRIX IF SOME                  EIGEN324          
C SUBDIAGONAL ELEMENTS ARE EQUAL TO ZERO. THE VALUE OF M IS             EIGEN325          
C CHOSEN SO THAT THE LAST N-M COMPONENTS OF THE COMPLEX                 EIGEN326          
C EIGENVECTOR ARE ZERO.                                                 EIGEN327          
C                                                                       EIGEN328          
C THE REAL PARTS OF THE FIRST M COMPONENTS OF THE COMPUTED              EIGEN329          
C COMPLEX EIGENVECTOR WILL BE FOUND IN THE FIRST M PLACES OF            EIGEN330          
C THE COLUMN WHOSE TOP ELEMENT IS VECR(1,IVEC) AND THE                  EIGEN331          
C CORRESPONDING IMAGINARY PARTS OF THE FIRST M COMPONENTS OF            EIGEN332          
C THE COMPLEX EIGENVECTOR WILL BE FOUND IN THE FIRST M                  EIGEN333          
C PLACES OF THE COLUMN WHOSE TOP ELEMENT IS VECR(1,IVEC-1).             EIGEN334          
C                                                                       EIGEN335          
C THE ARRAY INDIC INDICATES THE SUCCESS OF THE ROUTINE AS               EIGEN336          
C FOLLOWS                                                               EIGEN337          
C     VALUE OF INDIC(I)   EIGENVECTOR                                   EIGEN338          
C            1              NOT FOUND                                   EIGEN339          
C            2                FOUND                                     EIGEN340          
C THE ARRAYS IWORK,WORK1,WORK2 AND WORK ARE THE WORKING                 EIGEN341          
C STORES USED DURING THE INVERSE ITERATION PROCESS.                     EIGEN342          
C EPS IS A SMALL POSITIVE NUMBER THAT NUMERICALLY REPRESENTS            EIGEN343          
C ZERO IN THE PROGRAM. EPS = (EUCLIDIAN NORM OF H)*EX, WHERE            EIGEN344          
C EX = 2**(-T). T IS THE NUMBER OF BINARY DIGITS IN THE                 EIGEN345          
C MANTISSA OF A FLOATING POINT NUMBER.                                  EIGEN346          
C                                                                       EIGEN347          
      FKSI=EVR(IVEC)                                                    EIGEN348          
      ETA=EVI(IVEC)                                                     EIGEN349          
C THE MODIFICATION OF THE EIGENVALUE (FKSI + I*ETA) IF MORE             EIGEN350          
C EIGENVALUES ARE EQUAL.                                                EIGEN351          
      IF(IVEC.EQ.M) GOTO 180                                            EIGEN352          
      K=IVEC+1                                                          EIGEN353          
      R=0.0                                                             EIGEN354          
      I=K                                                               EIGEN355          
81    IF(I.GT.M) GOTO 89                                                EIGEN356          
      IF(FKSI.NE.DBLE(EVR(I))) GOTO 140                                 EIGEN357          
      IF(ABS(ETA).NE.ABS(EVI(I))) GOTO 140                              EIGEN358          
      R=R+3.0D0                                                         EIGEN359          
140   CONTINUE                                                          EIGEN360          
      I=I+1                                                             EIGEN361          
      GOTO 81                                                           EIGEN362          
89    CONTINUE                                                          EIGEN363          
      R=R*EX                                                            EIGEN364          
      FKSI=FKSI+R                                                       EIGEN365          
      ETA=ETA+R                                                         EIGEN366          
C                                                                       EIGEN367          
C THE MATRIX  ((H-FKSI*I)*(H-FKSI*I) + (ETA*ETA)*I) IS                  EIGEN368          
C STORED INTO THE ARRAY A.                                              EIGEN369          
180   R=FKSI*FKSI+ETA*ETA                                               EIGEN370          
      S=2.0D0*FKSI                                                      EIGEN371          
      L=M-1                                                             EIGEN372          
      I=1                                                               EIGEN373          
211   IF(I.GT.M) GOTO 219                                               EIGEN374          
      J=I                                                               EIGEN375          
221   IF(J.GT.M) GOTO 229                                               EIGEN376          
      D=0.0                                                             EIGEN377          
      A(J,I)=0.0                                                        EIGEN378          
      K=I                                                               EIGEN379          
251   IF(K.GT.J) GOTO 259                                               EIGEN380          
      D=D+DBLE(H(I,K))*DBLE(H(K,J))                                     EIGEN381          
      K=K+1                                                             EIGEN382          
      GOTO 251                                                          EIGEN383          
259   CONTINUE                                                          EIGEN384          
      A(I,J)=D-S*DBLE(H(I,J))                                           EIGEN385          
      J=J+1                                                             EIGEN386          
      GOTO 221                                                          EIGEN387          
229   CONTINUE                                                          EIGEN388          
      A(I,I)=DBLE(A(I,I))+R                                             EIGEN389          
      I=I+1                                                             EIGEN390          
      GOTO 211                                                          EIGEN391          
219   CONTINUE                                                          EIGEN392          
      I=1                                                               EIGEN393          
291   IF(I.GT.L) GOTO 299                                               EIGEN394          
      R=SUBDIA(I)                                                       EIGEN395          
      A(I+1,I)=-S*R                                                     EIGEN396          
      I1=I+1                                                            EIGEN397          
      J=1                                                               EIGEN398          
331   IF(J.GT.I1) GOTO 339                                              EIGEN399          
      A(J,I)=DBLE(A(J,I))+R*DBLE(H(J,I+1))                              EIGEN400          
      J=J+1                                                             EIGEN401          
      GOTO 331                                                          EIGEN402          
339   CONTINUE                                                          EIGEN403          
      IF(I.EQ.1) GOTO 380                                               EIGEN404          
      A(I+1,I-1)=R*SUBDIA(I-1)                                          EIGEN405          
380   J=I                                                               EIGEN406          
381   IF(J.GT.M) GOTO 389                                               EIGEN407          
      A(I+1,J)=DBLE(A(I+1,J))+R*DBLE(H(I,J))                            EIGEN408          
      J=J+1                                                             EIGEN409          
      GOTO 381                                                          EIGEN410          
389   CONTINUE                                                          EIGEN411          
C                                                                       EIGEN412          
C THE GUASSIAN ELIMINATION OF THE MATRIX                                EIGEN413          
C ((H-FKSI*I)*I)*(H-FKSI*I) + (ETA*ETA)*I) IN THE ARRAY A. THE          EIGEN414          
C ROW INTERCHANGES THAT OCCUR ARE INDICATED IN THE ARRAY                EIGEN415          
C IWORK. ALL THE MULTIPLIERS ARE STORED IN THE FIRST AND IN             EIGEN416          
C THE SECOND SUBDIAGONAL OF THE ARRAY A.                                EIGEN417          
      I=I+1                                                             EIGEN418          
      GOTO 291                                                          EIGEN419          
299   CONTINUE                                                          EIGEN420          
      K=M-1                                                             EIGEN421          
      I=1                                                               EIGEN422          
421   IF(I.GT.K) GOTO 429                                               EIGEN423          
      I1=I+1                                                            EIGEN424          
      I2=I+2                                                            EIGEN425          
      IWORK(I)=0                                                        EIGEN426          
      IF(I.EQ.K) GOTO 500                                               EIGEN427          
      IF(A(I+2,I).NE.0.0) GOTO 560                                      EIGEN428          
500   IF(A(I+1,I).NE.0.0) GOTO 560                                      EIGEN429          
      IF(A(I,I).NE.0.0) GOTO 810                                        EIGEN430          
      A(I,I)=EPS                                                        EIGEN431          
      GOTO 810                                                          EIGEN432          
C                                                                       EIGEN433          
560   IF(I.EQ.K) GOTO 650                                               EIGEN434          
      IF(ABS(A(I+1,I)).GE.ABS(A(I+2,I))) GOTO 650                       EIGEN435          
      IF(ABS(A(I,I)).GE.ABS(A(I+2,I))) GOTO 760                         EIGEN436          
      L=I+2                                                             EIGEN437          
      IWORK(I)=2                                                        EIGEN438          
      GOTO 690                                                          EIGEN439          
650   IF(ABS(A(I,I)).GE.ABS(A(I+1,I))) GOTO 730                         EIGEN440          
      L=I+1                                                             EIGEN441          
      IWORK(I)=1                                                        EIGEN442          
C                                                                       EIGEN443          
690   J=I                                                               EIGEN444          
691   IF(J.GT.M) GOTO 699                                               EIGEN445          
      R=A(I,J)                                                          EIGEN446          
      A(I,J)=A(L,J)                                                     EIGEN447          
      A(L,J)=R                                                          EIGEN448          
      J=J+1                                                             EIGEN449          
      GOTO 691                                                          EIGEN450          
699   CONTINUE                                                          EIGEN451          
730   IF(I.NE.K) GOTO 760                                               EIGEN452          
      I2=I1                                                             EIGEN453          
760   L=I1                                                              EIGEN454          
761   IF(L.GT.I2) GOTO 769                                              EIGEN455          
      R=-A(L,I)/A(I,I)                                                  EIGEN456          
      A(L,I)=R                                                          EIGEN457          
      J=I1                                                              EIGEN458          
791   IF(J.GT.M) GOTO 799                                               EIGEN459          
      A(L,J)=DBLE(A(L,J))+R*DBLE(A(I,J))                                EIGEN460          
      J=J+1                                                             EIGEN461          
      GOTO 791                                                          EIGEN462          
799   CONTINUE                                                          EIGEN463          
      L=L+1                                                             EIGEN464          
      GOTO 761                                                          EIGEN465          
769   CONTINUE                                                          EIGEN466          
810   CONTINUE                                                          EIGEN467          
      I=I+1                                                             EIGEN468          
      GOTO 421                                                          EIGEN469          
429   CONTINUE                                                          EIGEN470          
      IF(A(M,M).NE.0.0) GOTO 850                                        EIGEN471          
      A(M,M)=EPS                                                        EIGEN472          
C                                                                       EIGEN473          
C THE VECTOR (1,1,...,1) IS STORED INTO THE RIGHT-HAND SIDE             EIGEN474          
C VECTORS VECR( ,IVEC) AND VECR( ,IVEC-1) REPRESENTING THE              EIGEN475          
C COMPLEX RIGHT-HAND SIDE VECTOR.                                       EIGEN476          
850   I=1                                                               EIGEN477          
851   IF(I.GT.N) GOTO 859                                               EIGEN478          
      IF(I.GT.M) GOTO 910                                               EIGEN479          
      VECR(I,IVEC)=1.0                                                  EIGEN480          
      VECR(I,IVEC-1)=1.0                                                EIGEN481          
      GOTO 930                                                          EIGEN482          
910   VECR(I,IVEC)=0.0                                                  EIGEN483          
      VECR(I,IVEC-1)=0.0                                                EIGEN484          
930   CONTINUE                                                          EIGEN485          
C                                                                       EIGEN486          
C THE INVERSE ITERATION IS PERFORMED ON THE MATRIX UNTIL THE            EIGEN487          
C INFINITE NORM OF THE RIGHT-HAND SIDE VECTOR IS GREATER                EIGEN488          
C THAN THE BOUND DEFINED AS 0.01/(N*EX).                                EIGEN489          
      I=I+1                                                             EIGEN490          
      GOTO 851                                                          EIGEN491          
859   CONTINUE                                                          EIGEN492          
      BOUND=0.01D0/(EX*DBLE(FLOAT(N)))                                  EIGEN493          
      NS=0                                                              EIGEN494          
      ITER=1                                                            EIGEN495          
      I=1                                                               EIGEN496          
971   IF(I.GT.M) GOTO 979                                               EIGEN497          
      WORK(I)=DBLE(H(I,I))-FKSI                                         EIGEN498          
C                                                                       EIGEN499          
C THE SEQUENCE OF THE COMPLEX VECTORS Z(S) = P(S)+I*Q(S) AND            EIGEN500          
C W(S+1)= U(S+1)+I*V(S+1) IS GIVEN BY THE RELATIONS                     EIGEN501          
C (A - (FKSI-I*ETA)*I)*W(S+1) = Z(S) AND                                EIGEN502          
C Z(S+1) = S(S+1)/MAX(W(S+1)).                                          EIGEN503          
C THE FINAL W(S) IS TAKEN AS THE COMPUTED EIGENVECTOR.                  EIGEN504          
C                                                                       EIGEN505          
C THE COMPUTATION OF THE RIGHT-HAND SIDE VECTOR                         EIGEN506          
C (A-FKSI*I)*P(S)-ETA*Q(S). A IS AN UPPER-HESSENBERG MATRIX.            EIGEN507          
      I=I+1                                                             EIGEN508          
      GOTO 971                                                          EIGEN509          
979   CONTINUE                                                          EIGEN510          
990   I=1                                                               EIGEN511          
991   IF(I.GT.M) GOTO 999                                               EIGEN512          
      D=WORK(I)*DBLE(VECR(I,IVEC))                                      EIGEN513          
      IF(I.EQ.1) GOTO 1040                                              EIGEN514          
      D=D+SUBDIA(I-1)*DBLE(VECR(I-1,IVEC))                              EIGEN515          
1040  L=I+1                                                             EIGEN516          
      IF(L.GT.M) GOTO 1090                                              EIGEN517          
      K=L                                                               EIGEN518          
1071  IF(K.GT.M) GOTO 1079                                              EIGEN519          
      D=D+DBLE(H(I,K))*DBLE(VECR(K,IVEC))                               EIGEN520          
      K=K+1                                                             EIGEN521          
      GOTO 1071                                                         EIGEN522          
1079  CONTINUE                                                          EIGEN523          
1090  VECR(I,IVEC-1)=D-ETA*DBLE(VECR(I,IVEC-1))                         EIGEN524          
C                                                                       EIGEN525          
C GAUSSIAN ELIMINATION OF THE RIGHT-HAND SIDE VECTOR.                   EIGEN526          
      I=I+1                                                             EIGEN527          
      GOTO 991                                                          EIGEN528          
999   CONTINUE                                                          EIGEN529          
      K=M-1                                                             EIGEN530          
      I=1                                                               EIGEN531          
1121  IF(I.GT.K) GOTO 1129                                              EIGEN532          
      L=I+IWORK(I)                                                      EIGEN533          
      R=VECR(L,IVEC-1)                                                  EIGEN534          
      VECR(L,IVEC-1)=VECR(I,IVEC-1)                                     EIGEN535          
      VECR(I,IVEC-1)=R                                                  EIGEN536          
      VECR(I+1,IVEC-1)=DBLE(VECR(I+1,IVEC-1))+DBLE(A(I+1,I))*R          EIGEN537          
      IF(I.EQ.K) GOTO 1210                                              EIGEN538          
      VECR(I+2,IVEC-1)=DBLE(VECR(I+2,IVEC-1))+DBLE(A(I+2,I))*R          EIGEN539          
1210  CONTINUE                                                          EIGEN540          
C                                                                       EIGEN541          
C THE COMPUTATION OF THE REAL PART U(S+1) OF THE COMPLEX                EIGEN542          
C VECTOR W(S+1). THE VECTOR U(S+1) IS OBTAINED AFTER THE                EIGEN543          
C BACKSUBSTITUTION.                                                     EIGEN544          
      I=I+1                                                             EIGEN545          
      GOTO 1121                                                         EIGEN546          
1129  CONTINUE                                                          EIGEN547          
      I=1                                                               EIGEN548          
1221  IF(I.GT.M) GOTO 1229                                              EIGEN549          
      J=M-I+1                                                           EIGEN550          
      D=VECR(J,IVEC-1)                                                  EIGEN551          
      IF(J.EQ.M) GOTO 1310                                              EIGEN552          
      L=J+1                                                             EIGEN553          
      K=L                                                               EIGEN554          
1281  IF(K.GT.M) GOTO 1289                                              EIGEN555          
      D1=A(J,K)                                                         EIGEN556          
      D=D-D1*DBLE(VECR(K,IVEC-1))                                       EIGEN557          
      K=K+1                                                             EIGEN558          
      GOTO 1281                                                         EIGEN559          
1289  CONTINUE                                                          EIGEN560          
1310  VECR(J,IVEC-1)=D/DBLE(A(J,J))                                     EIGEN561          
C                                                                       EIGEN562          
C THE COMPUTATION OF THE IMAGINARY PART V(S+1) OF THE VECTOR            EIGEN563          
C W(S+1), WHERE V(S+1) = (P(S)-(A-FKSI*I)*U(S+1))/ETA.                  EIGEN564          
      I=I+1                                                             EIGEN565          
      GOTO 1221                                                         EIGEN566          
1229  CONTINUE                                                          EIGEN567          
      I=1                                                               EIGEN568          
1331  IF(I.GT.M) GOTO 1339                                              EIGEN569          
      D=WORK(I)*DBLE(VECR(I,IVEC-1))                                    EIGEN570          
      IF(I.EQ.1) GOTO 1380                                              EIGEN571          
      D=D+SUBDIA(I-1)*DBLE(VECR(I-1,IVEC-1))                            EIGEN572          
1380  L=I+1                                                             EIGEN573          
      IF(L.GT.M) GOTO 1430                                              EIGEN574          
      K=L                                                               EIGEN575          
1411  IF(K.GT.M) GOTO 1419                                              EIGEN576          
      D=D+DBLE(H(I,K))*DBLE(VECR(K,IVEC-1))                             EIGEN577          
      K=K+1                                                             EIGEN578          
      GOTO 1411                                                         EIGEN579          
1419  CONTINUE                                                          EIGEN580          
1430  VECR(I,IVEC)=(DBLE(VECR(I,IVEC))-D)/ETA                           EIGEN581          
C                                                                       EIGEN582          
C THE COMPUTATION OF  (INFIN. NORM OF W(S+1))**2 .                      EIGEN583          
      I=I+1                                                             EIGEN584          
      GOTO 1331                                                         EIGEN585          
1339  CONTINUE                                                          EIGEN586          
      L=1                                                               EIGEN587          
      S=0.0                                                             EIGEN588          
      I=1                                                               EIGEN589          
1471  IF(I.GT.M) GOTO 1479                                              EIGEN590          
      R=VECR(I,IVEC)**2+VECR(I,IVEC-1)**2                               EIGEN591          
      IF(R.LE.S) GOTO 1530                                              EIGEN592          
      S=R                                                               EIGEN593          
      L=I                                                               EIGEN594          
1530  CONTINUE                                                          EIGEN595          
C THE COMPUTATION OF THE VECTOR Z(S+1),WHERE Z(S+1)= W(S+1)/            EIGEN596          
C (COMPONENT OF W(S+1) WITH THE LARGEST ABSOLUTE VALUE) .               EIGEN597          
      I=I+1                                                             EIGEN598          
      GOTO 1471                                                         EIGEN599          
1479  CONTINUE                                                          EIGEN600          
      U=VECR(L,IVEC-1)                                                  EIGEN601          
      V=VECR(L,IVEC)                                                    EIGEN602          
      I=1                                                               EIGEN603          
1561  IF(I.GT.M) GOTO 1569                                              EIGEN604          
      B=VECR(I,IVEC)                                                    EIGEN605          
      R=VECR(I,IVEC-1)                                                  EIGEN606          
      VECR(I,IVEC)=(R*U+B*V)/S                                          EIGEN607          
      VECR(I,IVEC-1)=(B*U-R*V)/S                                        EIGEN608          
C THE COMPUTATION OF THE RESIDUALS AND COMPARISON OF THE                EIGEN609          
C RESIDUALS OF THE TWO SUCCESSIVE STEPS OF THE INVERSE                  EIGEN610          
C ITERATION. IF THE INFINITE NORM OF THE RESIDUAL VECTOR IS             EIGEN611          
C GREATER THAN THE INFINITE NORM OF THE PREVIOUS RESIDUAL               EIGEN612          
C VECTOR THE COMPUTED VECTOR OF THE PREVIOUS STEP IS TAKEN              EIGEN613          
C AS THE COMPUTED APPROXIMATION TO THE EIGENVECTOR.                     EIGEN614          
      I=I+1                                                             EIGEN615          
      GOTO 1561                                                         EIGEN616          
1569  CONTINUE                                                          EIGEN617          
      B=0.0                                                             EIGEN618          
      I=1                                                               EIGEN619          
1621  IF(I.GT.M) GOTO 1629                                              EIGEN620          
      R=WORK(I)*DBLE(VECR(I,IVEC-1))-ETA*DBLE(VECR(I,IVEC))             EIGEN621          
      U=WORK(I)*DBLE(VECR(I,IVEC))+ETA*DBLE(VECR(I,IVEC-1))             EIGEN622          
      IF(I.EQ.1) GOTO 1690                                              EIGEN623          
      R=R+SUBDIA(I-1)*DBLE(VECR(I-1,IVEC-1))                            EIGEN624          
      U=U+SUBDIA(I-1)*DBLE(VECR(I-1,IVEC-1))                            EIGEN625          
1690  L=I+1                                                             EIGEN626          
      IF(L.GT.M) GOTO 1750                                              EIGEN627          
      J=L                                                               EIGEN628          
1721  IF(J.GT.M) GOTO 1729                                              EIGEN629          
      R=R+DBLE(H(I,J))*DBLE(VECR(J,IVEC-1))                             EIGEN630          
      U=U+DBLE(H(I,J))*DBLE(VECR(J,IVEC))                               EIGEN631          
      J=J+1                                                             EIGEN632          
      GOTO 1721                                                         EIGEN633          
1729  CONTINUE                                                          EIGEN634          
1750  U=R*R+U*U                                                         EIGEN635          
      IF(B.GE.U) GOTO 1790                                              EIGEN636          
      B=U                                                               EIGEN637          
1790  CONTINUE                                                          EIGEN638          
      I=I+1                                                             EIGEN639          
      GOTO 1621                                                         EIGEN640          
1629  CONTINUE                                                          EIGEN641          
      IF(ITER.EQ.1) GOTO 1840                                           EIGEN642          
      IF(PREVIS.LE.B) GOTO 1970                                         EIGEN643          
1840  I=1                                                               EIGEN644          
1841  IF(I.GT.N) GOTO 1849                                              EIGEN645          
      WORK1(I)=VECR(I,IVEC)                                             EIGEN646          
      WORK2(I)=VECR(I,IVEC-1)                                           EIGEN647          
      I=I+1                                                             EIGEN648          
      GOTO 1841                                                         EIGEN649          
1849  CONTINUE                                                          EIGEN650          
      PREVIS=B                                                          EIGEN651          
      IF(NS.EQ.1) GOTO 2000                                             EIGEN652          
      IF(ITER.GT.6) GOTO 2020                                           EIGEN653          
      ITER=ITER+1                                                       EIGEN654          
      IF(BOUND.GT.DSQRT(S)) GOTO 990                                    EIGEN655          
      NS=1                                                              EIGEN656          
      GOTO 990                                                          EIGEN657          
C                                                                       EIGEN658          
1970  I=1                                                               EIGEN659          
1971  IF(I.GT.N) GOTO 1979                                              EIGEN660          
      VECR(I,IVEC)=WORK1(I)                                             EIGEN661          
      VECR(I,IVEC-1)=WORK2(I)                                           EIGEN662          
      I=I+1                                                             EIGEN663          
      GOTO 1971                                                         EIGEN664          
1979  CONTINUE                                                          EIGEN665          
2000  INDIC(IVEC-1)=2                                                   EIGEN666          
      INDIC(IVEC)=2                                                     EIGEN667          
2020  CONTINUE                                                          EIGEN668          
      RETURN                                                            EIGEN669          
      END                                                               EIGEN670          
      SUBROUTINE SCALE(N,NM,A,H,PRFACT,ENORM)                           EIGEN671          
      INTEGER I,J,ITER,N,NCOUNT,NM                                      EIGEN672          
      DOUBLEPRECISION COLUMN,FACTOR,FNORM,PRFACT,Q,ROW,BOUND1,BOUND2,   EIGEN673          
     .ENORM,DSQRT                                                       EIGEN674          
      DIMENSION A(NM,1),H(NM,1),PRFACT(NM)                              EIGEN675          
C                                                                       EIGEN676          
C THE FOLLOWING DOUBLE PRECISION VARIABLES WERE INITIALLY SINGLE PREC.- EIGEN677          
C BOUND1,BOUND2,ENORM                                                   EIGEN678          
C                                                                       EIGEN679          
C                                                                       EIGEN680          
C THIS SUBROUTINE STORES THE MATRIX OF THE ORDER N FROM THE             EIGEN681          
C ARRAY A INTO THE ARRAY H. AFTERWARD THE MATRIX IN THE                 EIGEN682          
C ARRAY A IS SCALED SO THAT THE QUOTIENT OF THE ABSOLUTE SUM            EIGEN683          
C OF THE OFF-DIAGONAL ELEMENTS OF COLUMN I AND THE ABSOLUTE             EIGEN684          
C SUM OF THE OFF-DIAGONAL ELEMENTS OF ROW I LIES WITHIN THE             EIGEN685          
C VALUES OF BOUND1 AND BOUND2.                                          EIGEN686          
C THE COMPONENT I OF THE EIGENVECTOR OBTAINED BY USING THE              EIGEN687          
C SCALED MATRIX MUST BE DIVIDED BY THE VALUE FOUND IN THE               EIGEN688          
C PRFACT(I) OF THE ARRAY PRFACT. IN THIS WAY THE EIGENVECTOR            EIGEN689          
C OF THE NON-SCALED MATRIX IS OBTAINED.                                 EIGEN690          
C                                                                       EIGEN691          
C AFTER THE MATRIX IS SCALED IT IS NORMALISED SO THAT THE               EIGEN692          
C VALUE OF THE EUCLIDIAN NORM IS EQUAL TO ONE.                          EIGEN693          
C IF THE PROCESS OF SCALING WAS NOT SUCCESSFUL THE ORIGINAL             EIGEN694          
C MATRIX FROM THE ARRAY H WOULD BE STORED BACK INTO A AND               EIGEN695          
C THE EIGENPROBLEM WOULD BE SOLVED BY USING THIS MATRIX.                EIGEN696          
C NM DEFINES THE FIRST DIMENSION OF THE ARRAYS A AND H. NM              EIGEN697          
C MUST BE GREATER OR EQUAL TO N.                                        EIGEN698          
C THE EIGENVALUES OF THE NORMALISED MATRIX MUST BE                      EIGEN699          
C MULTIPLIED BY THE SCALAR ENORM IN ORDER THAT THEY BECOME              EIGEN700          
C THE EIGENVALUES OF THE NON-NORMALISED MATRIX.                         EIGEN701          
C                                                                       EIGEN702          
      I=1                                                               EIGEN703          
21    IF(I.GT.N) GOTO 29                                                EIGEN704          
      J=1                                                               EIGEN705          
31    IF(J.GT.N) GOTO 39                                                EIGEN706          
      H(I,J)=A(I,J)                                                     EIGEN707          
      J=J+1                                                             EIGEN708          
      GOTO 31                                                           EIGEN709          
39    CONTINUE                                                          EIGEN710          
      PRFACT(I)=1.0                                                     EIGEN711          
      I=I+1                                                             EIGEN712          
      GOTO 21                                                           EIGEN713          
29    CONTINUE                                                          EIGEN714          
      BOUND1=0.75                                                       EIGEN715          
      BOUND2=1.33                                                       EIGEN716          
      ITER=0                                                            EIGEN717          
90    NCOUNT=0                                                          EIGEN718          
      I=1                                                               EIGEN719          
101   IF(I.GT.N) GOTO 109                                               EIGEN720          
      ROW=0.0                                                           EIGEN721          
      COLUMN=ROW                                                        EIGEN722          
      J=1                                                               EIGEN723          
121   IF(J.GT.N) GOTO 129                                               EIGEN724          
      IF(I.EQ.J) GOTO 170                                               EIGEN725          
      COLUMN=COLUMN+DBLE(ABS(A(J,I)))                                   EIGEN726          
      ROW=ROW+DBLE(ABS(A(I,J)))                                         EIGEN727          
170   CONTINUE                                                          EIGEN728          
      J=J+1                                                             EIGEN729          
      GOTO 121                                                          EIGEN730          
129   CONTINUE                                                          EIGEN731          
      IF(COLUMN.EQ.0.0D0) GOTO 270                                      EIGEN732          
      IF(ROW.EQ.0.0D0) GOTO 270                                         EIGEN733          
      Q=COLUMN/ROW                                                      EIGEN734          
      IF(Q.LT.BOUND1) GOTO 290                                          EIGEN735          
      IF(Q.GT.BOUND2) GOTO 290                                          EIGEN736          
270   NCOUNT=NCOUNT+1                                                   EIGEN737          
      GOTO 370                                                          EIGEN738          
290   FACTOR=DSQRT(Q)                                                   EIGEN739          
      J=1                                                               EIGEN740          
301   IF(J.GT.N) GOTO 309                                               EIGEN741          
      IF(I.EQ.J) GOTO 350                                               EIGEN742          
      A(I,J)=DBLE(A(I,J))*FACTOR                                        EIGEN743          
      A(J,I)=DBLE(A(J,I))/FACTOR                                        EIGEN744          
350   CONTINUE                                                          EIGEN745          
      J=J+1                                                             EIGEN746          
      GOTO 301                                                          EIGEN747          
309   CONTINUE                                                          EIGEN748          
      PRFACT(I)=PRFACT(I)*FACTOR                                        EIGEN749          
370   CONTINUE                                                          EIGEN750          
      I=I+1                                                             EIGEN751          
      GOTO 101                                                          EIGEN752          
109   CONTINUE                                                          EIGEN753          
      ITER=ITER+1                                                       EIGEN754          
      IF(ITER.GT.30) GOTO 540                                           EIGEN755          
      IF(NCOUNT.LT.N) GOTO 90                                           EIGEN756          
C                                                                       EIGEN757          
      FNORM=0.0                                                         EIGEN758          
      I=1                                                               EIGEN759          
441   IF(I.GT.N) GOTO 449                                               EIGEN760          
      J=1                                                               EIGEN761          
451   IF(J.GT.N) GOTO 459                                               EIGEN762          
      Q=A(I,J)                                                          EIGEN763          
      FNORM=FNORM+Q*Q                                                   EIGEN764          
      J=J+1                                                             EIGEN765          
      GOTO 451                                                          EIGEN766          
459   CONTINUE                                                          EIGEN767          
      I=I+1                                                             EIGEN768          
      GOTO 441                                                          EIGEN769          
449   CONTINUE                                                          EIGEN770          
      FNORM=DSQRT(FNORM)                                                EIGEN771          
      I=1                                                               EIGEN772          
491   IF(I.GT.N) GOTO 499                                               EIGEN773          
      J=1                                                               EIGEN774          
501   IF(J.GT.N) GOTO 509                                               EIGEN775          
      A(I,J)=DBLE(A(I,J))/FNORM                                         EIGEN776          
      J=J+1                                                             EIGEN777          
      GOTO 501                                                          EIGEN778          
509   CONTINUE                                                          EIGEN779          
      I=I+1                                                             EIGEN780          
      GOTO 491                                                          EIGEN781          
499   CONTINUE                                                          EIGEN782          
      ENORM=FNORM                                                       EIGEN783          
      GOTO 590                                                          EIGEN784          
C                                                                       EIGEN785          
540   I=1                                                               EIGEN786          
541   IF(I.GT.N) GOTO 549                                               EIGEN787          
C                                                                       EIGEN788          
C MODIFICATION SUGGESTED BY REFEREE IN A.C.M.CERTIFICATION              EIGEN789          
C                                                                       EIGEN790          
      PRFACT(I)=1.0                                                     EIGEN791          
      J=1                                                               EIGEN792          
561   IF(J.GT.N) GOTO 569                                               EIGEN793          
      A(I,J)=H(I,J)                                                     EIGEN794          
      J=J+1                                                             EIGEN795          
      GOTO 561                                                          EIGEN796          
569   CONTINUE                                                          EIGEN797          
      I=I+1                                                             EIGEN798          
      GOTO 541                                                          EIGEN799          
549   CONTINUE                                                          EIGEN800          
      ENORM=1.0                                                         EIGEN801          
C                                                                       EIGEN802          
590   CONTINUE                                                          EIGEN803          
      RETURN                                                            EIGEN804          
      END                                                               EIGEN805          
      SUBROUTINE HESQR(N,NM,A,H,EVR,EVI,SUBDIA,INDIC,EPS,EX)            EIGEN806          
      REAL T                                                            EIGEN807          
      INTEGER I,J,K,L,M,MAXST,M1,N,NM,NS                                EIGEN808          
      DOUBLEPRECISION S,SR,SR2,X,Y,Z,SUBDIA,EPS,EX,R,SHIFT,DSQRT        EIGEN809          
      DIMENSION A(NM,1),H(NM,1),EVR(NM),EVI(NM),SUBDIA(NM),INDIC(NM)    EIGEN810          
C                                                                       EIGEN811          
C THE FOLLOWING DOUBLE PRECISION VARIABLES WERE INITIALLY SINGLE PREC.- EIGEN812          
C SUBDIA, EPS, EX, R, SHIFT                                             EIGEN813          
C                                                                       EIGEN814          
C THIS SUBROUTINE FINDS ALL THE EIGENVALUES OF A REAL                   EIGEN815          
C GENERAL MATRIX. THE ORIGINAL MATRIX A OF ORDER N IS                   EIGEN816          
C REDUCED TO THE UPPER-HESSENBERG FORM H BY MEANS OF                    EIGEN817          
C SIMILARITY TRANSFORMATIONS(HOUSEHOLDER METHOD). THE MATRIX            EIGEN818          
C H IS PRESERVED IN THE UPPER HALF OF THE ARRAY H AND IN THE            EIGEN819          
C ARRAY SUBDIA.  THE SPECIAL VECTORS USED IN THE DEFINITION             EIGEN820          
C OF THE HOUSEHOLDER TRANSFORMATION MATRICES ARE STORED IN              EIGEN821          
C THE LOWER PART OF THE ARRAY H.                                        EIGEN822          
C NM IS THE FIRST DIMENSION OF THE ARRAYS A AND H. NM MUST              EIGEN823          
C BE EQUAL TO OR GREATER THAN N.                                        EIGEN824          
C THE REAL PARTS OF THE N EIGENVALUES WILL BE FOUND IN THE              EIGEN825          
C FIRST N PLACES OF THE ARRAY EVR,AND                                   EIGEN826          
C THE IMAGINARY PARTS IN THE FIRST N PLACES OF THE ARRAY EVI            EIGEN827          
C THE ARRAY INDIC INDICATES THE SUCCESS OF THE ROUTINE AS               EIGEN828          
C FOLLOWS                                                               EIGEN829          
C     VALUE OF INDIC(I)  EIGENVALUE I                                   EIGEN830          
C            0             NOT FOUND                                    EIGEN831          
C            1               FOUND                                      EIGEN832          
C EPS IS A SMALL POSITIVE NUMBER THAT NUMERICALLY REPRESENTS            EIGEN833          
C ZERO IN THE PROGRAM. EPS = (EUCLIDIAN NORM OF H)*EX ,WHERE            EIGEN834          
C EX = 2**(-T). T IS THE NUMBER OF BINARY DIGITS IN THE                 EIGEN835          
C MANTISSA OF A FLOATING POINT NUMBER.                                  EIGEN836          
C                                                                       EIGEN837          
C                                                                       EIGEN838          
C                                                                       EIGEN839          
C REDUCTION OF THE MATRIX A TO AN UPPER-HESSENBERG FORM H.              EIGEN840          
C THERE ARE N-2 STEPS.                                                  EIGEN841          
      IF(N-2) 510,30,50                                                 EIGEN842          
30    SUBDIA(1)=A(2,1)                                                  EIGEN843          
      GOTO 510                                                          EIGEN844          
50    M=N-2                                                             EIGEN845          
      K=1                                                               EIGEN846          
61    IF(K.GT.M) GOTO 69                                                EIGEN847          
      L=K+1                                                             EIGEN848          
      S=0.0                                                             EIGEN849          
      I=L                                                               EIGEN850          
91    IF(I.GT.N) GOTO 99                                                EIGEN851          
      H(I,K)=A(I,K)                                                     EIGEN852          
      S=S+DBLE(ABS(A(I,K)))                                             EIGEN853          
      I=I+1                                                             EIGEN854          
      GOTO 91                                                           EIGEN855          
99    CONTINUE                                                          EIGEN856          
      IF(S.NE.DBLE(ABS(A(K+1,K)))) GOTO 170                             EIGEN857          
      SUBDIA(K)=A(K+1,K)                                                EIGEN858          
      H(K+1,K)=0.0                                                      EIGEN859          
      GOTO 470                                                          EIGEN860          
170   SR2=0.0                                                           EIGEN861          
      I=L                                                               EIGEN862          
181   IF(I.GT.N) GOTO 189                                               EIGEN863          
      SR=A(I,K)                                                         EIGEN864          
      SR=SR/S                                                           EIGEN865          
      A(I,K)=SR                                                         EIGEN866          
      SR2=SR2+SR*SR                                                     EIGEN867          
      I=I+1                                                             EIGEN868          
      GOTO 181                                                          EIGEN869          
189   CONTINUE                                                          EIGEN870          
      SR=DSQRT(SR2)                                                     EIGEN871          
      IF(A(L,K).LT.0.0) GOTO 270                                        EIGEN872          
      SR=-SR                                                            EIGEN873          
270   SR2=SR2-SR*DBLE(A(L,K))                                           EIGEN874          
      A(L,K)=DBLE(A(L,K))-SR                                            EIGEN875          
      H(L,K)=DBLE(H(L,K))-SR*S                                          EIGEN876          
      SUBDIA(K)=SR*S                                                    EIGEN877          
      X=S*DSQRT(SR2)                                                    EIGEN878          
      I=L                                                               EIGEN879          
321   IF(I.GT.N) GOTO 329                                               EIGEN880          
      H(I,K)=DBLE(H(I,K))/X                                             EIGEN881          
      SUBDIA(I)=DBLE(A(I,K))/SR2                                        EIGEN882          
C PREMULTIPLICATION BY THE MATRIX PR.                                   EIGEN883          
      I=I+1                                                             EIGEN884          
      GOTO 321                                                          EIGEN885          
329   CONTINUE                                                          EIGEN886          
      J=L                                                               EIGEN887          
351   IF(J.GT.N) GOTO 359                                               EIGEN888          
      SR=0.0                                                            EIGEN889          
      I=L                                                               EIGEN890          
371   IF(I.GT.N) GOTO 379                                               EIGEN891          
      SR=SR+DBLE(A(I,K))*DBLE(A(I,J))                                   EIGEN892          
      I=I+1                                                             EIGEN893          
      GOTO 371                                                          EIGEN894          
379   CONTINUE                                                          EIGEN895          
      I=L                                                               EIGEN896          
391   IF(I.GT.N) GOTO 399                                               EIGEN897          
      A(I,J)=DBLE(A(I,J))-SUBDIA(I)*SR                                  EIGEN898          
C POSTMULTIPLICATION BY THE MATRIX PR.                                  EIGEN899          
      I=I+1                                                             EIGEN900          
      GOTO 391                                                          EIGEN901          
399   CONTINUE                                                          EIGEN902          
      J=J+1                                                             EIGEN903          
      GOTO 351                                                          EIGEN904          
359   CONTINUE                                                          EIGEN905          
      J=1                                                               EIGEN906          
411   IF(J.GT.N) GOTO 419                                               EIGEN907          
      SR=0.0                                                            EIGEN908          
      I=L                                                               EIGEN909          
431   IF(I.GT.N) GOTO 439                                               EIGEN910          
      SR=SR+DBLE(A(J,I))*DBLE(A(I,K))                                   EIGEN911          
      I=I+1                                                             EIGEN912          
      GOTO 431                                                          EIGEN913          
439   CONTINUE                                                          EIGEN914          
      I=L                                                               EIGEN915          
451   IF(I.GT.N) GOTO 459                                               EIGEN916          
      A(J,I)=DBLE(A(J,I))-SUBDIA(I)*SR                                  EIGEN917          
      I=I+1                                                             EIGEN918          
      GOTO 451                                                          EIGEN919          
459   CONTINUE                                                          EIGEN920          
      J=J+1                                                             EIGEN921          
      GOTO 411                                                          EIGEN922          
419   CONTINUE                                                          EIGEN923          
470   CONTINUE                                                          EIGEN924          
      K=K+1                                                             EIGEN925          
      GOTO 61                                                           EIGEN926          
69    CONTINUE                                                          EIGEN927          
      K=1                                                               EIGEN928          
481   IF(K.GT.M) GOTO 489                                               EIGEN929          
      A(K+1,K)=SUBDIA(K)                                                EIGEN930          
C TRANSER OF THE UPPER HALF OF THE MATRIX A INTO THE                    EIGEN931          
C ARRAY H AND THE CALCULATION OF THE SMALL POSITIVE NUMBER              EIGEN932          
C EPS.                                                                  EIGEN933          
      K=K+1                                                             EIGEN934          
      GOTO 481                                                          EIGEN935          
489   CONTINUE                                                          EIGEN936          
      SUBDIA(N-1)=A(N,N-1)                                              EIGEN937          
510   EPS=0.0                                                           EIGEN938          
      K=1                                                               EIGEN939          
521   IF(K.GT.N) GOTO 529                                               EIGEN940          
      INDIC(K)=0                                                        EIGEN941          
      IF(.NOT.(K.NE.N)) GOTO 550                                        EIGEN942          
      EPS=EPS+SUBDIA(K)**2                                              EIGEN943          
550   CONTINUE                                                          EIGEN944          
      I=K                                                               EIGEN945          
561   IF(I.GT.N) GOTO 569                                               EIGEN946          
      H(K,I)=A(K,I)                                                     EIGEN947          
      EPS=EPS+DBLE(A(K,I))**2                                           EIGEN948          
      I=I+1                                                             EIGEN949          
      GOTO 561                                                          EIGEN950          
569   CONTINUE                                                          EIGEN951          
      K=K+1                                                             EIGEN952          
      GOTO 521                                                          EIGEN953          
529   CONTINUE                                                          EIGEN954          
      EPS=EX*DSQRT(EPS)                                                 EIGEN955          
C                                                                       EIGEN956          
C THE QR ITERATIVE PROCESS. THE UPPER-HESSENBERG MATRIX H IS            EIGEN957          
C REDUCED TO THE UPPER-MODIFIED TRIANGULAR FORM.                        EIGEN958          
C                                                                       EIGEN959          
C DETERMINATION OF THE SHIFT OF ORIGIN FOR THE FIRST STEP OF            EIGEN960          
C THE QR ITERATIVE PROCESS.                                             EIGEN961          
      SHIFT=A(N,N-1)                                                    EIGEN962          
      IF(.NOT.(N.LE.2)) GOTO 620                                        EIGEN963          
      SHIFT=0.0                                                         EIGEN964          
620   CONTINUE                                                          EIGEN965          
      IF(.NOT.(A(N,N).NE.0.0)) GOTO 640                                 EIGEN966          
      SHIFT=0.0                                                         EIGEN967          
640   CONTINUE                                                          EIGEN968          
      IF(.NOT.(A(N-1,N).NE.0.0)) GOTO 660                               EIGEN969          
      SHIFT=0.0                                                         EIGEN970          
660   CONTINUE                                                          EIGEN971          
      IF(.NOT.(A(N-1,N-1).NE.0.0)) GOTO 680                             EIGEN972          
      SHIFT=0.0                                                         EIGEN973          
680   CONTINUE                                                          EIGEN974          
      M=N                                                               EIGEN975          
      NS=0                                                              EIGEN976          
      MAXST=N*10                                                        EIGEN977          
C                                                                       EIGEN978          
C TESTING IF THE UPPER HALF OF THE MATRIX IS EQUAL TO ZERO.             EIGEN979          
C IF IT IS EQUAL TO ZERO THE QR PROCESS IS NOT NECESSARY.               EIGEN980          
      I=2                                                               EIGEN981          
721   IF(I.GT.N) GOTO 729                                               EIGEN982          
      K=I                                                               EIGEN983          
731   IF(K.GT.N) GOTO 739                                               EIGEN984          
      IF(A(I-1,K).NE.0.0) GOTO 820                                      EIGEN985          
      K=K+1                                                             EIGEN986          
      GOTO 731                                                          EIGEN987          
739   CONTINUE                                                          EIGEN988          
      I=I+1                                                             EIGEN989          
      GOTO 721                                                          EIGEN990          
729   CONTINUE                                                          EIGEN991          
      I=1                                                               EIGEN992          
771   IF(I.GT.N) GOTO 779                                               EIGEN993          
      INDIC(I)=1                                                        EIGEN994          
      EVR(I)=A(I,I)                                                     EIGEN995          
      EVI(I)=0.0                                                        EIGEN996          
      I=I+1                                                             EIGEN997          
      GOTO 771                                                          EIGEN998          
779   CONTINUE                                                          EIGEN999          
      GOTO 2040                                                         EIGEO000          
C                                                                       EIGEO001          
C START THE MAIN LOOP OF THE QR PROCESS.                                EIGEO002          
820   K=M-1                                                             EIGEO003          
      M1=K                                                              EIGEO004          
      I=K                                                               EIGEO005          
C FIND ANY DECOMPOSITIONS OF THE MATRIX.                                EIGEO006          
C JUMP TO 34 IF THE LAST SUBMATRIX OF THE DECOMPOSITION IS              EIGEO007          
C OF THE ORDER ONE.                                                     EIGEO008          
C JUMP TO 35 IF THE LAST SUBMATRIX OF THE DECOMPOSITION IS              EIGEO009          
C OF THE ORDER TWO.                                                     EIGEO010          
      IF(K) 2040,1800,860                                               EIGEO011          
860   IF(DBLE(ABS(A(M,K))).LE.EPS) GOTO 1800                            EIGEO012          
      IF(M-2.EQ.0) GOTO 1850                                            EIGEO013          
900   I=I-1                                                             EIGEO014          
      IF(DBLE(ABS(A(K,I))).LE.EPS) GOTO 960                             EIGEO015          
      K=I                                                               EIGEO016          
      IF(K.GT.1) GOTO 900                                               EIGEO017          
960   IF(K.EQ.M1) GOTO 1850                                             EIGEO018          
C TRANSFORMATION OF THE MATRIX OF THE ORDER GREATER THAN TWO            EIGEO019          
      S=DBLE(A(M,M))+DBLE(A(M1,M1))+SHIFT                               EIGEO020          
      SR=DBLE(A(M,M))*DBLE(A(M1,M1))-DBLE(A(M,M1))*DBLE(A(M1,M))+0.25D0 EIGEO021          
     .*SHIFT**2                                                         EIGEO022          
      A(K+2,K)=0.0                                                      EIGEO023          
C CALCULATE X1,Y1,Z1,FOR THE SUBMATRIX OBTAINED BY THE                  EIGEO024          
C DECOMPOSITION                                                         EIGEO025          
      X=DBLE(A(K,K))*(DBLE(A(K,K))-S)+DBLE(A(K,K+1))*DBLE(A(K+1,K))+SR  EIGEO026          
      Y=DBLE(A(K+1,K))*(DBLE(A(K,K))+DBLE(A(K+1,K+1))-S)                EIGEO027          
      R=DABS(X)+DABS(Y)                                                 EIGEO028          
      IF(.NOT.(R.EQ.0.0D0)) GOTO 1050                                   EIGEO029          
      SHIFT=A(M,M-1)                                                    EIGEO030          
1050  CONTINUE                                                          EIGEO031          
      IF(R.EQ.0.0D0) GOTO 960                                           EIGEO032          
      Z=A(K+2,K+1)*A(K+1,K)                                             EIGEO033          
      SHIFT=0.0                                                         EIGEO034          
      NS=NS+1                                                           EIGEO035          
C                                                                       EIGEO036          
C THE LOOP FOR ONE STEP OF THE QR PROCESS.                              EIGEO037          
      I=K                                                               EIGEO038          
1111  IF(I.GT.M1) GOTO 1119                                             EIGEO039          
      IF(I.EQ.K) GOTO 1200                                              EIGEO040          
C CALCULATE XR,YR,ZR.                                                   EIGEO041          
      X=A(I,I-1)                                                        EIGEO042          
      Y=A(I+1,I-1)                                                      EIGEO043          
      Z=0.0                                                             EIGEO044          
      IF(I+2.GT.M) GOTO 1200                                            EIGEO045          
      Z=A(I+2,I-1)                                                      EIGEO046          
1200  SR2=DABS(X)+DABS(Y)+DABS(Z)                                       EIGEO047          
      IF(SR2.EQ.0.0D0) GOTO 1260                                        EIGEO048          
      X=X/SR2                                                           EIGEO049          
      Y=Y/SR2                                                           EIGEO050          
      Z=Z/SR2                                                           EIGEO051          
1260  S=DSQRT(X*X+Y*Y+Z*Z)                                              EIGEO052          
      IF(X.LT.0.0D0) GOTO 1300                                          EIGEO053          
      S=-S                                                              EIGEO054          
1300  IF(I.EQ.K) GOTO 1330                                              EIGEO055          
      A(I,I-1)=S*SR2                                                    EIGEO056          
1330  IF(SR2.NE.0.0D0) GOTO 1380                                        EIGEO057          
      IF(I+3.GT.M) GOTO 1760                                            EIGEO058          
      GOTO 1730                                                         EIGEO059          
1380  SR=1.0D0-X/S                                                      EIGEO060          
      S=X-S                                                             EIGEO061          
      X=Y/S                                                             EIGEO062          
      Y=Z/S                                                             EIGEO063          
C PREMULTIPLICATION BY THE MATRIX PR.                                   EIGEO064          
      J=I                                                               EIGEO065          
1421  IF(J.GT.M) GOTO 1429                                              EIGEO066          
      S=DBLE(A(I,J))+DBLE(A(I+1,J))*X                                   EIGEO067          
      IF(I+2.GT.M) GOTO 1470                                            EIGEO068          
      S=S+DBLE(A(I+2,J))*Y                                              EIGEO069          
1470  S=S*SR                                                            EIGEO070          
      A(I,J)=DBLE(A(I,J))-S                                             EIGEO071          
      A(I+1,J)=DBLE(A(I+1,J))-S*X                                       EIGEO072          
      IF(I+2.GT.M) GOTO 1530                                            EIGEO073          
      A(I+2,J)=DBLE(A(I+2,J))-S*Y                                       EIGEO074          
1530  CONTINUE                                                          EIGEO075          
C POSTMULTIPLICATION BY THE MATRIX PR.                                  EIGEO076          
      J=J+1                                                             EIGEO077          
      GOTO 1421                                                         EIGEO078          
1429  CONTINUE                                                          EIGEO079          
      L=I+2                                                             EIGEO080          
      IF(I.LT.M1) GOTO 1580                                             EIGEO081          
      L=M                                                               EIGEO082          
1580  J=K                                                               EIGEO083          
1581  IF(J.GT.L) GOTO 1589                                              EIGEO084          
      S=DBLE(A(J,I))+DBLE(A(J,I+1))*X                                   EIGEO085          
      IF(I+2.GT.M) GOTO 1630                                            EIGEO086          
      S=S+DBLE(A(J,I+2))*Y                                              EIGEO087          
1630  S=S*SR                                                            EIGEO088          
      A(J,I)=DBLE(A(J,I))-S                                             EIGEO089          
      A(J,I+1)=DBLE(A(J,I+1))-S*X                                       EIGEO090          
      IF(I+2.GT.M) GOTO 1690                                            EIGEO091          
      A(J,I+2)=DBLE(A(J,I+2))-S*Y                                       EIGEO092          
1690  CONTINUE                                                          EIGEO093          
      J=J+1                                                             EIGEO094          
      GOTO 1581                                                         EIGEO095          
1589  CONTINUE                                                          EIGEO096          
      IF(I+3.GT.M) GOTO 1760                                            EIGEO097          
      S=-DBLE(A(I+3,I+2))*Y*SR                                          EIGEO098          
1730  A(I+3,I)=S                                                        EIGEO099          
      A(I+3,I+1)=S*X                                                    EIGEO100          
      A(I+3,I+2)=S*Y+DBLE(A(I+3,I+2))                                   EIGEO101          
1760  CONTINUE                                                          EIGEO102          
C                                                                       EIGEO103          
      I=I+1                                                             EIGEO104          
      GOTO 1111                                                         EIGEO105          
1119  CONTINUE                                                          EIGEO106          
      IF(NS.GT.MAXST) GOTO 2040                                         EIGEO107          
      GOTO 820                                                          EIGEO108          
C                                                                       EIGEO109          
C COMPUTE THE LAST EIGENVALUE.                                          EIGEO110          
1800  EVR(M)=A(M,M)                                                     EIGEO111          
      EVI(M)=0.0                                                        EIGEO112          
      INDIC(M)=1                                                        EIGEO113          
      M=K                                                               EIGEO114          
      GOTO 820                                                          EIGEO115          
C                                                                       EIGEO116          
C COMPUTE THE EIGENVALUES OF THE LAST 2X2 MATRIX OBTAINED BY            EIGEO117          
C THE DECOMPOSITION.                                                    EIGEO118          
1850  R=0.5*(A(K,K)+A(M,M))                                             EIGEO119          
      S=0.5*(A(M,M)-A(K,K))                                             EIGEO120          
      S=S*S+DBLE(A(K,M))*DBLE(A(M,K))                                   EIGEO121          
      INDIC(M)=1                                                        EIGEO122          
      INDIC(K)=INDIC(M)                                                 EIGEO123          
      IF(S.LT.0.0D0) GOTO 1970                                          EIGEO124          
      T=DSQRT(S)                                                        EIGEO125          
      EVR(K)=R-DBLE(T)                                                  EIGEO126          
      EVR(M)=R+DBLE(T)                                                  EIGEO127          
      EVI(M)=0.0                                                        EIGEO128          
      EVI(K)=EVI(M)                                                     EIGEO129          
      M=M-2                                                             EIGEO130          
      GOTO 820                                                          EIGEO131          
1970  T=DSQRT(-S)                                                       EIGEO132          
      EVR(K)=R                                                          EIGEO133          
      EVI(K)=T                                                          EIGEO134          
      EVR(M)=R                                                          EIGEO135          
      EVI(M)=-T                                                         EIGEO136          
      M=M-2                                                             EIGEO137          
      GOTO 820                                                          EIGEO138          
C                                                                       EIGEO139          
2040  CONTINUE                                                          EIGEO140          
      RETURN                                                            EIGEO141          
      END                                                               EIGEO142          
      SUBROUTINE REALVE(N,NM,M,IVEC,A,VECR,EVR,EVI,IWORK,WORK,INDIC,EPS EIGEO143          
     .,EX)                                                              EIGEO144          
      REAL T                                                            EIGEO145          
      INTEGER I,IVEC,ITER,J,K,L,M,N,NM,NS                               EIGEO146          
      DOUBLEPRECISION S,SR,WORK,BOUND,EPS,EVALUE,EX,PREVIS,R,R1         EIGEO147          
      DIMENSION A(NM,1),VECR(NM,1),EVR(NM),EVI(NM),IWORK(NM),WORK(NM),  EIGEO148          
     .INDIC(NM)                                                         EIGEO149          
C                                                                       EIGEO150          
C THE FOLLOWING DOUBLE PRECISION VARIABLES WERE INITIALLY SINGLE-       EIGEO151          
C BOUND,EPS,EVALUE,EX,PREVIS,R,R1,WORK                                  EIGEO152          
C                                                                       EIGEO153          
C                                                                       EIGEO154          
C THIS SUBROUTINE FINDS THE REAL EIGENVECTOR OF THE REAL                EIGEO155          
C UPPER-HESSENBERG MATRIX IN THE ARRAY A,CORRESPONDING TO               EIGEO156          
C THE REAL EIGENVALUE STORED IN EVR(IVEC). THE INVERSE                  EIGEO157          
C ITERATION METHOD IS USED.                                             EIGEO158          
C NOTE THE MATRIX IN A IS DESTROYED BY THE SUBROUTINE.                  EIGEO159          
C N IS THE ORDER OF THE UPPER-HESSENBERG MATRIX.                        EIGEO160          
C NM DEFINES THE FIRST DIMENSION OF THE TWO DIMENSIONAL                 EIGEO161          
C ARRAYS A AND VECR. NM MUST BE EQUAL TO OR GREATER THAN N.             EIGEO162          
C M IS THE ORDER OF THE SUBMATRIX OBTAINED BY A SUITABLE                EIGEO163          
C DECOMPOSITION OF THE UPPER-HESSENBERG MATRIX IF SOME                  EIGEO164          
C SUBDIAGONAL ELEMENTS ARE EQUAL TO ZERO. THE VALUE OF M IS             EIGEO165          
C CHOSEN SO THAT THE LAST N-M COMPONENTS OF THE EIGENVECTOR             EIGEO166          
C ARE ZERO.                                                             EIGEO167          
C IVEC GIVES THE POSITION OF THE EIGENVALUE IN THE ARRAY EVR            EIGEO168          
C FOR WHICH THE CORRESPONDING EIGENVECTOR IS COMPUTED.                  EIGEO169          
C THE ARRAY EVI WOULD CONTAIN THE IMAGINARY PARTS OF THE N              EIGEO170          
C EIGENVALUES IF THEY EXISTED.                                          EIGEO171          
C                                                                       EIGEO172          
C THE M COMPONENTS OF THE COMPUTED REAL EIGENVECTOR WILL BE             EIGEO173          
C FOUND IN THE FIRST M PLACES OF THE COLUMN IVEC OF THE TWO             EIGEO174          
C DIMENSIONAL ARRAY VECR.                                               EIGEO175          
C                                                                       EIGEO176          
C IWORK AND WORK ARE THE WORKING STORES USED DURING THE                 EIGEO177          
C GAUSSIAN ELIMINATION AND BACKSUBSTITUTION PROCESS.                    EIGEO178          
C THE ARRAY INDIC INDICATES THE SUCCESS OF THE ROUTINE AS               EIGEO179          
C FOLLOWS                                                               EIGEO180          
C     VALUE OF INDIC(I)   EIGENVECTOR I                                 EIGEO181          
C            1             NOT FOUND                                    EIGEO182          
C            2               FOUND                                      EIGEO183          
C EPS IS A SMALL POSITIVE NUMBER THAT NUMERICALLY REPRESENTS            EIGEO184          
C ZERO IN THE PROGRAM. EPS = (EUCLIDIAN NORM OF A)*EX,WHERE             EIGEO185          
C EX = 2**(-T). T IS THE NUMBER OF BINARY DIGITS IN THE                 EIGEO186          
C MANTISSA OF A FLOATING POINT NUMBER.                                  EIGEO187          
      VECR(1,IVEC)=1.0                                                  EIGEO188          
      IF(M.EQ.1) GOTO 1090                                              EIGEO189          
C SMALL PERTURBATION OF EQUAL EIGENVALUES TO OBTAIN A FULL              EIGEO190          
C SET OF EIGENVECTORS.                                                  EIGEO191          
      EVALUE=EVR(IVEC)                                                  EIGEO192          
      IF(IVEC.EQ.M) GOTO 180                                            EIGEO193          
      K=IVEC+1                                                          EIGEO194          
      R=0.0                                                             EIGEO195          
      I=K                                                               EIGEO196          
101   IF(I.GT.M) GOTO 109                                               EIGEO197          
      IF(EVALUE.NE.DBLE(EVR(I))) GOTO 160                               EIGEO198          
      IF(EVI(I).NE.0.0) GOTO 160                                        EIGEO199          
      R=R+3.0D0                                                         EIGEO200          
160   CONTINUE                                                          EIGEO201          
      I=I+1                                                             EIGEO202          
      GOTO 101                                                          EIGEO203          
109   CONTINUE                                                          EIGEO204          
      EVALUE=EVALUE+R*EX                                                EIGEO205          
180   K=1                                                               EIGEO206          
181   IF(K.GT.M) GOTO 189                                               EIGEO207          
      A(K,K)=DBLE(A(K,K))-EVALUE                                        EIGEO208          
C                                                                       EIGEO209          
C GAUSSIAN ELIMINATION OF THE UPPER-HESSENBERG MATRIX A. ALL            EIGEO210          
C ROW INTERCHANGES ARE INDICATED IN THE ARRAY IWORK.ALL THE             EIGEO211          
C MULTIPLIERS ARE STORED AS THE SUBDIAGONAL ELEMENTS OF A.              EIGEO212          
      K=K+1                                                             EIGEO213          
      GOTO 181                                                          EIGEO214          
189   CONTINUE                                                          EIGEO215          
      K=M-1                                                             EIGEO216          
      I=1                                                               EIGEO217          
211   IF(I.GT.K) GOTO 219                                               EIGEO218          
      L=I+1                                                             EIGEO219          
      IWORK(I)=0                                                        EIGEO220          
      IF(A(I+1,I).NE.0.0) GOTO 300                                      EIGEO221          
      IF(A(I,I).NE.0.0) GOTO 410                                        EIGEO222          
      A(I,I)=EPS                                                        EIGEO223          
      GOTO 410                                                          EIGEO224          
300   IF(ABS(A(I,I)).GE.ABS(A(I+1,I))) GOTO 370                         EIGEO225          
      IWORK(I)=1                                                        EIGEO226          
      J=I                                                               EIGEO227          
331   IF(J.GT.M) GOTO 339                                               EIGEO228          
      R=A(I,J)                                                          EIGEO229          
      A(I,J)=A(I+1,J)                                                   EIGEO230          
      A(I+1,J)=R                                                        EIGEO231          
      J=J+1                                                             EIGEO232          
      GOTO 331                                                          EIGEO233          
339   CONTINUE                                                          EIGEO234          
370   R=-A(I+1,I)/A(I,I)                                                EIGEO235          
      A(I+1,I)=R                                                        EIGEO236          
      J=L                                                               EIGEO237          
391   IF(J.GT.M) GOTO 399                                               EIGEO238          
      A(I+1,J)=DBLE(A(I+1,J))+R*DBLE(A(I,J))                            EIGEO239          
      J=J+1                                                             EIGEO240          
      GOTO 391                                                          EIGEO241          
399   CONTINUE                                                          EIGEO242          
410   CONTINUE                                                          EIGEO243          
      I=I+1                                                             EIGEO244          
      GOTO 211                                                          EIGEO245          
219   CONTINUE                                                          EIGEO246          
      IF(A(M,M).NE.0.0) GOTO 450                                        EIGEO247          
      A(M,M)=EPS                                                        EIGEO248          
C                                                                       EIGEO249          
C THE VECTOR (1,1,...,1) IS STORED IN THE PLACE OF THE RIGHT            EIGEO250          
C HAND SIDE COLUMN VECTOR.                                              EIGEO251          
450   I=1                                                               EIGEO252          
451   IF(I.GT.N) GOTO 459                                               EIGEO253          
      IF(I.GT.M) GOTO 500                                               EIGEO254          
      WORK(I)=1.0                                                       EIGEO255          
      GOTO 510                                                          EIGEO256          
500   WORK(I)=0.0                                                       EIGEO257          
510   CONTINUE                                                          EIGEO258          
C                                                                       EIGEO259          
C THE INVERSE ITERATION IS PERFORMED ON THE MATRIX UNTIL THE            EIGEO260          
C INFINITE NORM OF THE RIGHT-HAND SIDE VECTOR IS GREATER                EIGEO261          
C THAN THE BOUND DEFINED AS  0.01(N*EX).                                EIGEO262          
      I=I+1                                                             EIGEO263          
      GOTO 451                                                          EIGEO264          
459   CONTINUE                                                          EIGEO265          
      BOUND=0.01D0/(EX*DBLE(FLOAT(N)))                                  EIGEO266          
      NS=0                                                              EIGEO267          
      ITER=1                                                            EIGEO268          
C                                                                       EIGEO269          
C THE BACKSUBSTITUTION.                                                 EIGEO270          
550   R=0.0                                                             EIGEO271          
      I=1                                                               EIGEO272          
561   IF(I.GT.M) GOTO 569                                               EIGEO273          
      J=M-I+1                                                           EIGEO274          
      S=WORK(J)                                                         EIGEO275          
      IF(J.EQ.M) GOTO 650                                               EIGEO276          
      L=J+1                                                             EIGEO277          
      K=L                                                               EIGEO278          
621   IF(K.GT.M) GOTO 629                                               EIGEO279          
      SR=WORK(K)                                                        EIGEO280          
      S=S-SR*DBLE(A(J,K))                                               EIGEO281          
      K=K+1                                                             EIGEO282          
      GOTO 621                                                          EIGEO283          
629   CONTINUE                                                          EIGEO284          
650   WORK(J)=S/DBLE(A(J,J))                                            EIGEO285          
      T=DABS(WORK(J))                                                   EIGEO286          
      IF(R.GE.DBLE(T)) GOTO 700                                         EIGEO287          
      R=T                                                               EIGEO288          
700   CONTINUE                                                          EIGEO289          
C                                                                       EIGEO290          
C THE COMPUTATION OF THE RIGHT-HAND SIDE VECTOR FOR THE NEW             EIGEO291          
C ITERATION STEP.                                                       EIGEO292          
      I=I+1                                                             EIGEO293          
      GOTO 561                                                          EIGEO294          
569   CONTINUE                                                          EIGEO295          
      I=1                                                               EIGEO296          
711   IF(I.GT.M) GOTO 719                                               EIGEO297          
      WORK(I)=WORK(I)/R                                                 EIGEO298          
C                                                                       EIGEO299          
C THE COMPUTATION OF THE RESIDUALS AND COMPARISON OF THE                EIGEO300          
C RESIDUALS OF THE TWO SUCCESSIVE STEPS OF THE INVERSE                  EIGEO301          
C ITERATION. IF THE INFINITE NORM OF THE RESIDUAL VECTOR IS             EIGEO302          
C GREATER THAN THE INFINITE NORM OF THE PREVIOUS RESIDUAL               EIGEO303          
C VECTOR THE COMPUTED EIGENVECTOR OF THE PREVIOUS STEP IS               EIGEO304          
C TAKEN AS THE FINAL EIGENVECTOR.                                       EIGEO305          
      I=I+1                                                             EIGEO306          
      GOTO 711                                                          EIGEO307          
719   CONTINUE                                                          EIGEO308          
      R1=0.0                                                            EIGEO309          
      I=1                                                               EIGEO310          
741   IF(I.GT.M) GOTO 749                                               EIGEO311          
      T=0.0                                                             EIGEO312          
      J=I                                                               EIGEO313          
761   IF(J.GT.M) GOTO 769                                               EIGEO314          
      T=DBLE(T)+DBLE(A(I,J))*WORK(J)                                    EIGEO315          
      J=J+1                                                             EIGEO316          
      GOTO 761                                                          EIGEO317          
769   CONTINUE                                                          EIGEO318          
      T=ABS(T)                                                          EIGEO319          
      IF(R1.GE.DBLE(T)) GOTO 820                                        EIGEO320          
      R1=T                                                              EIGEO321          
820   CONTINUE                                                          EIGEO322          
      I=I+1                                                             EIGEO323          
      GOTO 741                                                          EIGEO324          
749   CONTINUE                                                          EIGEO325          
      IF(ITER.EQ.1) GOTO 870                                            EIGEO326          
      IF(PREVIS.LE.R1) GOTO 1090                                        EIGEO327          
870   I=1                                                               EIGEO328          
871   IF(I.GT.M) GOTO 879                                               EIGEO329          
      VECR(I,IVEC)=WORK(I)                                              EIGEO330          
      I=I+1                                                             EIGEO331          
      GOTO 871                                                          EIGEO332          
879   CONTINUE                                                          EIGEO333          
      PREVIS=R1                                                         EIGEO334          
      IF(NS.EQ.1) GOTO 1090                                             EIGEO335          
      IF(ITER.GT.6) GOTO 1100                                           EIGEO336          
      ITER=ITER+1                                                       EIGEO337          
      IF(R.LT.BOUND) GOTO 980                                           EIGEO338          
      NS=1                                                              EIGEO339          
C                                                                       EIGEO340          
C GAUSSIAN ELIMINATION OF THE RIGHT-HAND SIDE VECTOR.                   EIGEO341          
980   K=M-1                                                             EIGEO342          
      I=1                                                               EIGEO343          
991   IF(I.GT.K) GOTO 999                                               EIGEO344          
      R=WORK(I+1)                                                       EIGEO345          
      IF(IWORK(I).EQ.0) GOTO 1060                                       EIGEO346          
      WORK(I+1)=WORK(I)+WORK(I+1)*DBLE(A(I+1,I))                        EIGEO347          
      WORK(I)=R                                                         EIGEO348          
      GOTO 1070                                                         EIGEO349          
1060  WORK(I+1)=WORK(I+1)+WORK(I)*DBLE(A(I+1,I))                        EIGEO350          
1070  CONTINUE                                                          EIGEO351          
      I=I+1                                                             EIGEO352          
      GOTO 991                                                          EIGEO353          
999   CONTINUE                                                          EIGEO354          
      GOTO 550                                                          EIGEO355          
C                                                                       EIGEO356          
1090  INDIC(IVEC)=2                                                     EIGEO357          
1100  IF(M.EQ.N) GOTO 1150                                              EIGEO358          
      J=M+1                                                             EIGEO359          
      I=J                                                               EIGEO360          
1131  IF(I.GT.N) GOTO 1139                                              EIGEO361          
      VECR(I,IVEC)=0.0                                                  EIGEO362          
      I=I+1                                                             EIGEO363          
      GOTO 1131                                                         EIGEO364          
1139  CONTINUE                                                          EIGEO365          
1150  CONTINUE                                                          EIGEO366          
      RETURN                                                            EIGEO367          
      END                                                               EIGEO368          
