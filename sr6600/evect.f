      SUBROUTINE EVECT(A,V2,N,R,E)                                      EVECT002          
      DIMENSION A(20,20),V2(20),E(3,22)                                 EVECT003          
C ROUTINE      F4 CSIR EVECT             DATE DEC 65       170470 006201EVECT004          
C TITLE        EIGENVECTORS OF A REAL SYMMETRIC MATRIX.           006202EVECT005          
C DESCRIPTION  SUBROUTINE WILL PROVIDE SOME OR ALL OF THE         006203EVECT006          
C              EIGENVECTORS OF A REAL SYMMETRIC MATRIX.  IT IS    006204EVECT007          
C              DESIGNED FOR USE WITH F4 CSIR EVAL.                006205EVECT008          
C              REVISED FEB 1970 TO PREVENT THE DIMENSIONS OF "A"  006206EVECT009          
C              BEING EXCEEDED IN THE LOOP TO STATEMENT 71.        006207EVECT010          
C LANGUAGE     32/36FTN                  AVAILABLE FROM SRLIST    006208EVECT011          
C AVAILABLE AS (LISTING, SOURCE DECK, WRITEUP)                    006209EVECT012          
C AUTHOR       D.R. ROSS, DIVISION OF COMPUTING RESEARCH,         006210EVECT013          
C              C.S.I.R.O., BRISBANE, AUSTRALIA.                   006211EVECT014          
C KEYWORDS     EIGENVECTORS,HOUSEHOLDER                           006212EVECT015          
C SEE ALSO     EVAL,JACOBI                                        006213EVECT016          
C     THE PREVIOUS DIMENSION STATEMENT SHOULD HAVE THE FORM             EVECT017          
C     DIMENSION A(N,N),V2(N),E(3,N+2)                                   EVECT018          
C     EIGENVECTORS                                                      EVECT019          
C     CALCULATE EIGENVECTORS                                            EVECT020          
      E(1,N+1)=0.                                                       EVECT021          
      E(1,N+2)=0.                                                       EVECT022          
C     FORMULATE COEFFICIENT MATRIX                                      EVECT023          
      E(1,1)=A(1,1)-R                                                   EVECT024          
      E(2,1)=A(2,1)                                                     EVECT025          
      E(3,1)=0.                                                         EVECT026          
      E(1,N)=A(N,N-1)                                                   EVECT027          
      E(2,N)=A(N,N)-R                                                   EVECT028          
      E(3,N)=0                                                          EVECT029          
      I1=N+1                                                            EVECT030          
      NM1=N-1                                                           EVECT031          
      I2=N-1                                                            EVECT032          
      I=2                                                               EVECT033          
131   IF(I.GT.NM1) GOTO 139                                             EVECT034          
      E(1,I)=A(I,I-1)                                                   EVECT035          
      E(2,I)=A(I,I)-R                                                   EVECT036          
      E(3,I)=A(I+1,I)                                                   EVECT037          
C     REDUCE EQUATIONS TO SOLVABLE FORM                                 EVECT038          
      I=I+1                                                             EVECT039          
      GOTO 131                                                          EVECT040          
139   CONTINUE                                                          EVECT041          
      I=1                                                               EVECT042          
171   IF(I.GT.NM1) GOTO 179                                             EVECT043          
C     SELECT PIVOT ELEMENT                                              EVECT044          
      IF(ABS(E(1,I))-ABS(E(1,I+1))) 190,230,230                         EVECT045          
190   I1=1                                                              EVECT046          
191   IF(I1.GT.3) GOTO 199                                              EVECT047          
      C=E(I1,I)                                                         EVECT048          
      E(I1,I)=E(I1,I+1)                                                 EVECT049          
      E(I1,I+1)=C                                                       EVECT050          
C     REDUCE (I+1)-TH EQUATION AND LEFT SHIFT                           EVECT051          
      I1=I1+1                                                           EVECT052          
      GOTO 191                                                          EVECT053          
199   CONTINUE                                                          EVECT054          
230   C=E(1,I+1)/E(1,I)                                                 EVECT055          
      I1=1                                                              EVECT056          
241   IF(I1.GT.2) GOTO 249                                              EVECT057          
      E(I1,I+1)=E(I1+1,I+1)-E(I1+1,I)*C                                 EVECT058          
      I1=I1+1                                                           EVECT059          
      GOTO 241                                                          EVECT060          
249   CONTINUE                                                          EVECT061          
      E(3,I+1)=0.                                                       EVECT062          
C     CREATE SOLUTION IN COLUMN 1                                       EVECT063          
      I=I+1                                                             EVECT064          
      GOTO 171                                                          EVECT065          
179   CONTINUE                                                          EVECT066          
      I=N                                                               EVECT067          
      I1=1                                                              EVECT068          
281   IF(I1.GT.N) GOTO 289                                              EVECT069          
      E(1,I)=(1.-E(2,I)*E(1,I+1)-E(3,I)*E(1,I+2))/E(1,I)                EVECT070          
      I=I-1                                                             EVECT071          
C     COMPUTE EIGENVECTORS OF ORIGINAL MATRIX.                          EVECT072          
      I1=I1+1                                                           EVECT073          
      GOTO 281                                                          EVECT074          
289   CONTINUE                                                          EVECT075          
      I=1                                                               EVECT076          
311   IF(I.GT.NM1) GOTO 319                                             EVECT077          
C     COMPUTE SCALAR.                                                   EVECT078          
      C=0.                                                              EVECT079          
      I1=N-I                                                            EVECT080          
      I4=1                                                              EVECT081          
341   IF(I4.GT.I) GOTO 349                                              EVECT082          
      I2=I1+I4                                                          EVECT083          
      C=C+A(I1,I2)*E(1,I2)                                              EVECT084          
      I4=I4+1                                                           EVECT085          
      GOTO 341                                                          EVECT086          
349   CONTINUE                                                          EVECT087          
      C=2.*C                                                            EVECT088          
C     COMPUTE NEXT ITERATION ON VECTOR.                                 EVECT089          
      I4=1                                                              EVECT090          
381   IF(I4.GT.I) GOTO 389                                              EVECT091          
      I2=I1+I4                                                          EVECT092          
      E(1,I2)=E(1,I2)-C*A(I1,I2)                                        EVECT093          
C     NORMALIZE VECTOR.                                                 EVECT094          
      I4=I4+1                                                           EVECT095          
      GOTO 381                                                          EVECT096          
389   CONTINUE                                                          EVECT097          
      I=I+1                                                             EVECT098          
      GOTO 311                                                          EVECT099          
319   CONTINUE                                                          EVECT100          
      C=0.                                                              EVECT101          
      I=1                                                               EVECT102          
421   IF(I.GT.N) GOTO 429                                               EVECT103          
      C=C+E(1,I)*E(1,I)                                                 EVECT104          
      I=I+1                                                             EVECT105          
      GOTO 421                                                          EVECT106          
429   CONTINUE                                                          EVECT107          
      C=SQRT(C)                                                         EVECT108          
      I=1                                                               EVECT109          
451   IF(I.GT.N) GOTO 459                                               EVECT110          
      V2(I)=E(1,I)/C                                                    EVECT111          
      I=I+1                                                             EVECT112          
      GOTO 451                                                          EVECT113          
459   CONTINUE                                                          EVECT114          
      RETURN                                                            EVECT115          
      END                                                               EVECT116          
