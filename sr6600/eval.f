      SUBROUTINE EVAL(A,V1,N,P)                                         EVAL0002          
      DIMENSION A(35,35),V1(50),P(50)                                   EVAL0003          
C     THE FOLLOWING DIMENSION STATEMENT SHOULD HAVE THE FORM            EVAL0004          
C     DIMENSION A(N,N),V1(N),P(N)                                       EVAL0005          
C     EIGENVECTOR CALCULATION - HOUSEHOLDERS METHOD.                    EVAL0006          
      TEST=-1                                                           EVAL0007          
C     REDUCE TO TRIDIAGONAL FORM.                                       EVAL0008          
      NM1=N-1                                                           EVAL0009          
      K=1                                                               EVAL0010          
41    IF(K.GT.NM1) GOTO 49                                              EVAL0011          
C     CALCULATE SSQ OF OFF DIAG. ELEMENTS.                              EVAL0012          
      S=0.                                                              EVAL0013          
      KP1=K+1                                                           EVAL0014          
      I=KP1                                                             EVAL0015          
71    IF(I.GT.N) GOTO 79                                                EVAL0016          
      S=S+A(K,I)*A(K,I)                                                 EVAL0017          
      I=I+1                                                             EVAL0018          
      GOTO 71                                                           EVAL0019          
79    CONTINUE                                                          EVAL0020          
      S=SQRT(S)                                                         EVAL0021          
C     CALCULATE VECTOR W                                                EVAL0022          
      SGN=1.                                                            EVAL0023          
      IF(A(K,KP1)) 120,130,130                                          EVAL0024          
120   SGN=-1.                                                           EVAL0025          
130   WW=SQRT(A(K,KP1)*SGN/(S*2.)+0.5)                                  EVAL0026          
      A(K,KP1)=WW                                                       EVAL0027          
      WW=SGN/(WW*S*2.)                                                  EVAL0028          
      KP2=K+2                                                           EVAL0029          
      IF(K-N+1) 180,200,200                                             EVAL0030          
180   I=KP2                                                             EVAL0031          
181   IF(I.GT.N) GOTO 189                                               EVAL0032          
      A(K,I)=A(K,I)*WW                                                  EVAL0033          
      I=I+1                                                             EVAL0034          
      GOTO 181                                                          EVAL0035          
189   CONTINUE                                                          EVAL0036          
200   A(KP1,K)=-SGN*S                                                   EVAL0037          
C     CALCULATE P=AW                                                    EVAL0038          
      I=KP1                                                             EVAL0039          
211   IF(I.GT.N) GOTO 219                                               EVAL0040          
      P(I)=0.                                                           EVAL0041          
      J=KP1                                                             EVAL0042          
231   IF(J.GT.N) GOTO 239                                               EVAL0043          
      P(I)=P(I)+A(I,J)*A(K,J)                                           EVAL0044          
      J=J+1                                                             EVAL0045          
      GOTO 231                                                          EVAL0046          
239   CONTINUE                                                          EVAL0047          
      I=I+1                                                             EVAL0048          
      GOTO 211                                                          EVAL0049          
219   CONTINUE                                                          EVAL0050          
      C=0.                                                              EVAL0051          
      I=KP1                                                             EVAL0052          
261   IF(I.GT.N) GOTO 269                                               EVAL0053          
      C=C+P(I)*A(K,I)                                                   EVAL0054          
      I=I+1                                                             EVAL0055          
      GOTO 261                                                          EVAL0056          
269   CONTINUE                                                          EVAL0057          
      I=KP1                                                             EVAL0058          
281   IF(I.GT.N) GOTO 289                                               EVAL0059          
      P(I)=P(I)-C*A(K,I)                                                EVAL0060          
      I=I+1                                                             EVAL0061          
      GOTO 281                                                          EVAL0062          
289   CONTINUE                                                          EVAL0063          
      I=KP1                                                             EVAL0064          
301   IF(I.GT.N) GOTO 309                                               EVAL0065          
      J=I                                                               EVAL0066          
311   IF(J.GT.N) GOTO 319                                               EVAL0067          
      A(I,J)=A(I,J)-2.*(A(K,I)*P(J)+A(K,J)*P(I))                        EVAL0068          
      A(J,I)=A(I,J)                                                     EVAL0069          
C     S/R TO DETERMINE BOUNDS ETC. ON EIGENVALUES.                      EVAL0070          
      J=J+1                                                             EVAL0071          
      GOTO 311                                                          EVAL0072          
319   CONTINUE                                                          EVAL0073          
      I=I+1                                                             EVAL0074          
      GOTO 301                                                          EVAL0075          
309   CONTINUE                                                          EVAL0076          
      K=K+1                                                             EVAL0077          
      GOTO 41                                                           EVAL0078          
49    CONTINUE                                                          EVAL0079          
340   IF(TEST) 350,350,520                                              EVAL0080          
C     PERFORM ON FIRST ENTRY ONLY.                                      EVAL0081          
C     CALCULATE MAXIMUM BOUNDS ON EIGENVALUES.                          EVAL0082          
350   TEST=1.                                                           EVAL0083          
      J=0                                                               EVAL0084          
      BU=0.                                                             EVAL0085          
      I=2                                                               EVAL0086          
381   IF(I.GT.NM1) GOTO 389                                             EVAL0087          
      VALU=ABS(A(I,I))+ABS(A(I,I-1))+ABS(A(I+1,I))                      EVAL0088          
      IF(VALU-BU) 420,420,410                                           EVAL0089          
410   BU=VALU                                                           EVAL0090          
420   CONTINUE                                                          EVAL0091          
      I=I+1                                                             EVAL0092          
      GOTO 381                                                          EVAL0093          
389   CONTINUE                                                          EVAL0094          
      VALU=BU                                                           EVAL0095          
      BU=ABS(A(1,1))+ABS(A(2,1))                                        EVAL0096          
      IF(VALU-BU) 460,470,470                                           EVAL0097          
460   VALU=BU                                                           EVAL0098          
470   BU=ABS(A(N,N))+ABS(A(N,N-1))                                      EVAL0099          
      IF(VALU-BU) 490,500,500                                           EVAL0100          
490   VALU=BU                                                           EVAL0101          
500   K=0                                                               EVAL0102          
      ER=1.E-6                                                          EVAL0103          
520   BU=VALU*(1E0+ER)                                                  EVAL0104          
      BL=-VALU                                                          EVAL0105          
      K=K+1                                                             EVAL0106          
      IF(K-N-1) 570,560,570                                             EVAL0107          
560   K=-1                                                              EVAL0108          
C     END CONTROL PROGRAM FOR EIGENVALUES.                              EVAL0109          
570   E2=ER*2.                                                          EVAL0110          
      IF(K) 940,590,590                                                 EVAL0111          
C     20 IS RETURN TO CALLING PROGRAM.                                  EVAL0112          
590   NOU=0                                                             EVAL0113          
      NOL=N                                                             EVAL0114          
610   R=(BL+BU)/2.                                                      EVAL0115          
      IF(ABS((BU-BL)/R)-E2) 910,910,630                                 EVAL0116          
C     CALCULATE NUMBER OF SIGN AGREEMENTS IN POLYNOMIAL.                EVAL0117          
630   PM2=1.                                                            EVAL0118          
      PM1=A(1,1)-R                                                      EVAL0119          
      NO=1                                                              EVAL0120          
      L1=1                                                              EVAL0121          
      L2=1                                                              EVAL0122          
      IF(PM1) 690,690,710                                               EVAL0123          
690   L2=-1                                                             EVAL0124          
      NO=0                                                              EVAL0125          
710   I=2                                                               EVAL0126          
711   IF(I.GT.N) GOTO 719                                               EVAL0127          
      PP=(A(I,I)-R)*PM1-A(I,I-1)*A(I,I-1)*PM2                           EVAL0128          
      L1=L2                                                             EVAL0129          
      L2=1                                                              EVAL0130          
      IF(PP) 780,760,790                                                EVAL0131          
760   L2=-L1                                                            EVAL0132          
      GOTO 790                                                          EVAL0133          
780   L2=-1                                                             EVAL0134          
790   PM2=PM1                                                           EVAL0135          
      PM1=PP                                                            EVAL0136          
      IF(L1-L2) 830,820,830                                             EVAL0137          
820   NO=NO+1                                                           EVAL0138          
830   CONTINUE                                                          EVAL0139          
      I=I+1                                                             EVAL0140          
      GOTO 711                                                          EVAL0141          
719   CONTINUE                                                          EVAL0142          
      IF(NO-K) 850,880,880                                              EVAL0143          
850   BU=R                                                              EVAL0144          
      NOU=NO                                                            EVAL0145          
      GOTO 610                                                          EVAL0146          
880   BL=R                                                              EVAL0147          
      NOL=NO                                                            EVAL0148          
      GOTO 610                                                          EVAL0149          
910   J=J+1                                                             EVAL0150          
      V1(J)=R                                                           EVAL0151          
      GOTO 340                                                          EVAL0152          
940   CONTINUE                                                          EVAL0153          
C     END EIGENVALUE ROUTINE.                                           EVAL0154          
      RETURN                                                            EVAL0155          
      END                                                               EVAL0156          
