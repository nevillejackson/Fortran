      SUBROUTINE FIT(X,Y,LIM,IMAX,AA,I,B)                               FIT00002          
      DIMENSION PLOD(30),PMID(30),A(30),B(30),X(600),Y(600),AA(30)      FIT00003          
C     THIS DECK CONTAINS SUBROUTINES DECHEB AND POLYE                   FIT00004          
      XSMALL=X(1)                                                       FIT00005          
      XLARGE=XSMALL                                                     FIT00006          
      J=2                                                               FIT00007          
31    IF(J.GT.LIM) GOTO 39                                              FIT00008          
      IF(.NOT.(X(J).GT.XLARGE)) GOTO 70                                 FIT00009          
      XLARGE=X(J)                                                       FIT00010          
      GOTO 90                                                           FIT00011          
70    IF(.NOT.(X(J).LT.XSMALL)) GOTO 90                                 FIT00012          
      XSMALL=X(J)                                                       FIT00013          
90    CONTINUE                                                          FIT00014          
      J=J+1                                                             FIT00015          
      GOTO 31                                                           FIT00016          
39    CONTINUE                                                          FIT00017          
      S=-(XLARGE+XSMALL)/2.                                             FIT00018          
      R=(XLARGE-XSMALL)/2.                                              FIT00019          
      J=1                                                               FIT00020          
121   IF(J.GT.LIM) GOTO 129                                             FIT00021          
      X(J)=(X(J)+S)/R                                                   FIT00022          
      J=J+1                                                             FIT00023          
      GOTO 121                                                          FIT00024          
129   CONTINUE                                                          FIT00025          
      GG=S/R                                                            FIT00026          
      K=1                                                               FIT00027          
151   IF(K.GT.IMAX) GOTO 159                                            FIT00028          
      A(K)=0.0                                                          FIT00029          
      K=K+1                                                             FIT00030          
      GOTO 151                                                          FIT00031          
159   CONTINUE                                                          FIT00032          
      K=1                                                               FIT00033          
171   IF(K.GT.30) GOTO 179                                              FIT00034          
      PMID(K)=0.0                                                       FIT00035          
      PLOD(K)=PMID(K)                                                   FIT00036          
      K=K+1                                                             FIT00037          
      GOTO 171                                                          FIT00038          
179   CONTINUE                                                          FIT00039          
      PMID(1)=1.0                                                       FIT00040          
      DELSQ=0.0                                                         FIT00041          
      I=1                                                               FIT00042          
220   SUMYP=0.0                                                         FIT00043          
      SUMXPP=SUMYP                                                      FIT00044          
      SUMPP=SUMXPP                                                      FIT00045          
      K=1                                                               FIT00046          
231   IF(K.GT.LIM) GOTO 239                                             FIT00047          
      J=I                                                               FIT00048          
      POLY=POLYE(X(K),PMID,J)                                           FIT00049          
      PP=POLY*POLY                                                      FIT00050          
      SUMPP=SUMPP+PP                                                    FIT00051          
      SUMXPP=SUMXPP+X(K)*PP                                             FIT00052          
      SUMYP=SUMYP+Y(K)*POLY                                             FIT00053          
      IF(I-1) 740,330,310                                               FIT00054          
310   J=I-1                                                             FIT00055          
      YAPP=POLYE(X(K),A,J)                                              FIT00056          
330   CONTINUE                                                          FIT00057          
      K=K+1                                                             FIT00058          
      GOTO 231                                                          FIT00059          
239   CONTINUE                                                          FIT00060          
      ALPHA=SUMXPP/SUMPP                                                FIT00061          
      C=SUMYP/SUMPP                                                     FIT00062          
      J=1                                                               FIT00063          
361   IF(J.GT.I) GOTO 369                                               FIT00064          
      A(J)=A(J)+C*PMID(J)                                               FIT00065          
      J=J+1                                                             FIT00066          
      GOTO 361                                                          FIT00067          
369   CONTINUE                                                          FIT00068          
      IF(I-1) 740,390,460                                               FIT00069          
390   BETA=0.0                                                          FIT00070          
      K=1                                                               FIT00071          
401   IF(K.GT.LIM) GOTO 409                                             FIT00072          
      J=I                                                               FIT00073          
      YAPP=POLYE(X(K),A,J)                                              FIT00074          
      RESID=YAPP-Y(K)                                                   FIT00075          
      DELSQ=DELSQ+RESID*RESID                                           FIT00076          
      K=K+1                                                             FIT00077          
      GOTO 401                                                          FIT00078          
409   CONTINUE                                                          FIT00079          
      GOTO 480                                                          FIT00080          
460   BETA=SUMPP/SSUMPP                                                 FIT00081          
      DELSQ=DELSQ-C*C*SUMPP                                             FIT00082          
480   AA(I)=DELSQ                                                       FIT00083          
      SSUMPP=SUMPP                                                      FIT00084          
      PHID=PMID(2)+PMID(2)-2.0*ALPHA*PMID(1)-BETA*PLOD(1)               FIT00085          
      PLOD(1)=PMID(1)                                                   FIT00086          
      PMID(1)=PHID                                                      FIT00087          
      JLIM=I+1                                                          FIT00088          
      J=2                                                               FIT00089          
541   IF(J.GT.JLIM) GOTO 549                                            FIT00090          
      PHID=PMID(J+1)+PLOD(J-1)-2.0*ALPHA*PMID(J)-BETA*PLOD(J)           FIT00091          
      PLOD(J)=PMID(J)                                                   FIT00092          
      PMID(J)=PHID                                                      FIT00093          
      J=J+1                                                             FIT00094          
      GOTO 541                                                          FIT00095          
549   CONTINUE                                                          FIT00096          
      IF(I-29) 590,590,630                                              FIT00097          
590   IF(I.GE.IMAX) GOTO 630                                            FIT00098          
      IF(I-LIM+1) 610,610,630                                           FIT00099          
610   I=I+1                                                             FIT00100          
      GOTO 220                                                          FIT00101          
630   CALL DECHEB(A,PMID,I)                                             FIT00102          
      J=1                                                               FIT00103          
641   IF(J.GT.I) GOTO 649                                               FIT00104          
      B(J)=0.                                                           FIT00105          
      IP=1                                                              FIT00106          
      H=1.0                                                             FIT00107          
      G=H                                                               FIT00108          
      M=J                                                               FIT00109          
681   IF(M.GT.I) GOTO 689                                               FIT00110          
      B(J)=B(J)+FLOAT(IP)*G*PMID(M)                                     FIT00111          
      IP=FLOAT(IP)*FLOAT(M)/H                                           FIT00112          
      H=H+1E0                                                           FIT00113          
      G=G*GG                                                            FIT00114          
      M=M+1                                                             FIT00115          
      GOTO 681                                                          FIT00116          
689   CONTINUE                                                          FIT00117          
      B(J)=B(J)/R**(J-1)                                                FIT00118          
      J=J+1                                                             FIT00119          
      GOTO 641                                                          FIT00120          
649   CONTINUE                                                          FIT00121          
740   CONTINUE                                                          FIT00122          
      RETURN                                                            FIT00123          
      END                                                               FIT00124          
      SUBROUTINE DECHEB(A,B,I)                                          FIT00125          
      DIMENSION A(30),B(30),IC(30)                                      FIT00126          
CHEBYSHEV POLYNOMIAL COEFFICIENTS, IC, ARE FORMED TO GENERATE THE NEW   FIT00127          
COEFFICIENTS, B, OF A SINGLE POLYNOMIAL OF DEGREE I-1, USING THE FORMER FIT00128          
COEFFICIENTS, A, OF EACH OF THE I CHEBYSHEV POLYNOMIALS, I NOT GT 30.   FIT00129          
      ISTART=1                                                          FIT00130          
      IC(2)=ISTART                                                      FIT00131          
      IC(1)=IC(2)                                                       FIT00132          
      B(1)=A(1)*.5                                                      FIT00133          
      J=2                                                               FIT00134          
41    IF(J.GT.I) GOTO 49                                                FIT00135          
      IC(J+1)=0                                                         FIT00136          
      B(J)=IC(J+1)                                                      FIT00137          
      ISTART=3-ISTART                                                   FIT00138          
      L=ISTART                                                          FIT00139          
71    IF(L.GT.J) GOTO 79                                                FIT00140          
      IF(L.EQ.1) GOTO 110                                               FIT00141          
      IC(L)=-IC(L)+2*IC(L-1)                                            FIT00142          
      GOTO 120                                                          FIT00143          
110   IC(L)=-IC(L)                                                      FIT00144          
120   B(L)=B(L)+A(J)*FLOAT(IC(L))                                       FIT00145          
      L=L+2                                                             FIT00146          
      GOTO 71                                                           FIT00147          
79    CONTINUE                                                          FIT00148          
      J=J+1                                                             FIT00149          
      GOTO 41                                                           FIT00150          
49    CONTINUE                                                          FIT00151          
      RETURN                                                            FIT00152          
      END                                                               FIT00153          
      FUNCTION POLYE(X,A,J)                                             FIT00154          
      DIMENSION A(30)                                                   FIT00155          
      BS1=0                                                             FIT00156          
      BS2=0                                                             FIT00157          
40    BS=2.*X*BS1-BS2+A(J)                                              FIT00158          
      J=J-1                                                             FIT00159          
      IF(.NOT.(J.NE.0)) GOTO 100                                        FIT00160          
      BS2=BS1                                                           FIT00161          
      BS1=BS                                                            FIT00162          
      GOTO 40                                                           FIT00163          
100   POLYE=.5*(BS-BS2)                                                 FIT00164          
      RETURN                                                            FIT00165          
      END                                                               FIT00166          
