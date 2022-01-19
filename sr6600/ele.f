      FUNCTION ELE(P,R)                                                 ELE00002          
      DIMENSION B(4),SYN(4)                                             ELE00003          
      XPI=1.5707963268                                                  ELE00004          
      YPI=1.570796326                                                   ELE00005          
      Q=ABS(P)                                                          ELE00006          
      IF(XPI.LT.Q) GOTO 80                                              ELE00007          
      T=0.                                                              ELE00008          
      GOTO 170                                                          ELE00009          
80    T=1.                                                              ELE00010          
      K=1                                                               ELE00011          
      N=Q/3.1415926536                                                  ELE00012          
      FN=N                                                              ELE00013          
      Q=Q-FN*3.1415926536                                               ELE00014          
      IF(.NOT.(XPI.LT.Q)) GOTO 170                                      ELE00015          
      T=-1.                                                             ELE00016          
      FN=FN+1.                                                          ELE00017          
      Q=3.1415926536-Q                                                  ELE00018          
170   S=ABS(R)                                                          ELE00019          
      IF(.NOT.(1..LT.S)) GOTO 230                                       ELE00020          
      WRITE(61,200) R                                                   ELE00021          
200   FORMAT(8H ERROR I,8HN ELE,AR,3HG= ,E18.10)                        ELE00022          
      ELE=1.E307                                                        ELE00023          
      GOTO 1200                                                         ELE00024          
230   IF(.NOT.(YPI.LT.Q)) GOTO 500                                      ELE00025          
240   A=1.-S*S                                                          ELE00026          
      D=SQRT(A)                                                         ELE00027          
      IF(S.LT..9539) GOTO 400                                           ELE00028          
      IF(.NOT.(S.EQ.1.)) GOTO 300                                       ELE00029          
      E=1.                                                              ELE00030          
      GOTO 1090                                                         ELE00031          
300   A1=(1.+S)*.5                                                      ELE00032          
      Z=A*.5+A1*A1-S                                                    ELE00033          
      C=SQRT(S)                                                         ELE00034          
      A2=(A1+C)*.5                                                      ELE00035          
      X=A1*C                                                            ELE00036          
      Z=Z*.5+A2*A2-X                                                    ELE00037          
      C=SQRT(X)                                                         ELE00038          
      A3=A2+C                                                           ELE00039          
      E=A3*.5+Z*ALOG(128.*A3*A2*A1*A1/(A*A))/A3                         ELE00040          
      GOTO 1090                                                         ELE00041          
400   C=D                                                               ELE00042          
      A1=1.                                                             ELE00043          
      Z=1.+A                                                            ELE00044          
      M=1                                                               ELE00045          
431   IF(M.GT.3) GOTO 439                                               ELE00046          
      X=A1*C                                                            ELE00047          
      A1=(A1+C)*.5                                                      ELE00048          
      Z=Z*.5-A1*A1+X                                                    ELE00049          
      C=SQRT(X)                                                         ELE00050          
      M=M+1                                                             ELE00051          
      GOTO 431                                                          ELE00052          
439   CONTINUE                                                          ELE00053          
      E=12.566370614*Z/(A1+C)                                           ELE00054          
      GOTO 1090                                                         ELE00055          
500   IF(.NOT.(Q.LT.1.E-10)) GOTO 530                                   ELE00056          
      E=0.                                                              ELE00057          
      GOTO 1090                                                         ELE00058          
530   U=1.-S*S                                                          ELE00059          
      A=1.                                                              ELE00060          
      IF(.NOT.(S.LT..9539)) GOTO 810                                    ELE00061          
      C=SQRT(U)                                                         ELE00062          
      COT=COS(Q)/SIN(Q)                                                 ELE00063          
      S2=0.                                                             ELE00064          
      S1=S2                                                             ELE00065          
      A1=U                                                              ELE00066          
      I=0                                                               ELE00067          
      M=1                                                               ELE00068          
611   IF(M.GT.4) GOTO 619                                               ELE00069          
      Y=A*C                                                             ELE00070          
      COT=(COT-Y/COT)*.5                                                ELE00071          
      I=2*I                                                             ELE00072          
      IF(.NOT.(COT.LT.0.)) GOTO 670                                     ELE00073          
      I=I+1                                                             ELE00074          
670   U=A*A                                                             ELE00075          
      A=(A+C)*.5                                                        ELE00076          
      U=U-A1                                                            ELE00077          
      W=1.                                                              ELE00078          
      IF(.NOT.(0.LT.(I-(I/4)*4-1))) GOTO 730                            ELE00079          
      W=-1.                                                             ELE00080          
730   S2=S2+U*W/SQRT(A*A+COT*COT)                                       ELE00081          
      S1=S1*.5+U                                                        ELE00082          
      A1=Y                                                              ELE00083          
      C=SQRT(Y)                                                         ELE00084          
      M=M+1                                                             ELE00085          
      GOTO 611                                                          ELE00086          
619   CONTINUE                                                          ELE00087          
      IF(.NOT.(COT.LT.0.)) GOTO 790                                     ELE00088          
      I=I+1                                                             ELE00089          
790   E=((2.*ATAN(A/COT)+FLOAT(I)*3.1415926536)*(.25-S1)/A*.5+S2)*.25   ELE00090          
      GOTO 1090                                                         ELE00091          
810   S1=U                                                              ELE00092          
      B(1)=S                                                            ELE00093          
      SY=SIN(Q)                                                         ELE00094          
      COT=COS(Q)/SY                                                     ELE00095          
      SYN(1)=SY                                                         ELE00096          
      B(4)=1.+S                                                         ELE00097          
      X=B(4)                                                            ELE00098          
      COT=(COT+SQRT(COT*COT+1.-S*S))/X                                  ELE00099          
      SY=COT*COT                                                        ELE00100          
      M=1                                                               ELE00101          
891   IF(M.GT.3) GOTO 899                                               ELE00102          
      J=M+1                                                             ELE00103          
      SYN(J)=1./SQRT(1.+SY)                                             ELE00104          
      X=A*B(M)                                                          ELE00105          
      A=B(4)*.5                                                         ELE00106          
      B(J)=SQRT(X)                                                      ELE00107          
      U=A*A                                                             ELE00108          
      S1=S1*.5+U-X                                                      ELE00109          
      B(4)=A+B(J)                                                       ELE00110          
      Y=B(4)                                                            ELE00111          
      COT=(COT*A+SQRT(U*(SY+1.)-X))/Y                                   ELE00112          
      SY=COT*COT                                                        ELE00113          
      M=M+1                                                             ELE00114          
      GOTO 891                                                          ELE00115          
899   CONTINUE                                                          ELE00116          
      SY=SQRT(1.+SY)                                                    ELE00117          
      U=1./SY                                                           ELE00118          
      V=0.                                                              ELE00119          
      B(4)=A                                                            ELE00120          
      M=1                                                               ELE00121          
1041  IF(M.GT.4) GOTO 1049                                              ELE00122          
      MM=5-M                                                            ELE00123          
      V=2.*V+B(MM)*(U-SYN(MM))                                          ELE00124          
      M=M+1                                                             ELE00125          
      GOTO 1041                                                         ELE00126          
1049  CONTINUE                                                          ELE00127          
      E=((4.*S1*ALOG((1.+SY)/COT))/A+U+V)                               ELE00128          
      GOTO 1090                                                         ELE00129          
1090  IF(.NOT.(T.EQ.0.)) GOTO 1120                                      ELE00130          
      ELE=E                                                             ELE00131          
      GOTO 1170                                                         ELE00132          
1120  IF(K.LT.1) GOTO 1160                                              ELE00133          
      K=0                                                               ELE00134          
      ELE=E                                                             ELE00135          
      GOTO 240                                                          ELE00136          
1160  ELE=T*ELE+2.*FN*E                                                 ELE00137          
1170  IF(.NOT.(P.LT.0.)) GOTO 1190                                      ELE00138          
      ELE=-ELE                                                          ELE00139          
1190  CONTINUE                                                          ELE00140          
1200  RETURN                                                            ELE00141          
      END                                                               ELE00142          
