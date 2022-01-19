      FUNCTION ELF(P,R)                                                 ELF00002          
      Q=ABS(P)                                                          ELF00003          
      S=ABS(R)                                                          ELF00004          
      IF(.NOT.(1..LT.S)) GOTO 90                                        ELF00005          
50    WRITE(61,60) P,R                                                  ELF00006          
60    FORMAT(8H ERROR I,5HN ELF/8H AMPLITU,4HDE= ,E18.11/8H PARAMET,    ELF00007          
     .4HER= ,E18.11/)                                                   ELF00008          
      ELF=1.E307                                                        ELF00009          
      GOTO 870                                                          ELF00010          
90    IF(.NOT.(1..EQ.S)) GOTO 110                                       ELF00011          
      IF(Q.LT.1.5707963267) GOTO 120                                    ELF00012          
      GOTO 50                                                           ELF00013          
110   IF(.NOT.(Q.LT.1.5707963268)) GOTO 140                             ELF00014          
120   T=0.                                                              ELF00015          
      GOTO 230                                                          ELF00016          
140   N=Q/3.1415926536                                                  ELF00017          
      FN=N                                                              ELF00018          
      Q=Q-FN*3.1415926536                                               ELF00019          
      T=1.                                                              ELF00020          
      K=1                                                               ELF00021          
      IF(Q.LT.1.5707963268) GOTO 230                                    ELF00022          
      FN=FN+1.                                                          ELF00023          
      T=-1.                                                             ELF00024          
      Q=3.1415926536-Q                                                  ELF00025          
230   IF(.NOT.(Q.LT.1.E-10)) GOTO 260                                   ELF00026          
      E=0.                                                              ELF00027          
      GOTO 760                                                          ELF00028          
260   IF(Q.LT.1.570796326) GOTO 450                                     ELF00029          
270   U=1.-S*S                                                          ELF00030          
      IF(.NOT.(.99.LT.S)) GOTO 360                                      ELF00031          
      IF(S.EQ.1.) GOTO 50                                               ELF00032          
      A1=(1.+S)*.5                                                      ELF00033          
      C=SQRT(S)                                                         ELF00034          
      A2=(A1+C)*.5                                                      ELF00035          
      C=SQRT(A1*C)                                                      ELF00036          
      E=ALOG(128.*(A2+C)*A2*A1*A1/(U*U))/(A2+C)*.5                      ELF00037          
      GOTO 760                                                          ELF00038          
360   B=SQRT(U)                                                         ELF00039          
      A1=1.                                                             ELF00040          
380   IF(A1-B.LT.1.E-10) GOTO 430                                       ELF00041          
      A2=(A1+B)*.5                                                      ELF00042          
      B=SQRT(A1*B)                                                      ELF00043          
      A1=A2                                                             ELF00044          
      GOTO 380                                                          ELF00045          
430   E=3.1415926536/(A1+B)                                             ELF00046          
      GOTO 760                                                          ELF00047          
450   A=1.                                                              ELF00048          
      COT=COS(Q)/SIN(Q)                                                 ELF00049          
      N=0                                                               ELF00050          
      M=N                                                               ELF00051          
      IF(.9539.LT.S) GOTO 630                                           ELF00052          
      B=SQRT(1.-S*S)                                                    ELF00053          
      B1=B                                                              ELF00054          
500   M=M+1                                                             ELF00055          
      COT=(COT-B1/COT)*.5                                               ELF00056          
      A=(A+B)*.5                                                        ELF00057          
      N=2*N                                                             ELF00058          
      J=4-M                                                             ELF00059          
      IF(.NOT.(0.LT.J)) GOTO 580                                        ELF00060          
      B=SQRT(B1)                                                        ELF00061          
      B1=A*B                                                            ELF00062          
580   IF(.NOT.(COT.LT.0.)) GOTO 600                                     ELF00063          
      N=N+1+M/4                                                         ELF00064          
600   IF(J.GT.0) GOTO 500                                               ELF00065          
      E=.0625*(ATAN(A/COT)+FLOAT(N)*1.5707963268)/A                     ELF00066          
      GOTO 760                                                          ELF00067          
630   C=S                                                               ELF00068          
      A1=C*C                                                            ELF00069          
650   M=M+1                                                             ELF00070          
      B1=A*C                                                            ELF00071          
      SECS=COT*COT+1.                                                   ELF00072          
      COT=A*COT+SQRT(A*A*SECS-A1)                                       ELF00073          
      A=(A+C)*.5                                                        ELF00074          
      COT=COT/(A*2.)                                                    ELF00075          
      A1=B1                                                             ELF00076          
      IF(.NOT.(3-M.GT.0)) GOTO 750                                      ELF00077          
      C=SQRT(B1)                                                        ELF00078          
      GOTO 650                                                          ELF00079          
750   E=ALOG((1.+SQRT(SECS))/COT)/A                                     ELF00080          
760   IF(.NOT.(T.EQ.0.)) GOTO 790                                       ELF00081          
      ELF=E                                                             ELF00082          
      GOTO 840                                                          ELF00083          
790   IF(K.EQ.0) GOTO 830                                               ELF00084          
      K=0                                                               ELF00085          
      ELF=E                                                             ELF00086          
      GOTO 270                                                          ELF00087          
830   ELF=T*ELF+2.*FN*E                                                 ELF00088          
840   IF(.NOT.(P.LT.0.)) GOTO 860                                       ELF00089          
      ELF=-ELF                                                          ELF00090          
860   CONTINUE                                                          ELF00091          
870   RETURN                                                            ELF00092          
      END                                                               ELF00093          
