      SUBROUTINE EIGEN(N,A,T,ISW)                                       EIGEN002          
      DIMENSION A(40,40),T(40,40)                                       EIGEN003          
C     EIGEN SUBROUTINE                                                  EIGEN004          
C     P J EBERLEIN                                                      EIGEN005          
C     REVISED SEPTEMBER, 1966                                           EIGEN006          
C     EIGENVALUES OF REAL MATRICES                                      EIGEN007          
C     ISW = ZERO DO NOT MAKE VECTORS                                    EIGEN008          
C     ISW LESS THAN ZERO MAKE LEFT VECTORS                              EIGEN009          
C     ISW GREATER THAN ZERO MAKE RIGHT VECTORS                          EIGEN010          
C     FORM IDENTITY MATRIX IN T IF ISW IS NOT EQUAL TO ZERO             EIGEN011          
      IF(ISW) 30,70,30                                                  EIGEN012          
30    I=1                                                               EIGEN013          
31    IF(I.GT.N) GOTO 39                                                EIGEN014          
      J=1                                                               EIGEN015          
41    IF(J.GT.N) GOTO 49                                                EIGEN016          
      T(I,J)=0.0                                                        EIGEN017          
      J=J+1                                                             EIGEN018          
      GOTO 41                                                           EIGEN019          
49    CONTINUE                                                          EIGEN020          
      T(I,I)=1.0                                                        EIGEN021          
      I=I+1                                                             EIGEN022          
      GOTO 31                                                           EIGEN023          
39    CONTINUE                                                          EIGEN024          
70    CONTINUE                                                          EIGEN025          
C     EP IS SET EQUAL TO THE MACHINE PRECISION                          EIGEN026          
      EP=1E-11                                                          EIGEN027          
      EPS=SQRT(EP)                                                      EIGEN028          
      MARK=0                                                            EIGEN029          
      NM1=N-1                                                           EIGEN030          
C     ITM LOOP                                                          EIGEN031          
      IT=1                                                              EIGEN032          
121   IF(IT.GT.50) GOTO 129                                             EIGEN033          
      IF(MARK) 140,160,140                                              EIGEN034          
140   IT=1-IT                                                           EIGEN035          
      GOTO 1170                                                         EIGEN036          
160   I=1                                                               EIGEN037          
161   IF(I.GT.NM1) GOTO 169                                             EIGEN038          
      IP1=I+1                                                           EIGEN039          
      J=IP1                                                             EIGEN040          
181   IF(J.GT.N) GOTO 189                                               EIGEN041          
      ENO=ABS(A(I,J)+A(J,I))                                            EIGEN042          
      IF(ENO-EPS) 210,210,280                                           EIGEN043          
210   ENO=ABS(A(I,J)-A(J,I))                                            EIGEN044          
      IF(ENO-EPS) 250,250,230                                           EIGEN045          
230   ENO=ABS(A(I,I)-A(J,J))                                            EIGEN046          
      IF(ENO-EPS) 250,250,280                                           EIGEN047          
250   CONTINUE                                                          EIGEN048          
      J=J+1                                                             EIGEN049          
      GOTO 181                                                          EIGEN050          
189   CONTINUE                                                          EIGEN051          
      I=I+1                                                             EIGEN052          
      GOTO 161                                                          EIGEN053          
169   CONTINUE                                                          EIGEN054          
      IT=IT-1                                                           EIGEN055          
      GOTO 1170                                                         EIGEN056          
280   MARK=1                                                            EIGEN057          
      K=1                                                               EIGEN058          
291   IF(K.GT.NM1) GOTO 299                                             EIGEN059          
      KP1=K+1                                                           EIGEN060          
      M=KP1                                                             EIGEN061          
311   IF(M.GT.N) GOTO 319                                               EIGEN062          
      H=0.0                                                             EIGEN063          
      G=0.0                                                             EIGEN064          
      HJ=0.0                                                            EIGEN065          
      YH=0.                                                             EIGEN066          
      I=1                                                               EIGEN067          
361   IF(I.GT.N) GOTO 369                                               EIGEN068          
      TE=A(I,K)*A(I,K)                                                  EIGEN069          
      TEE=A(I,M)*A(I,M)                                                 EIGEN070          
      YH=YH+TE-TEE                                                      EIGEN071          
      IF(I-K) 410,510,410                                               EIGEN072          
410   IF(I-M) 420,510,420                                               EIGEN073          
420   AIK=A(I,K)                                                        EIGEN074          
      AKI=A(K,I)                                                        EIGEN075          
      AIM=A(I,M)                                                        EIGEN076          
      AMI=A(M,I)                                                        EIGEN077          
      H=H+AKI*AMI-AIK*AIM                                               EIGEN078          
      TEP=AMI*AMI+TE                                                    EIGEN079          
      TEM=AKI*AKI+TEE                                                   EIGEN080          
      G=G+TEP+TEM                                                       EIGEN081          
      HJ=HJ-TEP+TEM                                                     EIGEN082          
510   CONTINUE                                                          EIGEN083          
      I=I+1                                                             EIGEN084          
      GOTO 361                                                          EIGEN085          
369   CONTINUE                                                          EIGEN086          
      H=2.0*H                                                           EIGEN087          
      D=A(K,K)-A(M,M)                                                   EIGEN088          
      TEP=A(K,M)                                                        EIGEN089          
      TEM=A(M,K)                                                        EIGEN090          
      C=TEP+TEM                                                         EIGEN091          
      E=TEP-TEM                                                         EIGEN092          
      IF(ABS(C)-EP) 590,590,620                                         EIGEN093          
590   CC=1.0                                                            EIGEN094          
      SS=0.0                                                            EIGEN095          
      GOTO 700                                                          EIGEN096          
620   BY=D/C                                                            EIGEN097          
      IF(BY) 640,660,660                                                EIGEN098          
640   SIG=-1.0                                                          EIGEN099          
      GOTO 670                                                          EIGEN100          
660   SIG=+1.0                                                          EIGEN101          
670   COT=BY+(SIG*SQRT(BY*BY+1.0))                                      EIGEN102          
      SS=SIG/SQRT(COT*COT+1.0)                                          EIGEN103          
      CC=SS*COT                                                         EIGEN104          
700   IF(YH) 710,710,740                                                EIGEN105          
710   TEM=CC                                                            EIGEN106          
      CC=SS                                                             EIGEN107          
      SS=-TEM                                                           EIGEN108          
740   TEP=CC*CC-SS*SS                                                   EIGEN109          
      TEM=2.0*SS*CC                                                     EIGEN110          
      D=D*TEP+C*TEM                                                     EIGEN111          
      H=H*TEP-HJ*TEM                                                    EIGEN112          
      DEN=G+2.0*(E*E+D*D)                                               EIGEN113          
      TEE=(E*D-H/2.)/DEN                                                EIGEN114          
      IF(ABS(TEE)-EP) 810,810,840                                       EIGEN115          
810   CH=1.0                                                            EIGEN116          
      SH=0.0                                                            EIGEN117          
      GOTO 860                                                          EIGEN118          
840   CH=1.0/SQRT(1.0-TEE*TEE)                                          EIGEN119          
      SH=CH*TEE                                                         EIGEN120          
860   C1=CH*CC-SH*SS                                                    EIGEN121          
      C2=CH*CC+SH*SS                                                    EIGEN122          
      S1=CH*SS+SH*CC                                                    EIGEN123          
      S2=-CH*SS+SH*CC                                                   EIGEN124          
      IF(ABS(S1)-EP) 910,910,920                                        EIGEN125          
910   IF(ABS(S2)-EP) 1150,1150,920                                      EIGEN126          
920   MARK=0                                                            EIGEN127          
      J=1                                                               EIGEN128          
931   IF(J.GT.N) GOTO 939                                               EIGEN129          
      TEP=A(K,J)                                                        EIGEN130          
      TEM=A(M,J)                                                        EIGEN131          
      A(K,J)=C1*TEP+S1*TEM                                              EIGEN132          
      A(M,J)=S2*TEP+C2*TEM                                              EIGEN133          
      J=J+1                                                             EIGEN134          
      GOTO 931                                                          EIGEN135          
939   CONTINUE                                                          EIGEN136          
      J=1                                                               EIGEN137          
981   IF(J.GT.N) GOTO 989                                               EIGEN138          
      TEP=A(J,K)                                                        EIGEN139          
      TEM=A(J,M)                                                        EIGEN140          
      A(J,K)=C2*TEP-S2*TEM                                              EIGEN141          
      A(J,M)=-S1*TEP+C1*TEM                                             EIGEN142          
      J=J+1                                                             EIGEN143          
      GOTO 981                                                          EIGEN144          
989   CONTINUE                                                          EIGEN145          
      IF(ISW) 1040,1150,1100                                            EIGEN146          
1040  J=1                                                               EIGEN147          
1041  IF(J.GT.N) GOTO 1049                                              EIGEN148          
      TEP=T(K,J)                                                        EIGEN149          
      TEM=T(M,J)                                                        EIGEN150          
      T(K,J)=C1*TEP+S1*TEM                                              EIGEN151          
      T(M,J)=S2*TEP+C2*TEM                                              EIGEN152          
      J=J+1                                                             EIGEN153          
      GOTO 1041                                                         EIGEN154          
1049  CONTINUE                                                          EIGEN155          
      GOTO 1150                                                         EIGEN156          
1100  J=1                                                               EIGEN157          
1101  IF(J.GT.N) GOTO 1109                                              EIGEN158          
      TEP=T(J,K)                                                        EIGEN159          
      TEM=T(J,M)                                                        EIGEN160          
      T(J,K)=C2*TEP-S2*TEM                                              EIGEN161          
      T(J,M)=-S1*TEP+C1*TEM                                             EIGEN162          
      J=J+1                                                             EIGEN163          
      GOTO 1101                                                         EIGEN164          
1109  CONTINUE                                                          EIGEN165          
1150  CONTINUE                                                          EIGEN166          
      M=M+1                                                             EIGEN167          
      GOTO 311                                                          EIGEN168          
319   CONTINUE                                                          EIGEN169          
      K=K+1                                                             EIGEN170          
      GOTO 291                                                          EIGEN171          
299   CONTINUE                                                          EIGEN172          
      IT=IT+1                                                           EIGEN173          
      GOTO 121                                                          EIGEN174          
129   CONTINUE                                                          EIGEN175          
1170  ISW=IT                                                            EIGEN176          
      CALL ZEROISE(A,N,EPS)                                             EIGEN177          
      RETURN                                                            EIGEN178          
      END                                                               EIGEN179          
      SUBROUTINE ZEROISE(A,N,EPS)                                       EIGEN180          
      DIMENSION A(40,40)                                                EIGEN181          
      I=1                                                               EIGEN182          
21    IF(I.GT.N) GOTO 29                                                EIGEN183          
      J=1                                                               EIGEN184          
31    IF(J.GT.N) GOTO 39                                                EIGEN185          
      IF(.NOT.(ABS(A(I,J)).LT.EPS)) GOTO 60                             EIGEN186          
      A(I,J)=0.                                                         EIGEN187          
60    CONTINUE                                                          EIGEN188          
      J=J+1                                                             EIGEN189          
      GOTO 31                                                           EIGEN190          
39    CONTINUE                                                          EIGEN191          
      I=I+1                                                             EIGEN192          
      GOTO 21                                                           EIGEN193          
29    CONTINUE                                                          EIGEN194          
      RETURN                                                            EIGEN195          
      END                                                               EIGEN196          
