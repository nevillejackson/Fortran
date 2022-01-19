      SUBROUTINE BESZERO(NU,M,Z,BESJR)                                  BESZE002          
      REAL NU                                                           BESZE003          
      DIMENSION BESJR(1),Z(1)                                           BESZE004          
C   EVALUATION OF ZEROS OF J(Z),NON-NEGATIVE ORDER                      BESZE005          
C   WRITTEN BY G. KEADY, C.S.I.R.O., MELBOURNE, DECEMBER, 1967          BESZE006          
C   LAST REVISION OCTOBER 1968                                          BESZE007          
C   MUST BE USED IN CONJUNCTION WITH C3 CSIR GAMMA                      BESZE008          
C   BESZERO ALSO USES SUBROUTINES BESSJNUX AND LOMMEL BY G. KEADY       BESZE009          
C   NU IS (REAL) ORDER OF BESSEL FUNCTION                               BESZE010          
C   M IS THE NUMBER OF ZEROS REQUIRED                                   BESZE011          
C   Z RETURNS THE ZEROS REQUIRED - DIMENSION AT LEAST M                 BESZE012          
C   BESJR RETURNS THE VALUE OF THE FUNCTION J(NU)                       BESZE013          
C     ESTIMATE FIRST ZERO AND CALCULATE IT AND THE NEXT (M-1)           BESZE014          
C                                                                       BESZE015          
      KOUNT=0                                                           BESZE016          
      I=1                                                               BESZE017          
      IF(.NOT.(NU.LT.0E0)) GOTO 70                                      BESZE018          
      WRITE(61,60) NU                                                   BESZE019          
60    FORMAT(8H NU IS L,8HESS THAN,8H 0.   RE,8HSULTS UN,8HDEFINED,,    BESZE020          
     .4HNU= ,E15.8)                                                     BESZE021          
70    IF(.NOT.(NU.LT.1.)) GOTO 100                                      BESZE022          
      G=(2.4*(1.-NU)+3.8*NU)                                            BESZE023          
      GOTO 120                                                          BESZE024          
100   A=NU**(1.0/3.0)                                                   BESZE025          
      G=NU+1.8557571*A+1.033150/A                                       BESZE026          
120   IM=IFIX(NU)+1                                                     BESZE027          
130   C=.7978845608/SQRT(G)                                             BESZE028          
      MAX=IM+20                                                         BESZE029          
      M1=2*IFIX(G)                                                      BESZE030          
      IF(.NOT.(M1.GT.MAX)) GOTO 180                                     BESZE031          
      MAX=M1                                                            BESZE032          
180   CALL BESJNUX(G,NU,MAX,BESJR)                                      BESZE033          
C                                                                       BESZE034          
      IF(.NOT.(KOUNT.GE.10)) GOTO 230                                   BESZE035          
      WRITE(61,210) I,NU                                                BESZE036          
210   FORMAT(8H ITERATI,8HON IS NO,8HT CONVER,8HGING AT ,4HTHE ,I4,     BESZE037          
     .8HTH ZERO ,8HOF J NU,,3HNU=,F6.2)                                 BESZE038          
      GOTO 430                                                          BESZE039          
230   KOUNT=KOUNT+1                                                     BESZE040          
      III=0                                                             BESZE041          
      FUNC=BESJR(IM)                                                    BESZE042          
      DERIV=-BESJR(IM+1)+NU*FUNC/G                                      BESZE043          
      EST=G-FUNC/DERIV                                                  BESZE044          
      IF(.NOT.(ABS(EST-G)/G.LT.1.E-8)) GOTO 360                         BESZE045          
      Z(I)=EST                                                          BESZE046          
      KOUNT=0                                                           BESZE047          
      IF(.NOT.(I.GE.M)) GOTO 330                                        BESZE048          
      GOTO 430                                                          BESZE049          
330   I=I+1 $ G=EST+3.1416                                              BESZE050          
      GOTO 130                                                          BESZE051          
360   CALL LOMMEL(NU,MAX,G,EST,BESJR,FUNC,DERIV)                        BESZE052          
      III=III+1                                                         BESZE053          
      EST=EST-FUNC/DERIV                                                BESZE054          
      IF(ABS(FUNC/C).LT.1.E-7) GOTO 410                                 BESZE055          
      IF(III.LE.10) GOTO 360                                            BESZE056          
410   G=EST $ GOTO 180                                                  BESZE057          
430   RETURN                                                            BESZE058          
      END                                                               BESZE059          
      SUBROUTINE LOMMEL(NU,MAX,G,W,BESJR,FUNC,DERIV)                    BESZE060          
      REAL NU                                                           BESZE061          
      DIMENSION BESJR(1)                                                BESZE062          
C   WRITTEN BY G. KEADY, C.S.I.R.O., MELBOURNE, DECEMBER, 1967          BESZE063          
C   LAST REVISION OCTOBER 1968                                          BESZE064          
C     G=GOT , W=WANT                                                    BESZE065          
C     BESJR UP TO ORDER NU+MAX MUST BE GIVEN                            BESZE066          
      I=IFIX(NU)+2                                                      BESZE067          
      HOT=-.5*(W*W-G*G)                                                 BESZE068          
      MEND=MAX+I-1                                                      BESZE069          
      COEF=G**(-NU)                                                     BESZE070          
      SUMJ=COEF*BESJR(I-1)                                              BESZE071          
      SUMD=0.                                                           BESZE072          
      F=1.                                                              BESZE073          
      M=I                                                               BESZE074          
91    IF(M.GT.MEND) GOTO 99                                             BESZE075          
      COEG=COEF/G                                                       BESZE076          
      COEF=HOT*COEG/F                                                   BESZE077          
      A=COEF*BESJR(M)                                                   BESZE078          
      SUMJ=SUMJ+A                                                       BESZE079          
      SUMD=SUMD+COEG*BESJR(M)                                           BESZE080          
      IF(ABS(A/SUMJ).LT.1.E-9) GOTO 170                                 BESZE081          
160   F=F+1E0                                                           BESZE082          
      M=M+1                                                             BESZE083          
      GOTO 91                                                           BESZE084          
99    CONTINUE                                                          BESZE085          
170   F=W**NU                                                           BESZE086          
      FUNC=F*SUMJ                                                       BESZE087          
      DERIV=-F*SUMD*W+FUNC*NU/W                                         BESZE088          
      RETURN                                                            BESZE089          
      END                                                               BESZE090          
      SUBROUTINE BESJNUX(REZ,NU,MAX,BESJR)                              BESZE091          
      REAL NU                                                           BESZE092          
      INTEGER EXFLTF                                                    BESZE093          
      DIMENSION BESJR(1)                                                BESZE094          
C   WRITTEN BY G. KEADY, C.S.I.R.O., MELBOURNE, DECEMBER, 1967          BESZE095          
C   LAST REVISION OCTOBER 1968                                          BESZE096          
      IFLAG=0                                                           BESZE097          
      INU=IFIX(NU) $ N=IABS(INU)                                        BESZE098          
50    LN=(N+MAX+2)/2*2                                                  BESZE099          
      L=LN                                                              BESZE100          
      IF(.NOT.(N.GT.100.OR.REZ.GT.200.)) GOTO 130                       BESZE101          
      WRITE(61,80) REZ,NU                                               BESZE102          
80    FORMAT(//,10X,8HBESSJNUX,8H REJECT.,8H Z OR NU,8H OFF SCA,5HLE. , BESZE103          
     .,2E15.5)                                                          BESZE104          
90    IF(.NOT.(EXFLTF(J).EQ.2)) GOTO 110                                BESZE105          
      GOTO 730                                                          BESZE106          
110   IFLAG=IFLAG+1 $ GOTO 50                                           BESZE107          
130   J=EXFLTF(J)                                                       BESZE108          
      ZRR=1./REZ                                                        BESZE109          
      ZSQRR=4.*ZRR*ZRR                                                  BESZE110          
      IF(IFLAG-1) 220,170,190                                           BESZE111          
170   ABC=1.E-300                                                       BESZE112          
      AC=ABC                                                            BESZE113          
      GOTO 230                                                          BESZE114          
190   WRITE(61,210) $ GOTO 730                                          BESZE115          
210   FORMAT(8H OVERFLO,8HW HAS OC,8HCURRED A,8HFTER TWO,8H STARTIN,    BESZE116          
     .8HG ATTEMP,8HTS, CALL,8H REJECTE,8HD, RESUL,8HTS UNDEF,6HINED  )  BESZE117          
220   ABC=1.E-25                                                        BESZE118          
      AC=ABC                                                            BESZE119          
230   BA=0.                                                             BESZE120          
      AA=BA                                                             BESZE121          
      IF(ABS(NU-FLOAT(INU))-1.E-10) 280,280,410                         BESZE122          
250   BA=BA+2E0*AE                                                      BESZE123          
      AA=AC $ AC=AE                                                     BESZE124          
280   A=FLOAT(L)*FLOAT(L-1)*ZSQRR-2.*FLOAT(L)/FLOAT(L+1)                BESZE125          
      C=(1.-FLOAT(L))/FLOAT(1+L)                                        BESZE126          
      AE=A*AC+C*AA                                                      BESZE127          
      BESJR(L-1)=AE                                                     BESZE128          
      L=L-2                                                             BESZE129          
      IF(L.NE.0) GOTO 250                                               BESZE130          
      BA=BA+AE                                                          BESZE131          
      BESJR(1)=AE/BA                                                    BESZE132          
      L=1                                                               BESZE133          
370   BESJR(L+2)=BESJR(L+2)/BA                                          BESZE134          
      BESJR(L+1)=REZ*(BESJR(L)+BESJR(L+2))/FLOAT(2*L)                   BESZE135          
      L=L+2                                                             BESZE136          
      IF(L.GE.LN) GOTO 90                                               BESZE137          
      GOTO 370                                                          BESZE138          
C     FRACTIONAL ORDER                                                  BESZE139          
410   AAN=ABS(NU-FLOAT(INU))                                            BESZE140          
      BN=1.-AAN $ DD=AAN                                                BESZE141          
      II=L/2                                                            BESZE142          
      YMOD=(2./REZ)**DD*(DD+2E0*FLOAT(II))*GAMMA(DD+FLOAT(II))/GAMMA(   BESZE143          
     .FLOAT(II)+1.)                                                     BESZE144          
      II=II+1                                                           BESZE145          
      GOTO 530                                                          BESZE146          
480   X=(DD+2E0*FLOAT(II))*(DD+FLOAT(II)-1.)/(FLOAT(II)*(DD+2E0*FLOAT(  BESZE147          
     .II)-2.))                                                          BESZE148          
      YMOD=YMOD/X                                                       BESZE149          
      BA=BA+YMOD*AE                                                     BESZE150          
      AA=AC $ AC=AE                                                     BESZE151          
530   FLN=FLOAT(L)-BN+1.                                                BESZE152          
      A=FLN*(FLN-1.)*ZSQRR-2.*FLN/(FLN+1.)                              BESZE153          
      C=(1.-FLN)/(1.+FLN)                                               BESZE154          
      AE=A*AC+C*AA                                                      BESZE155          
      L=L-2 $ II=II-1                                                   BESZE156          
      BESJR(L+1)=AE                                                     BESZE157          
      IF(L) 610,610,480                                                 BESZE158          
610   X=(DD+2E0*FLOAT(II))*(DD+FLOAT(II)-1.)/(FLOAT(II)*(DD+2E0*FLOAT(  BESZE159          
     .II)-2.))                                                          BESZE160          
      YMOD=YMOD/X                                                       BESZE161          
      BA=BA+YMOD*AE                                                     BESZE162          
      FLN=AAN                                                           BESZE163          
      BESJR(1)=BESJR(1)/BA                                              BESZE164          
      FLN=FLN-1E0                                                       BESZE165          
      L=1                                                               BESZE166          
680   BESJR(L+2)=BESJR(L+2)/BA                                          BESZE167          
      FLN=FLN+2E0                                                       BESZE168          
      BESJR(L+1)=REZ*(BESJR(L)+BESJR(L+2))/(2.*FLN)                     BESZE169          
      L=L+2                                                             BESZE170          
      IF(L.GE.LN) GOTO 90                                               BESZE171          
      GOTO 680                                                          BESZE172          
730   RETURN                                                            BESZE173          
      END                                                               BESZE174          
