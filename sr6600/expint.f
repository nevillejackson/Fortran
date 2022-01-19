      FUNCTION EXPINT(X)                                                EXPIN002          
      DIMENSION A(18)                                                   EXPIN003          
C ROUTINE      C3 CSIR EXPINT            DATE MAR 65              002501EXPIN004          
C TITLE        EXPONENTIAL INTEGRAL                               002502EXPIN005          
C DESCRIPTION  TO EVALUATE THE EXPONENTIAL INTEGRAL.              002503EXPIN006          
C LANGUAGE     32/36FTN                  AVAILABLE FROM SRLIST    002504EXPIN007          
C AVAILABLE AS (LISTING, SOURCE DECK, WRITEUP)                    002505EXPIN008          
C AUTHOR       J.J.RUSSELL, DIVISION OF COMPUTING RESEARCH,       002506EXPIN009          
C              C.S.I.R.O., CANBERRA, AUSTRALIA.                   002507EXPIN010          
C KEYWORDS     EXPONENTIAL,INTEGRAL                               002508EXPIN011          
C SEE ALSO     -                                                  002509EXPIN012          
      T0=1                                                              EXPIN013          
      IF(ABS(X)-4E0) 40,270,270                                         EXPIN014          
40    A(1)=-8.031487429                                                 EXPIN015          
      A(2)=3.8797325769                                                 EXPIN016          
      A(3)=-1.6042971073                                                EXPIN017          
      A(4)=5.630905454E-1                                               EXPIN018          
      A(5)=-1.7044230174E-1                                             EXPIN019          
      A(6)=4.5209939002E-2                                              EXPIN020          
      A(7)=-1.0653898644E-2                                             EXPIN021          
      A(8)=2.2562638123E-3                                              EXPIN022          
      A(9)=-4.3357004732E-4                                             EXPIN023          
      A(10)=7.621668119E-5                                              EXPIN024          
      A(11)=-1.2341744306E-5                                            EXPIN025          
      A(12)=1.8519745698E-6                                             EXPIN026          
      A(13)=-2.5886986618E-7                                            EXPIN027          
      A(14)=3.3860431857E-8                                             EXPIN028          
      A(15)=-4.1611417804E-9                                            EXPIN029          
      A(16)=4.8216060614E-10                                            EXPIN030          
      A(17)=-5.284647758E-11                                            EXPIN031          
      A(18)=5.49454166E-12                                              EXPIN032          
      EXPINT=3.9368857696                                               EXPIN033          
      T1=X/4E0                                                          EXPIN034          
      Y=T1                                                              EXPIN035          
      GOTO 470                                                          EXPIN036          
270   A(1)=1.0281062152E-1                                              EXPIN037          
      A(2)=-4.5526707132E-3                                             EXPIN038          
      A(3)=3.5716131229E-4                                              EXPIN039          
      A(4)=-3.7934161693E-5                                             EXPIN040          
      A(5)=4.9143944914E-6                                              EXPIN041          
      A(6)=-7.355024922E-7                                              EXPIN042          
      A(7)=1.2306036058E-7                                              EXPIN043          
      A(8)=-2.252369069E-8                                              EXPIN044          
      A(9)=4.4412374605E-9                                              EXPIN045          
      A(10)=-9.32850854E-10                                             EXPIN046          
      A(11)=2.0692969757E-10                                            EXPIN047          
      A(12)=-4.815022663E-11                                            EXPIN048          
      A(13)=1.168906859E-11                                             EXPIN049          
      A(14)=-2.94739043E-12                                             EXPIN050          
      A(15)=7.6907322E-13                                               EXPIN051          
      A(16)=-2.0702314E-13                                              EXPIN052          
      A(17)=5.733765E-14                                                EXPIN053          
      A(18)=-1.630195E-14                                               EXPIN054          
      EXPINT=.10776418884                                               EXPIN055          
      T1=8E0/X-1E0                                                      EXPIN056          
      Y=T1                                                              EXPIN057          
470   J=1                                                               EXPIN058          
471   IF(J.GT.18) GOTO 479                                              EXPIN059          
      EXPINT=EXPINT+A(J)*T1                                             EXPIN060          
      T2=2E0*Y*T1-T0                                                    EXPIN061          
      T0=T1                                                             EXPIN062          
      T1=T2                                                             EXPIN063          
      J=J+1                                                             EXPIN064          
      GOTO 471                                                          EXPIN065          
479   CONTINUE                                                          EXPIN066          
      IF(ABS(X)-4E0) 530,550,550                                        EXPIN067          
530   EXPINT=-(ALOG(ABS(X))+EXPINT)                                     EXPIN068          
      GOTO 560                                                          EXPIN069          
550   EXPINT=EXPINT*EXP(-X)                                             EXPIN070          
560   RETURN                                                            EXPIN071          
      END                                                               EXPIN072          
