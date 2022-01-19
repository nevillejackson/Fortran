      SUBROUTINE MINFUN(ITEST,X,F,MAXFUN,ABSACC,RELACC,XSTEP)           MINFU002          
C5 CSIR MINFUN - MINIMIZATION OF FUNCTION OF ONE VARIABLE               MINFU003          
C  P.J.ROSS, CSIRO BRISBANE, SEPTEMBER 1969                             MINFU004          
      KQZ001=ITEST                                                      MINFU005          
      IF(KQZ001.LT.1) KQZ001=1                                          MINFU006          
      IF(KQZ001.GT.3) KQZ001=3                                          MINFU007          
      GOTO(180,30,30),KQZ001                                            MINFU008          
30    IS=6-ITEST                                                        MINFU009          
      ITEST=1                                                           MINFU010          
      IINC=1                                                            MINFU011          
      XINC=XSTEP+XSTEP                                                  MINFU012          
      MC=IS-3                                                           MINFU013          
      IF(MC) 200,200,170                                                MINFU014          
90    MC=MC+1                                                           MINFU015          
      IF(MAXFUN-MC) 110,170,170                                         MINFU016          
110   ITEST=4                                                           MINFU017          
120   X=DB                                                              MINFU018          
      F=FB                                                              MINFU019          
      IF(FB-FC) 170,170,150                                             MINFU020          
150   X=DC                                                              MINFU021          
      F=FC                                                              MINFU022          
170   GOTO 960                                                          MINFU023          
180   KQZ001=IS                                                         MINFU024          
      IF(KQZ001.LT.1) KQZ001=1                                          MINFU025          
      IF(KQZ001.GT.4) KQZ001=4                                          MINFU026          
      GOTO(460,390,240,190),KQZ001                                      MINFU027          
190   IS=3                                                              MINFU028          
200   DC=X                                                              MINFU029          
      FC=F                                                              MINFU030          
      X=X+XSTEP                                                         MINFU031          
      GOTO 90                                                           MINFU032          
240   IF(FC-F) 280,250,320                                              MINFU033          
250   X=X+XINC                                                          MINFU034          
      XINC=XINC+XINC                                                    MINFU035          
      GOTO 90                                                           MINFU036          
280   DB=X                                                              MINFU037          
      FB=F                                                              MINFU038          
      XINC=-XINC                                                        MINFU039          
      GOTO 360                                                          MINFU040          
320   DB=DC                                                             MINFU041          
      FB=FC                                                             MINFU042          
      DC=X                                                              MINFU043          
      FC=F                                                              MINFU044          
360   X=DC+DC-DB                                                        MINFU045          
      IS=2                                                              MINFU046          
      GOTO 90                                                           MINFU047          
390   DA=DB                                                             MINFU048          
      DB=DC                                                             MINFU049          
      FA=FB                                                             MINFU050          
      FB=FC                                                             MINFU051          
430   DC=X                                                              MINFU052          
      FC=F                                                              MINFU053          
      GOTO 680                                                          MINFU054          
460   IF(FB-FC) 530,470,470                                             MINFU055          
470   IF(F-FB) 480,430,430                                              MINFU056          
480   FA=FB                                                             MINFU057          
      DA=DB                                                             MINFU058          
500   FB=F                                                              MINFU059          
      DB=X                                                              MINFU060          
      GOTO 680                                                          MINFU061          
530   IF(FA-FC) 600,600,540                                             MINFU062          
540   XINC=FA                                                           MINFU063          
      FA=FC                                                             MINFU064          
      FC=XINC                                                           MINFU065          
      XINC=DA                                                           MINFU066          
      DA=DC                                                             MINFU067          
      DC=XINC                                                           MINFU068          
600   XINC=DC                                                           MINFU069          
      IF((D-DB)*(D-DC)) 430,620,620                                     MINFU070          
620   IF(F-FA) 630,660,660                                              MINFU071          
630   FC=FB                                                             MINFU072          
      DC=DB                                                             MINFU073          
      GOTO 500                                                          MINFU074          
660   FA=F                                                              MINFU075          
      DA=X                                                              MINFU076          
680   IF(FB-FC) 690,690,720                                             MINFU077          
690   IINC=2                                                            MINFU078          
      XINC=DC                                                           MINFU079          
      IF(FB-FC) 720,920,720                                             MINFU080          
720   D=(FA-FB)/(DA-DB)-(FA-FC)/(DA-DC)                                 MINFU081          
      IF(D*(DB-DC)) 850,850,740                                         MINFU082          
740   D=0.5*(DB+DC-(FB-FC)/D)                                           MINFU083          
      IF(ABS(D-X)-ABS(ABSACC)) 770,770,760                              MINFU084          
760   IF(ABS(D-X)-ABS(D*RELACC)) 770,770,790                            MINFU085          
770   ITEST=2                                                           MINFU086          
      GOTO 120                                                          MINFU087          
790   IS=1                                                              MINFU088          
      X=D                                                               MINFU089          
      IF((DA-DC)*(DC-D)) 90,940,820                                     MINFU090          
820   IS=2                                                              MINFU091          
      KQZ001=IINC                                                       MINFU092          
      IF(KQZ001.LT.1) KQZ001=1                                          MINFU093          
      IF(KQZ001.GT.2) KQZ001=2                                          MINFU094          
      GOTO(840,890),KQZ001                                              MINFU095          
840   IF(ABS(XINC)-ABS(DC-D)) 870,90,90                                 MINFU096          
850   IS=2                                                              MINFU097          
      KQZ001=IINC                                                       MINFU098          
      IF(KQZ001.LT.1) KQZ001=1                                          MINFU099          
      IF(KQZ001.GT.2) KQZ001=2                                          MINFU100          
      GOTO(870,900),KQZ001                                              MINFU101          
870   X=DC                                                              MINFU102          
      GOTO 250                                                          MINFU103          
890   IF(ABS(XINC-X)-ABS(X-DC)) 900,900,90                              MINFU104          
900   X=0.5*(XINC+DC)                                                   MINFU105          
      IF((XINC-X)*(X-DC)) 940,940,90                                    MINFU106          
920   X=0.5*(DB+DC)                                                     MINFU107          
      IF((DB-X)*(X-DC)) 940,940,90                                      MINFU108          
940   ITEST=3                                                           MINFU109          
      GOTO 120                                                          MINFU110          
960   RETURN                                                            MINFU111          
      END                                                               MINFU112          
