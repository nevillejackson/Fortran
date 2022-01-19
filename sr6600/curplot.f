      SUBROUTINE CURPLOT(AUX,DS,SCX,SCY,T0,T1)                          CURVP002          
      CALL AUX(T0,X,Y,XT,YT)                                            CURVP003          
      CALL PLOT(X,Y,3)                                                  CURVP004          
      T=T0 $ PM1=SIGN(1.,T1-T0)                                         CURVP005          
60    Z=SQRT((XT/SCX)**2+(YT/SCY)**2)                                   CURVP006          
      IF(Z-1.E-5) 100,100,80                                            CURVP007          
80    DT=DS/Z $ GOTO 120                                                CURVP008          
100   CALL AUX(T+1.E-5*PM1,X,Y,XT,YT)                                   CURVP009          
      DT=SQRT(2.E-5*DS/((XT/X)**2+(YT/SCY)**2))                         CURVP010          
120   T=T+DT*PM1                                                        CURVP011          
      IF(PM1*(T-T1)) 140,170,170                                        CURVP012          
140   CALL AUX(T,X,Y,XT,YT)                                             CURVP013          
      CALL PLOT(X,Y,4)                                                  CURVP014          
      GOTO 60                                                           CURVP015          
170   CALL AUX(T1,X,Y,XT,YT)                                            CURVP016          
      CALL PLOT(X,Y,4)                                                  CURVP017          
      RETURN                                                            CURVP018          
      END                                                               CURVP019          
