      SUBROUTINE TWODINT(F,G,M,N,K,L)                                   TWODI002          
      INTEGER Q,R,S,T,QPI,V                                             TWODI003          
      DIMENSION F(1),G(1)                                               TWODI004          
      EQUIVALENCE(NP1,NM1),(KM1,LM1)                                    TWODI005          
C         VERSION OF 21/7/67, 3200 COMPATIBLE                           TWODI006          
C         INITIALISE FIRST LOOP                                         TWODI007          
      U=1./FLOAT(K) $ NP1=N+1 $ KM1=K-1 $ MM1=M-1 $ MKP1=M*K+1          TWODI008          
      LMKP1=L*MKP1 $ S=1 $ Q=1 $ ASIXTH=1./6. $ MP1=M+1                 TWODI009          
C         FIRST PASS THROUGH MESH IN I DIRECTION                        TWODI010          
      J=1                                                               TWODI011          
121   IF(J.GT.NP1) GOTO 129                                             TWODI012          
      T=S+1 $ V=1 $ G(T-1)=F(Q)                                         TWODI013          
      F0=G(T-1)                                                         TWODI014          
      F1=F(Q+1) $ F2=F(Q+2)                                             TWODI015          
      R=1                                                               TWODI016          
181   IF(R.GT.KM1) GOTO 189                                             TWODI017          
      P=FLOAT(R)*U $ GOTO 910                                           TWODI018          
210   G(T)=FI                                                           TWODI019          
      T=T+1                                                             TWODI020          
      R=R+1                                                             TWODI021          
      GOTO 181                                                          TWODI022          
189   CONTINUE                                                          TWODI023          
      I=2                                                               TWODI024          
231   IF(I.GT.MM1) GOTO 239                                             TWODI025          
      QPI=Q+I $ FM1=F(QPI-2) $ F1=F(QPI)                                TWODI026          
      F2=F(QPI+1) $ F0=F(QPI-1)                                         TWODI027          
      G(T)=F0 $ T=T+1                                                   TWODI028          
      R=1                                                               TWODI029          
301   IF(R.GT.KM1) GOTO 309                                             TWODI030          
      P=FLOAT(R)*U $ GOTO 930                                           TWODI031          
330   G(T)=FI                                                           TWODI032          
      T=T+1                                                             TWODI033          
      R=R+1                                                             TWODI034          
      GOTO 301                                                          TWODI035          
309   CONTINUE                                                          TWODI036          
      I=I+1                                                             TWODI037          
      GOTO 231                                                          TWODI038          
239   CONTINUE                                                          TWODI039          
      Q=Q+MP1 $ V=2 $ F2=F(Q-3) $ F0=F(Q-1)                             TWODI040          
      F1=F(Q-2)                                                         TWODI041          
      G(T)=F1 $ T=T+1                                                   TWODI042          
C         APPLY FORMULA BACKWARDS AT OTHER END                          TWODI043          
      R=1                                                               TWODI044          
411   IF(R.GT.KM1) GOTO 419                                             TWODI045          
      P=1.-FLOAT(R)*U $ GOTO 910                                        TWODI046          
440   G(T)=FI                                                           TWODI047          
      T=T+1                                                             TWODI048          
      R=R+1                                                             TWODI049          
      GOTO 411                                                          TWODI050          
419   CONTINUE                                                          TWODI051          
      G(T)=F(Q-1)                                                       TWODI052          
      S=S+LMKP1                                                         TWODI053          
C         INITIALISE SECOND LOOP                                        TWODI054          
      J=J+1                                                             TWODI055          
      GOTO 121                                                          TWODI056          
129   CONTINUE                                                          TWODI057          
      U=1./FLOAT(L) $ LM1=L-1 $ NM1=N-1                                 TWODI058          
C         NOW PASS THRU MESH IN J DIRECTION                             TWODI059          
      Q=1                                                               TWODI060          
511   IF(Q.GT.MKP1) GOTO 519                                            TWODI061          
      F0=G(Q) $ R=Q+LMKP1 $ F1=G(R)                                     TWODI062          
      R=R+LMKP1 $ F2=G(R) $ V=3 $ T=Q+MKP1                              TWODI063          
      I=1                                                               TWODI064          
591   IF(I.GT.LM1) GOTO 599                                             TWODI065          
      P=FLOAT(I)*U $ GOTO 910                                           TWODI066          
620   G(T)=FI                                                           TWODI067          
      T=T+MKP1                                                          TWODI068          
      I=I+1                                                             TWODI069          
      GOTO 591                                                          TWODI070          
599   CONTINUE                                                          TWODI071          
      T=T+MKP1 $ V=2 $ R=Q                                              TWODI072          
      J=2                                                               TWODI073          
671   IF(J.GT.NM1) GOTO 679                                             TWODI074          
      FM1=G(R) $ S=R+LMKP1 $ F0=G(S)                                    TWODI075          
      S=S+LMKP1 $ F1=G(S) $ S=S+LMKP1 $ F2=G(S)                         TWODI076          
      I=1                                                               TWODI077          
751   IF(I.GT.LM1) GOTO 759                                             TWODI078          
      P=FLOAT(I)*U $ GOTO 930                                           TWODI079          
780   G(T)=FI                                                           TWODI080          
      T=T+MKP1                                                          TWODI081          
      I=I+1                                                             TWODI082          
      GOTO 751                                                          TWODI083          
759   CONTINUE                                                          TWODI084          
      T=T+MKP1                                                          TWODI085          
      R=R+LMKP1                                                         TWODI086          
      J=J+1                                                             TWODI087          
      GOTO 671                                                          TWODI088          
679   CONTINUE                                                          TWODI089          
      V=4 $ F0=F2 $ F2=G(R)                                             TWODI090          
      I=1                                                               TWODI091          
851   IF(I.GT.LM1) GOTO 859                                             TWODI092          
      P=1.-FLOAT(I)*U $ GOTO 910                                        TWODI093          
880   G(T)=FI                                                           TWODI094          
      T=T+MKP1                                                          TWODI095          
      I=I+1                                                             TWODI096          
      GOTO 851                                                          TWODI097          
859   CONTINUE                                                          TWODI098          
      Q=Q+1                                                             TWODI099          
      GOTO 511                                                          TWODI100          
519   CONTINUE                                                          TWODI101          
      GOTO 950                                                          TWODI102          
C         TWO "SUBROUTINES", RETURN ADDRESS IN V, PARAMETERS IN P,F0,   TWODI103          
C         ETC., RESULT IN FI                                            TWODI104          
C         THREE-POINT INTERPOLATION FORMULA                             TWODI105          
910   FI=.5*((2.-P)*(F0*(1.-P)+2.*P*F1)+P*(P-1.)*F2)                    TWODI106          
      KQZ001=V                                                          TWODI107          
      IF(KQZ001.LT.1) KQZ001=1                                          TWODI108          
      IF(KQZ001.GT.4) KQZ001=4                                          TWODI109          
      GOTO(210,440,620,880),KQZ001                                      TWODI110          
C         FOUR-POINT INTERPOLATION FORMULA                              TWODI111          
930   FI=ASIXTH*((F0*(1.-P)+F1*P)*(6.-3.*P*(P-1.))+P*(P-1.)*(FM1*(2.-P) TWODI112          
     .+F2*(P+1.))) $ KQZ001=V                                           TWODI113          
      IF(KQZ001.LT.1) KQZ001=1                                          TWODI114          
      IF(KQZ001.GT.2) KQZ001=2                                          TWODI115          
      GOTO(330,780),KQZ001                                              TWODI116          
950   RETURN                                                            TWODI117          
      END                                                               TWODI118          
