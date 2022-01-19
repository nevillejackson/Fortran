      SUBROUTINE KIZNER(X,F,CA,KM)                                      KIZNE002          
C   D.P.MCKEE AND K.SERKOWSKA,DIV.OF COMP.RES.,CANBERRA.                KIZNE003          
C  TO MINIMISE NONLINEAR FUNCTION OF ONE VARIABLE                       KIZNE004          
C  BY THE METHOD OF KIZNER                                              KIZNE005          
C  X  IS INITIAL VALUE OF VARIABLE ON INPUT                             KIZNE006          
C  AND VALUE AT TERMINATION ON RETURN                                   KIZNE007          
C  F  IS VALUE OF FUNCTION ON RETURN                                    KIZNE008          
C  CA  IS THE ACCURACY REQUIRED BY THE USER. IE.SOLUTION FOUND WHEN     KIZNE009          
C  ABS(F(X)) IS LESS THAN CA.                                           KIZNE010          
C  KM  IS THE MAXIMUM NUMBER OF ITERATIONS TO BE USED.                  KIZNE011          
C  IF KM IS EXCEEDED WITHOUT A SOLUTION THE ROUTINE RETURNS KM NEGATIVE KIZNE012          
C  THE USER MUST PROVIDE THE SUBROUTINE GIVE(X,F) WHICH RETURNS F,      KIZNE013          
C  THE VALUE OF THE FUNCTION,FOR THE VALUE OF THE INDEPENDENT VARIABLE, KIZNE014          
C  AND WHICH HAS AN ENTRY POINT DERIV.                                  KIZNE015          
C  IF ENTRY KIZNERD IS USED,DERIV MUST RETURN THE VALUE OF THE DERIVATI KIZNE016          
      MIT=1                                                             KIZNE017          
      K=1                                                               KIZNE018          
31    IF(K.GT.KM) GOTO 39                                               KIZNE019          
      CALL GIVEF(X,F)                                                   KIZNE020          
      H=-F                                                              KIZNE021          
      IF(ABS(H).LT.CA) GOTO 310                                         KIZNE022          
      Y=X*1.E-8+1.E-9                                                   KIZNE023          
      XY=X+Y                                                            KIZNE024          
      CALL GIVEF(XY,FXY)                                                KIZNE025          
      A=H*Y/(FXY+H)                                                     KIZNE026          
      T=X+A/2.                                                          KIZNE027          
      Y=T*1.E-8+1.E-9                                                   KIZNE028          
      TY=T+Y                                                            KIZNE029          
      CALL GIVEF(TY,FTY)                                                KIZNE030          
      CALL GIVEF(T,FT)                                                  KIZNE031          
      B=H*Y/(FTY-FT)                                                    KIZNE032          
      T=X+B/2.                                                          KIZNE033          
      Y=T*1.E-8+1.E-9                                                   KIZNE034          
      TY=T+Y                                                            KIZNE035          
      CALL GIVEF(TY,FTY)                                                KIZNE036          
      CALL GIVEF(T,FT)                                                  KIZNE037          
      C=H*Y/(FTY-FT)                                                    KIZNE038          
      T=X+C                                                             KIZNE039          
      Y=T*1.E-8+1.E-9                                                   KIZNE040          
      TY=T+Y                                                            KIZNE041          
      CALL GIVEF(TY,FTY)                                                KIZNE042          
      CALL GIVEF(T,FT)                                                  KIZNE043          
      D=H*Y/(FTY-FT)                                                    KIZNE044          
      X=X+(A+B*2.+C*2.+D)/6.                                            KIZNE045          
      K=K+1                                                             KIZNE046          
      GOTO 31                                                           KIZNE047          
39    CONTINUE                                                          KIZNE048          
      MIT=-1                                                            KIZNE049          
310   KM=K*MIT                                                          KIZNE050          
      CA=-H                                                             KIZNE051          
      GOTO 550                                                          KIZNE052          
      ENTRY KIZNERD                                                     KIZNE053          
C DERIV(X,D) MUST RETURN THE VALUE OF THE DERIVATIVE.                   KIZNE054          
      MIT=1                                                             KIZNE055          
      K=1                                                               KIZNE056          
361   IF(K.GT.KM) GOTO 369                                              KIZNE057          
      CALL GIVEF(X,F)                                                   KIZNE058          
      H=-F                                                              KIZNE059          
      IF(ABS(H).LT.CA) GOTO 530                                         KIZNE060          
      CALL DERIV(X,D)                                                   KIZNE061          
      A=H/D                                                             KIZNE062          
      XA=X+A/2.                                                         KIZNE063          
      CALL DERIV(XA,DXA)                                                KIZNE064          
      B=H/DXA                                                           KIZNE065          
      XB=X+B/2.                                                         KIZNE066          
      CALL DERIV(XB,DXB)                                                KIZNE067          
      C=H/DXB                                                           KIZNE068          
      XC=X+C                                                            KIZNE069          
      CALL DERIV(XC,DXC)                                                KIZNE070          
      D=H/DXC                                                           KIZNE071          
      X=X+(A+B*2.+C*2.+D)/6.                                            KIZNE072          
      K=K+1                                                             KIZNE073          
      GOTO 361                                                          KIZNE074          
369   CONTINUE                                                          KIZNE075          
      MIT=-1                                                            KIZNE076          
530   KM=K*MIT                                                          KIZNE077          
      CA=-H                                                             KIZNE078          
550   RETURN                                                            KIZNE079          
      END                                                               KIZNE080          
