      FUNCTION BESINTJ(N,X)                                             BESSI002          
      DIMENSION TERM(600)                                               BESSI003          
C       COMPUTATION OF INTEGRAL OF JN(T) OVER (0,X).                    BESSI004          
C     METHOD-  SUMMATION OF BESSEL FUNCTION SERIES.                     BESSI005          
C     THE BESSEL FUNCTION ARE COMPUTED BY BACKWARD RECURRENCE WITH      BESSI006          
C     SUITABLE NORMALISATION.                                           BESSI007          
C                                                                       BESSI008          
C. H. J. JOHNSON,DIVISION OF APPLIED CHEM.,MELB. APRIL, 1969            BESSI009          
      IF(.NOT.(X.LT.0E0)) GOTO 50                                       BESSI010          
      BESINTJ=0 $ GOTO 260                                              BESSI011          
C     COMPUTE BESSEL FUNCTIONS.                                         BESSI012          
50    NPLUS1=N+1 $ NPLUS2=NPLUS1+1 $ M=IFIX(X)+N+N+N+25                 BESSI013          
      MM=M                                                              BESSI014          
      IF(.NOT.(MM.GT.600)) GOTO 120                                     BESSI015          
      WRITE(61,110)                                                     BESSI016          
      STOP                                                              BESSI017          
110   FORMAT(8H5       ,8H  DIMENS,8HION O/FL,8HOW IN SU,8HBR BESSI,    BESSI018          
     .3HNTJ)                                                            BESSI019          
120   TERM(M+1)=0. $ TERM(M)=1.0E-30 $ RDX=2./X                         BESSI020          
150   TERM(M-1)=-TERM(M+1)+(FLOAT(M-1)*RDX)*TERM(M)                     BESSI021          
      M=M-1 $ IF(.NOT.(M.EQ.1)) GOTO 150                                BESSI022          
      SUM=0.5*TERM(1)                                                   BESSI023          
      J=3                                                               BESSI024          
191   IF(J.GT.MM) GOTO 199                                              BESSI025          
      SUM=TERM(J)+SUM                                                   BESSI026          
      J=J+2                                                             BESSI027          
      GOTO 191                                                          BESSI028          
199   CONTINUE                                                          BESSI029          
      ALPHA=SUM                                                         BESSI030          
C     COMPUTE INTEGRAL FROM SERIES.                                     BESSI031          
      SUM=0.                                                            BESSI032          
      J=NPLUS2                                                          BESSI033          
231   IF(J.GT.MM) GOTO 239                                              BESSI034          
      SUM=TERM(J)+SUM                                                   BESSI035          
      J=J+2                                                             BESSI036          
      GOTO 231                                                          BESSI037          
239   CONTINUE                                                          BESSI038          
      BESINTJ=SUM/ALPHA                                                 BESSI039          
260   RETURN                                                            BESSI040          
      END                                                               BESSI041          
