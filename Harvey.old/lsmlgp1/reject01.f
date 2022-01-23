*TEXT                                                                                     
      SUBROUTINE REJECT(IC,IN,NSKF,LSKF,LBEG,IREJ,ICOD)                 M0218000          
C     RETURNS ICOD=0 IF CARD OK -- ICOD=1 IF REJECT -- ICOD=-1 IF MULL  M0218001          
C     -------------------------------------------------                 M0218002          
      DIMENSION IC(480),LSKF(20),LBEG(20),IREJ(20)                      M0218003          
      DO 1 I=1,NSKF                                                     M0218004          
      KB=LBEG(I)                                                        M0218005          
      KE=KB+LSKF(I)-1                                                   M0218006          
      CALL FIELD(ICOD,KE,IC,KB,J)                                       M0218007          
      IF(J) 4,2,4                                                       M0218008          
    2 IF(ICOD-IREJ(I)) 1,3,1                                            M0218009          
    1 CONTINUE                                                          M0218010          
      ICOD=0                                                            M0218011          
      RETURN                                                            M0218012          
    3 ICOD=1                                                            M0218013          
      RETURN                                                            M0218014          
    4 ICOD=-1                                                           M0218015          
      RETURN                                                            M0218016          
      END                                                               M0218017          
*ENDTEXT                                                                                  
