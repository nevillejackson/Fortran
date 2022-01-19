      FUNCTION RINT5(F,K,P)                                             RINT5002          
      DIMENSION F(1)                                                    RINT5003          
      R=P-.5                                                            RINT5004          
      R2=R*R                                                            RINT5005          
      R4=16E0*R2*R2                                                     RINT5006          
      RINT5=(F(K+1)*(1E0+2E0*R)+F(K)*(1E0-2E0*R))*(R4-136E0*R2+225E0)/  RINT5007          
     .384E0-(F(K+2)*(1E0+2E0*R/3E0)+F(K-1)*(1E0-2E0*R/3E0))*(R4-104E0*  RINT5008          
     .R2+25E0)/256E0+(F(K+3)*(1E0+.4*R)+F(K-2)*(1E0-.4*R))*(R4-40E0*R2+ RINT5009          
     .9E0)/768E0                                                        RINT5010          
      RETURN                                                            RINT5011          
      END                                                               RINT5012          
