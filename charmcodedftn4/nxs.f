      FUNCTION NXS(ITM,IALF)                                                              
C-----WRITTEN N.J. 1978                                                                   
C-----CONVERTS LINKAGE FROM PARAM CARD COMMON BLOCK TO DUMMY ARGUMENTS                    
      COMMON /NXST/ KARD,KLEN,IPOS,ILEN,ITYP,IEND,KTM                                     
      DIMENSION KARD(8),ITM(1),KTM(8)                                                     
      NXS=NXST(KARD,KLEN,IPOS,ITM,ILEN,IALF,IEND)                                         
      RETURN                                                                              
      END                                                                                 
