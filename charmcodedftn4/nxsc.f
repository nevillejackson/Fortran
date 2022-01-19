      FUNCTION NXSC(ITM,IALF,JTM,JLEN,JNO,JTYP)                                           
C-----WRITTEN N.J. 1978                                                                   
C-----CONVERTS LINKAGE FROM PARAM CARD COMMON BLOCK TO DUMMY ARGUMENTS                    
      DIMENSION KARD(8),ITM(1),JTM(1),KTM(8)                                              
      COMMON /NXST/ KARD,KLEN,IPOS,ILEN,ITYP,IEND,KTM                                     
      NXSC=NXSTCM(KARD,KLEN,IPOS,ITM,ILEN,ITYP,IALF,IEND                                  
     1 ,JTM,JLEN,JNO,JTYP,KTM)                                                            
      RETURN                                                                              
      END                                                                                 
