      FUNCTION NXSTCM(KARD,KLEN,IPOS,ITM,ILEN,ITYP,IALF,IEND                              
     1 ,JTM,JLEN,JNO,JTYP,KTM)                                                            
C-----WRITTEN N.J. 1978                                                                   
C-----NEXT STRING COMPARISON                                                              
C-----                                                                                    
C-----ITM() - ARRAY CONTAINS NEXT STRING ON OUTPUT                                        
C-----IALF - 0=DECODE INTEGER STRINGS , 1=FORCE ALPHA FOR INTEGER STRINGS                 
C-----JTM() - ARRAY CONTAINS STRING OR INTEGER FOR COMPARISON , ON INPUT                  
C-----JLEN - LENGTH OF STRING FOR COMPARISON ON INPUT                                     
C-----JNO - NO OF ITEMS IN A MULTIPLE - ITEM COMPARISON STRING JTM()                      
C-----      IN THE CASE OF MULTIPLE ITEM STRINGS INDIVIDUAL ITEMS MAY                     
C-----      VARY IN LENGTH . ITEMS ARE COMPARED ONE-AT-A-TIME WITH IFM()                  
C-----      AND THE MATCHING CASE IS RETURNED VIA INO .                                   
C-----      ZERO MEANS OMIT THE COMPARISON AND JUST RETURN THE NEXT ITEM                  
C-----      WITH NXSTCM=0                                                                 
C-----JTYP - 0=SINGLE ITEM , 1=MULTIPLE ITEMS                                             
C-----NXSTCM - RETURNS WHEN JTYPE=0 -1 = ITM.LT.JTM                                       
C-----                               0 = ITM.EQ.JTM                                       
C-----                              +1 = ITM.GT.JTM                                       
C-----                 WHEN JTYP=1  0 = ITM.NE. ANY JTM                                   
C-----                              1 = ITM.EQ.ITEM 1 OF JTM                              
C-----                              2 = ITM.EQ.ITEM 2 OF JTM                              
C-----                              ETC.                                                  
C-----                                                                                    
C-----                                                                                    
C-----KARD( ) - CHAR STRING ARRAY BEING CRACKED                                           
C-----KLEN - LENGTH OF KARD() IN CHARS                                                    
C-----IPOS - POSITION MARKER WITHIN ARRAY KARD(),UPDATED ON RETURN                        
C-----ILEN - LENGTH OF NEXT STRING ON OUTPUT . UNDEF IF DECODED                           
C-----ITYP - CONTAINS TYPE OF NEXT STRING ON OUTPUT                                       
C-----      WHEN IALF=0  -1=ALPH 0=SPECIAL 1=NUMERIC                                      
C-----      WHEN IALF=1  -1=ALPHNUM 0=SPECIAL 1=$ALPHNUMSPEC$                             
C-----IEND - RETURNS  0=FOUND AN ITEM   1=END OF STRING KARD()                            
C-----KTM( ) - WORK AREA                                                                  
C-----                                                                                    
      DIMENSION KARD(1),ITM(1),JTM(1),KTM(1)                                              
C-----                                                                                    
C-----GET NEXT STRING                                                                     
      ITYP=NXST(KARD,KLEN,IPOS,ITM,ILEN,IALF,IEND)                                        
      IF(IEND) 5,2,5                                                                      
C-----                                                                                    
C-----SINGLE OR MULTIPLE ?                                                                
    2 IF(JTYP) 999,102,202                                                                
C-----                                                                                    
C-----SINGLE STRING COMPARE BRANCH                                                        
  102 IF(JNO-1) 11,3,999                                                                  
    3 IF(IALF) 999,8,9                                                                    
    8 IF(ITYP) 9,9,10                                                                     
C-----NUMERIC COMPARE                                                                     
   10 IF(ITM(1)-JTM(1)) 5,11,7                                                            
C-----ALPHA COMPARE                                                                       
    9 IF(ILEN-JLEN) 5,6,7                                                                 
    6 NXSTCM=IJSTCM(ITM,JTM,ILEN)                                                         
      GO TO 22                                                                            
    5 NXSTCM=-1                                                                           
      GO TO 22                                                                            
    7 NXSTCM=+1                                                                           
      GO TO 22                                                                            
C-----                                                                                    
C-----MULTIPLE STRING COMPARE BRANCH                                                      
  202 IF(JNO-1) 11,4,4                                                                    
    4 IF(IALF) 999,38,39                                                                  
   38 IF(ITYP) 39,39,40                                                                   
C-----ALPHA COMPARE MULTIPLE H STRING OF JNO ITEMS                                        
   39 DO 30 J=1,JNO                                                                       
      CALL JHS(JTM,JLEN,J,JNO,KTM,KL)                                                     
      IF(ILEN-KL)   30,36,30                                                              
   36 N=IJSTCM(ITM,KTM,ILEN)                                                              
      IF(N) 30,51,30                                                                      
   30 CONTINUE                                                                            
C-----NO MATCH                                                                            
      GO TO 11                                                                            
C-----MATCH                                                                               
   51 INO=J                                                                               
      NXSTCM=J                                                                            
      GO TO 22                                                                            
C-----INTEGER COMPARE MULTIPLE STRING OF JNO WORDS                                        
   40 DO 50 J=1,JNO                                                                       
      IF(ITM(1)-JTM(J)) 50,51,50                                                          
   50 CONTINUE                                                                            
      GO TO 11                                                                            
C-----                                                                                    
   11 NXSTCM=0                                                                            
   22 RETURN                                                                              
  999 CALL LOGIC(6HNXSTCM)                                                                
      RETURN                                                                              
      END                                                                                 
