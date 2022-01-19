*TEXT                                                                                     
      SUBROUTINE EDIT(IC,IN,NED,IED,ICOD)                                                 
C-----RETURNS ICOD=0 IF ACCEPT CARD                                                       
C-----RETURNS ICOD=-1 IF MULL                                                             
C-----RETURNS ICOD=+1 IF REJECT CARD                                                      
      DIMENSION IC(1),IED(1)                                                              
      DATA(IMIN=1H-)                                                                      
      DATA(IBLK=1H )                                                                      
      DATA(IPLS=1H+)                                                                      
C-----LOOP OVER EDIT PARAMETER CARDS                                                      
      DO 101 I=1,NED                                                                      
C-----UNCRACK OPERATION CODE ( INPUT UNDER I14 )                                          
      IOP=IED(I)/1000000000000                                                            
C-----MAIN BRANCH TO CORRECT WAY OF UNCRACKING REST OF IED(I)                             
      GO TO (1,1,2,2,10,10,10),IOP+1                                                      
C-----UNCRACK REST FOR IOP=00 OR 01                                                       
    1 LENG=(IED(I)-IOP*1000000000000)/100000000000                                        
      LBEG=(IED(I)-IOP*1000000000000-LENG*100000000000)/100000000                         
      KEY=IED(I)-IOP*1000000000000-LENG*100000000000-LBEG*100000000                       
      LEND=LBEG+LENG-1                                                                    
      CALL FIELD(ICOD,LEND,IC,LBEG,J)                                                     
C-----TEST FOR MULL                                                                       
      IF(J) 104,102,104                                                                   
C-----SUB BRANCH TO CORRECT IOP                                                           
  102 GO TO (3,4),IOP+1                                                                   
C-----IOP=00 REJECT ON FIELD EQUALS KEY                                                   
    3 IF(ICOD-KEY) 101,103,101                                                            
C-----IOP=01 REJECT ON FIELD NOT EQUAL TO KEY                                             
    4 IF(ICOD-KEY) 103,101,103                                                            
C-----UNCRACK REST FOR IOP=02 OR 03                                                       
    2 LENG=(IED(I)-IOP*1000000000000)/100000000000                                        
      LBEG=(IED(I)-IOP*1000000000000-LENG*100000000000)/100000000                         
      MBEG=IED(I)-IOP*1000000000000-LENG*100000000000-LBEG*100000000                      
      MBEG=MBEG/100000                                                                    
      LEND=LBEG+LENG-1                                                                    
      KEY=MBEG-LBEG                                                                       
C-----SUB BRANCH TO CORRECT IOP                                                           
      GO TO (104,104,5,6),IOP+1                                                           
C-----IOP=02 SWAP FIELDS 1 AND 2                                                          
    5 DO 7 K=LBEG,LEND                                                                    
      L=K+KEY                                                                             
      ICOD=IC(K)                                                                          
      IC(K)=IC(L)                                                                         
    7 IC(L)=ICOD                                                                          
      GO TO 101                                                                           
C-----IOP=03 COPY FIELD 1 AND OVERWRITE FIELD 2                                           
    6 DO 8 K=LBEG,LEND                                                                    
      L=K+KEY                                                                             
    8 IC(L)=IC(K)                                                                         
      GO TO 101                                                                           
C-----UNCRACK REST FOR IOP = 04 OR 05                                                     
   10 LBEG=(IED(I)-IOP*1000000000000)/1000000000                                          
C-----SUB BRANCH TO CORRECT IOP                                                           
      GO TO (104,104,104,104,11,12,13),IOP+1                                              
C-----IOP=04 REJECT ON HOLLERITH MINUS                                                    
   11 IF(IMIN-IC(LBEG)) 101,103,101                                                       
C-----IOP=05 REJECT ON HOLLERITH BLANK                                                    
   12 IF(IBLK-IC(LBEG)) 101,103,101                                                       
C-----IOP=06 REJECT ON HOLLERITH PLUS                                                     
   13 IF(IPLS-IC(LBEG)) 101,103,101                                                       
C-----END PARAMETER CARDS LOOP                                                            
  101 CONTINUE                                                                            
C-----EXIT FOR ACCEPT CARD                                                                
      ICOD=0                                                                              
      RETURN                                                                              
C-----EXIT FOR MULL                                                                       
  104 ICOD=-1                                                                             
      RETURN                                                                              
C-----EXIT FOR REJECT                                                                     
  103 ICOD=1                                                                              
      RETURN                                                                              
      END                                                                                 
*ENDTEXT                                                                                  
