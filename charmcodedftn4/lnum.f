      FUNCTION LNUM(ITM,IPOS)                                                             
C-----WRITTEN N.J. 1978                                                                   
C-----CONVERTS A MULTI-WORD CHARACTER STRING TO AN INTEGER VARIABLE                       
C-----+ OR - WILL BE DECODED PROVIDED NOT IN UNUTS POSITION OF STRING.                    
C-----SIGN NEED NOT BE IN HIGH ORDER POSN , BUT IF NOT ANY PRECEDING                      
C-----CHARS WILL BE IGNORED.                                                              
C-----DECIMEL POINT WILL LEAD TO ERROR MESSAGE AND JOBEND                                 
C-----NOTE FORMAT 8 IS MACHINE DEPENDENT                                                  
C-----BLANK WILL BE ASSUMED LEADING , + NO PRECEDING CHARS WILL BE DECODED                
C----- IPOS= LENGTH OF STRING                                                             
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      DIMENSION ITM(1)                                                                    
      IP=IPOS                                                                             
      NEG=+1                                                                              
      L=0                                                                                 
    1 ICH=IN(ITM,IP)                                                                      
      IT=ITYPE(ICH)                                                                       
      GO TO (3,4,5,6,3,2,3),IT                                                            
    4 ICH=JNUM(ICH)                                                                       
      GO TO 12                                                                            
    5 NEG=+1                                                                              
      GO TO 7                                                                             
    6 NEG=-1                                                                              
    7 IF(IP-1) 10,2,11                                                                    
   11 IF(IP-IPOS) 2,3,10                                                                  
    3 L=LWORDS(IPOS)                                                                      
      WRITE(LP,8) ICH,IP,(ITM(I),I=1,L)                                                   
    8 FORMAT(30H LNUM CANNOT DECODE CHARACTER ,A10,                                       
     1 14H  IN POSITION ,I4,10H OF STRING/(1H ,12A10))                                    
      CALL JOBEND                                                                         
   12 CONTINUE                                                                            
      L=L+ICH*10**(IPOS-IP)                                                               
      IP=IP-1                                                                             
      IF(IP) 10,2,1                                                                       
    2 LNUM=L*NEG                                                                          
      RETURN                                                                              
   10 CALL LOGIC(4HLNUM)                                                                  
      RETURN                                                                              
      END                                                                                 
