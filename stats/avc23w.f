      PROGRAM AVC23W(TAPE1,TAPE2,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                   
      DIMENSION FMT(12),TITLE(12),TSUM(6)                                                 
      DIMENSION RSS(6,6),CT(6,6),A(6,6),B(6),C(6,8),SSCP(6,6,9)                           
      DIMENSION SUMY(10,10,10),SUMX1(10,10,10),SUMX2(10,10,10)                            
      DIMENSION SUMX3(10,10,10),SUMX4(10,10,10),SUMX5(10,10,10)                           
      DIMENSION EMAIN(3,10,6),EM(10,10,10),EMI(10),EMJ(10),EMK(10)                        
      DIMENSION EIJ(10,10,6),EIK(10,10,6),EJK(10,10,6)                                    
      DIMENSION EMIJ(10,10),EMIK(10,10),EMJK(10,10)                                       
 1001 FORMAT(3I2,2I1,F5.0,I2)                                                             
 1002 FORMAT(12A6)                                                                        
 1004 FORMAT(40H1ANALYSIS OF VARIANCE AND COVARIANCE OF ,12A6)                            
      READ 1001,JOBS                                                                      
   10 READ 1002,(TITLE(I),I=1,12)                                                         
      PRINT 1004,(TITLE(I),I=1,12)                                                        
      READ 1001,NERR,NEQ                                                                  
      READ 1001,NI,NJ,NK,LV,NE,SEM,JOB                                                    
      LC=LV-1                                                                             
      NW=NE+1                                                                             
      PRINT 1003,NI,NJ,NK,LV,NE,SEM,JOB,NERR,NW,LC,NEQ                                    
 1003 FORMAT(6H0NI = ,I2,7H  NJ = ,I2,7H  NK = ,I2,7H  LV = ,I2,                          
     1   7H  NE = ,I2,8H  SEM = ,F6.0,8H  JOB = ,I2,8H NERR = ,I2,                        
     2   7H  NW = ,I2,7H  LC = ,I2,8H  NEQ = ,I2)                                         
      CALL READIN(RSS,CT,SUMY,SUMX1,SUMX2,SUMX3,SUMX4,SUMX5,                              
     2   EMAIN,EM,EMI,EMJ,EMK,TSUM,FMT,EIJ,EIK,EJK,EMIJ,EMIK,EMJK,                        
     3  NI,NJ,NK,NE,LV,SEM,LC,NW,A,B,NEQ)                                                 
      CALL DEVMAT(RSS,CT,SUMY,SUMX1,SUMX2,SUMX3,SUMX4,SUMX5,                              
     2   EM,EIJ,EIK,EJK,EMIJ,EMIK,EMJK,                                                   
     3   EMAIN,SSCP,EMI,EMJ,EMK,NI,NJ,NK,NE,LV,SEM,LC,NW)                                 
      IF(NERR-NW) 12,11,11                                                                
   12 CALL NEWERR(SSCP,LV,NW,NERR)                                                        
   11 CONTINUE                                                                            
      CALL ANOVA      (SSCP,NI,NJ,NK,NE,LV,SEM,LC,NW,C,A,B)                               
      CALL MEANS(SUMX1,SUMX2,SUMX3,SUMX4,SUMX5,EM,EMI,EMJ,EMK,SUMY,                       
     2 EMAIN,TSUM,EIJ,EIK,EJK,EMIJ,EMIK,EMJK,NI,NJ,NK,NE,LV,SEM,LC,NW,C)                  
      JOBS=JOBS-1                                                                         
      IF(JOBS) 9999,9999,10                                                               
 9999 STOP                                                                                
      END                                                                                 
