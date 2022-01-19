      SUBROUTINE PRNTPG(NPG,MXR,MAXCOL)                                                   
CI----MACHINE DEPENDENT ROUTINE - CYBER 76 VERSION                                        
      DIMENSION KPH(2), IFM(2)                                                            
      LEVEL 2, IPAGE                                                                      
      COMMON/ONE/IPAGE(14,70,80)                                                          
C---- WRITTEN R.E. 1977                                                                   
C---- USED BY PROGRAM SHEET                                                               
C---- WRITES PAGE NUMBER AND TOTAL NUMBER OF PAGES ON EACH PAGE                           
C---- AND WRITES COMPLETED IPAGE ARRAY ON TAPE11                                          
      IBEG=MAXCOL-15                                                                      
      CALL WRDPOS(IEND,IPOS,MAXCOL)                                                       
      NWD=IEND-1                                                                          
      ENCODE(15,2,IFM) NWD,IPOS                                                           
    2 FORMAT(5H(1H ,,I2,5HA10,A,I2,1H))                                                   
      DO 10 IPG=1,NPG                                                                     
      ENCODE(13,1,KPH)IPG,NPG                                                             
    1 FORMAT(5HPAGE ,I2,4H OF ,I2)                                                        
      CALL COPAGE(IBEG,1,IPG,KPH,1,13)                                                    
      WRITE(11,3)                                                                         
    3 FORMAT(1H1)                                                                         
      DO 9 J=1,MXR                                                                        
      WRITE(11,IFM) (IPAGE(I,J,IPG),I=1,IEND)                                             
    9 CONTINUE                                                                            
   10 CONTINUE                                                                            
      RETURN                                                                              
      END                                                                                 
