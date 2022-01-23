      SUBROUTINE MOVER(X,I,J,IB,JB,NEL,IND,JND)                                           
C --- WRITTEN R.E. SEPT. 1980 TO AVOID ARRAY OVERWRITING BUG FOUND IN                     
C     LSML76. MOVES NEL ELEMENTS OF VECTOR X FROM OLD POSITION BEGINNING                  
C     JB TO NEW POSITION BEGINNING IB BY ASSIGNMENT X(I)=X(J).                            
C     I AND J ARE INDEX VARIABLES OF X.                                                   
C     IB AND JB INDICATE THE START POSITIONS OF THE VECTOR SEGMENTS AND                   
C     ARE SET WITHIN THIS SUBROUTINE. HOWEVER, IN THE CALL STATEMENT,                     
C     IB AND JB CAN CONTAIN EITHER THE START OR FINISH POSITIONS                          
C     DEPENDING ON IND AND JND.                                                           
C     IF IND IS ZERO, THEN I IS RETURNED AT ITS MINIMUM VALUE AND IB                      
C     INDICATES THE END POSITION OF THE VECTOR SEGMENT.                                   
C     IF IND IS NON-ZERO, THEN I IS RETURNED AT ITS MAXIMUM VALUE AND IB                  
C     INDICATES THE START POSITION.                                                       
C     SIMILARLY FOR JND AND JB.                                                           
      DIMENSION X(1)                                                                      
      IF(IND)10,15,10                                                                     
   15 IB=IB-NEL+1                                                                         
   10 IF(JND)20,25,20                                                                     
   25 JB=JB-NEL+1                                                                         
   20 IF(IB-JB)50,100,150                                                                 
   50 I=IB                                                                                
      J=JB                                                                                
      IE=IB+NEL-1                                                                         
   51 X(I)=X(J)                                                                           
      I=I+1                                                                               
      J=J+1                                                                               
      IF(I-IE)51,51,100                                                                   
  150 I=IB+NEL-1                                                                          
      J=JB+NEL-1                                                                          
  151 X(I)=X(J)                                                                           
      I=I-1                                                                               
      J=J-1                                                                               
      IF(I-IB)100,151,151                                                                 
  100 IF(IND)101,102,101                                                                  
  101 I=IB+NEL-1                                                                          
      GO TO 105                                                                           
  102 I=IB                                                                                
  105 IF(JND)106,107,106                                                                  
  106 J=JB+NEL-1                                                                          
      GO TO 110                                                                           
  107 J=JB                                                                                
  110 RETURN                                                                              
      END                                                                                 
