      SUBROUTINE JOBEND                                                                   
C-----WRITTEN N.J. 1977                                                                   
C-----MACHINE DEPENDENT ROUTINE -- CYBER 76 VERSION                                       
C-----ABORTS WHOLE JOB , AS DISTINCT FROM A FORTRAN STOP STATEMENT                        
C-----WHICH ABORTS ONLY THE PRESENT JOB STEP                                              
C-----CALLED BY CHARM PROGRAMS WHEN AN ERROR IS SO FATAL THAT EXECUTION                   
C-----OF SUBSEQUENT PROGRAMS IN THE SAME JOB WOULD BE FUTILE                              
      CALL SYSTEM(52,13H JOBEND ERROR)                                                    
      RETURN                                                                              
      END                                                                                 
