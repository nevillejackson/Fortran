      subroutine exceed (m,n)                                                   
c------prints error message to output when program limits exceeded              
c------jobend called to abort job.                                              
      common /limits/ lim(5)                                                    
      common /stdio/ lin,lout,lmess                                             
      write (lmess,10) m,n,lim(m)                                               
   10 format(' limit ',i2,' exceeded -- value was',i10,' maximum'               
     +      ,i10)                                                               
      call jobend                                                               
      return                                                                    
      end                                                                       
