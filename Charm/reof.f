      subroutine reof (lun,k)                                                   
c------prints endfile message and record count on output file                   
c-----rewinds file and closes it.
      common /stdio/ lin,lout,lmess                                             
      write(lmess,10)lun,k                                                      
   10 format(' eof read on unit ',i3,' -- record count = ',i6)                  
      rewind lun
      return                                                                    
      end
