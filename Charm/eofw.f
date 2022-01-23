      subroutine eofw(lun,k)
c-----prints endfile message and record count on output file
c-----rewinds, ends and closes file.
      common /stdio/ lin,lout,lmess
      write (lmess,11) lun,k                                                    
   11 format(' eof written on unit ',i3,' -- record count = ',i6)               
      end file lun
      rewind lun
      close (lun,status='keep')
      return                                                                    
      end                                                                       
