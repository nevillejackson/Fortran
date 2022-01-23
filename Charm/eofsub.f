      subroutine eofrd (lun,reg)
c-----prints endfile message rewinds file and closes it
      character reg*50
      write (6,10) lun,reg
   10 format (' eof on unit ',i3,' file ',a20,' closed and kept')
      rewind lun
      close (lun,status='keep')
      return
      end
      subroutine eofwr (lun,reg,len)
c-----prints endfile message rewinds file closes and keeps it
      character reg*50
      write (6,10) lun,reg(1:len)
   10 format (' unit ',i3,' closed, updated and kept file = ',a)
      rewind lun
      close (lun,status='keep')
      return
      end
      subroutine eofnm (lun)
c-----rewinds and closes file no endfile message
      rewind lun 
      close (lun,status='keep')
      return
      end
