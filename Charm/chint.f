      subroutine chint (len,par,ipar)
c------gives integer format for internal file
c------converts type character parameter to integer
      character fmtci*4,par*4
      write (fmtci,10) len
   10 format ('(i',i1,')')
      read(par,fmtci)ipar
      return
      end
