      subroutine dpchar (len,ndec,fmt)
c-----gives format for double precision/character conversion
      character fmt*8
      write (fmt,10) len,ndec
   10 format ('(d',i2,'.',i1,')')
      return
      end
