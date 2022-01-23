      subroutine intch (len,fmt,fmtn) 
c-----provides format for integer/character conversion
      character fmt*8,fmtn*8
      write (fmt,10) len,len
   10 format ('(i',i2,'.',i2,')') 
      l = len-1
      write (fmtn,20) len,l
   20 format ('(i',i2,'.',i2,')')
      return
      end
