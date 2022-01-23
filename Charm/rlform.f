      subroutine rlform (id,fmt)
c-----gives format for writing real to character in internal file
c-----retains decimal point and does not have leading zeros
      character fmt*7
      write (fmt,10) id
   10 format ('(f10.',i1,')')
      return
      end
