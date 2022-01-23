      subroutine dpform (id,fmt)
c-----gives format for writing double precision to character in internal 
c-----file, retains decimal point and does not have leading zeros
      character fmt*7
      write (fmt,10) id
   10 format ('(D10.',i1,')')
      return
      end 
