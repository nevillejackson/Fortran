      integer function ixa(c)
      character *(*) c
      character*5 fmt
      write(fmt,1)len(c)
    1 format('(i',i2,')')
      read(c,fmt)ixa
      return
      end
