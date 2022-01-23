      character*(*) function axi(i,flag,iw,id)
      character*5 fmt
      character*2 flag
      write(fmt,1) iw 
    1 format('(i',i2,')')
      write(axi,fmt)i
      return
      end
