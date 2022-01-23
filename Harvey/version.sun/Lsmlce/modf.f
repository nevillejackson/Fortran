











      integer function modf(i1,i2)
      if(i2.eq.0) then
        modf = i1
      else
        modf = i1 - int(i1/i2)*i2
      endif
      return
      end
