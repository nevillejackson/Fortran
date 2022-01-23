    
      subroutine recwr1(irl,lun,rec,k)
      character rec*1000
      common /stdio/ lin,lout,lmess
      write(lun,20) irl,k,rec(1:irl)
   20 format(i3,i6,1x,a)
      return
      end
