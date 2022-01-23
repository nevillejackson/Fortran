    
      subroutine recrd1(irl,lun,rec,rdend,l)
      character rec*1000
      logical rdend
      common /stdio/ lin,lout,lmess 
      rdend=.false.
      read(lun,10,err=98,end=99,iostat=ios) rec(1:irl),l
   10 format(a,i10)
      return
   98 call errr(lun,l,ios)
      call jobend
      return
   99 rdend=.true.
      return
      end
