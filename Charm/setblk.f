

      subroutine setblk(rec,len)
      character*1000 rec
      common /limits/lc,ll,lp,lr,nv
      if (len.le.lr) then
       do 1 i=1,len
       rec(i:i)=' '
    1  continue
      else
       call exceed(4,len)
      endif
      return
      end
