      subroutine setlitf (litf)
      character litf*80
      common /limits/ lc,ll,lp,lr,nv
      do 1 i = 1,lp
    1 litf(i:i) = ' '
      return
      end
