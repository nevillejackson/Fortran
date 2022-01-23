      subroutine setlen (string)
      character string*80
      common /limits/ lc,ll,lp,lr,nv
      do 1 i = 1,lc
    1 string(i:i) = ' '
      return
      end
