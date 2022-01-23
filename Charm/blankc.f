      subroutine blankc (thd,rchd,ksd,multi)
c-----sets cols. 17-120 to blank 
      dimension thd(2,50,4),rchd(2) 
      character thd*120,rchd*120
      logical multi
      do 1 j = 17,120 
    1 rchd(1)(j:j) = ' '
      do 2 j = 1,ksd
      do 3 k = 1,4
      do 4 l = 17,120 
    4 thd(1,j,k)(l:l) = ' ' 
    3 continue
    2 continue
      if (multi) then
      rchd(2) = rchd(1)
      do 5 j = 1,ksd
      do 6 k = 1,4
    6 thd(2,j,k) = thd(1,j,k)
    5 continue
      end if
      return
      end 
