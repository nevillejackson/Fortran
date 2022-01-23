      subroutine blanks (kcc,ii,ie,cfhd)
c-----sets cols. 17-120 to blank
      dimension cfhd(2)
      character cfhd*120
      if (ii.eq.1) then
      do 1 j = ie,120
    1 cfhd(1)(j:j) = ' '
      if (kcc.gt.10) then
      do 2 j = 17,120
    2 cfhd(2)(j:j) = ' '
      end if
      else
      do 3 j = ie,120 
    3 cfhd(2)(j:j) = ' '
      end if
      return
      end
