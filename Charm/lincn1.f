      subroutine lincn1 (isn,lines)
c-----counts number of lines/record ... 120 char/line.
      intrinsic mod
      lines = 0
      lc = 120
      nl = isn/lc
      inl = mod(isn,lc)
      if (inl.eq.0) then
      lines = nl
      else
      lines = nl+1
      end if
      return
      end
