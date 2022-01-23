      subroutine linest (itp,lines)
c-----converts record length (itp) to line count (chars/line
c-----equals length of card set by limits)
      intrinsic mod
      common /limits/ lc,ll,lp,lr,nv
      lines = 0
      nl = itp/lc
      inl = mod(itp,lc)
      if (inl.eq.0) then
      lines = nl
      else
      lines = nl+1
      end if
      return
      end
