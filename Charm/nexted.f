      subroutine nexted (ipos,del,string,par,len,nonc,last)
      character del*1,string*80,par*10
      logical nonc,last
      common /stdio/ lin,lout,lmess
      common /limits/ lc,ll,lp,lr,nv
      last = .false.
      lenp = 0
c-----check for field delimiter
    1 if(string(ipos:ipos).ne.del)then
      if (lenp.eq.lp) goto 97
      lenp = lenp+1
      par(lenp:lenp) = string(ipos:ipos)
      if (nonc) call intck (par(lenp:lenp))
      ipos = ipos+1
      goto 1
      else
      len = lenp
      ipos = ipos+1
      if(lenp.eq.0)goto 97
      if (del.eq.')') then
      if (string(ipos:ipos).eq.',') then
      ipos = ipos+1
      else if (string(ipos:ipos).eq.' ') then
      last = .true.
      else
      goto 97
      end if
      end if
      end if
      return
c-----non match bracket/missing delimiter/val length wrong
   97 write(lmess,20)
   20 format(' *** fatal *** invalid editr directive')
      call jobend
      return
      end
