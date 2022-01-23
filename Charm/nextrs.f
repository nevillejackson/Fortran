      subroutine nextrs (ipos,del,string,par,len,check,del2,nonc
     +,ifmax,last)
      character del*1,string*80,par*10,del2*1
      logical check,nonc,last
      common /stdio/ lin,lout,lmess
      last = .false.
      lenp = 0
c-----check for special delimiters
    1 if(.not.check)goto 2
      if(string(ipos:ipos).eq.del2)then
      ipos = ipos+1
      check = .false.
      del2 = ' '
      if(string(ipos:ipos).eq.del)then
      goto 4
      else
      goto 97
      end if
      end if
c-----check for ordinary field delimiter
    2 if(string(ipos:ipos).eq.del)goto 4
      if (string(ipos:ipos).eq.' '.and.lenp.eq.ifmax) then
      last = .true.
      goto 4
      else if (lenp.eq.ifmax) then
c-----parameter string too long
      goto 97
      end if
      lenp = lenp+1
      par(lenp:lenp) = string(ipos:ipos)
      if (nonc) then
      call intck (par(lenp:lenp))
      end if
    3 ipos = ipos+1
      goto 1
c-----delimiter reached ... field complete
    4 ipos = ipos+1
      if(lenp.eq.0)goto 97
      len = lenp
      return
c-----non match bracket/missing delimiter/val length wrong
   97 write (lmess,10) string
   10 format (' ',a)
      write(lmess,20)
   20 format(' *** fatal *** invalid select directive')
      call jobend
      return
      end
