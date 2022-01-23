      subroutine nextc (ipos,del,string,par,len,last,nonc,j)
      character string*80,del*1,par*10
      logical last,nonc
      common /stdio/ lin,lout,lmess
      last = .false.
      lenp = 0
    1 if (string(ipos:ipos).eq.del) goto 3
      if (string(ipos:ipos).eq.' ') then
      last = .true.
      else
      lenp = lenp+1
      par(lenp:lenp) = string(ipos:ipos)
      ipos = ipos+1
      if (j.eq.3) then
      if (par(lenp:lenp).eq.'e'.or.par(lenp:lenp).eq.'o') then
      nonc = .false.
      goto 1
      end if
      end if
c-----integer field check 
      call intck (par(lenp:lenp))
      goto 1
      end if
    3 if (lenp.eq.0) then
      write (lmess,10)
   10 format (' *** fatal *** empty field')
      call jobend
      end if
      ipos = ipos+1
      len = lenp
      return
      end
