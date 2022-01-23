      subroutine nextsh (ipos,del,string,par,len,nonc,last)
c-----reads sheet directive parameters from "string", checks directive  
c-----length does not exceed 130 characters.
      character del*1,string*130,par*130
      logical nonc,last,null
      common /stdio/ lin,lout,lmess
      lenp = 0
      null = .false.
    1 if (string(ipos:ipos).ne.del) then
      if (string(ipos:ipos).eq.''''.and.lenp.eq.0) then
      if (del.eq.'=') then
      null = .true.
      else
      last = .true.
      end if
      nonc = .false.
      del = ''''
      ipos = ipos+1
      goto 1
      end if
      if (string(ipos:ipos).eq.','.and.del.eq.'=') then
      del = ','
      len = lenp
      ipos = ipos+1
      nonc = .true.
      call intck (par(lenp:lenp))
      return
      end if
      if (string(ipos:ipos).eq.' '.and.(.not.last)) then
      if (null) goto 2
      last = .true.
      len = lenp
      ipos = ipos+1
      return
      end if
      if (del.eq.'=') nonc = .false.
    2 lenp = lenp+1
      par(lenp:lenp) = string(ipos:ipos)
      if (nonc) then
      if (lenp.eq.1.and.par(1:1).eq.'-') goto 3
      call intck (par(lenp:lenp))
      end if
    3 ipos = ipos+1
      if (ipos.gt.130) call jobend
      goto 1
      else
      len = lenp
      ipos = ipos+1
      if (null) then
      if (string(ipos:ipos).eq.' ') then
      last = .true.
      return
      else if (string(ipos:ipos).ne.'=') then
      write (lmess,10)
   10 format (' *** fatal *equiv directive *** expected = at ipos',i3) 
      call jobend
      else
      del = ' '
      end if
      ipos = ipos+1
      end if
      if (lenp.eq.0) call jobend
      end if
      return
      end
