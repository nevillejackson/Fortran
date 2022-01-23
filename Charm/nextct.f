      subroutine nextct (ipos,del,string,par,len,nonc)
      character string*80,par*50,del*1
      common /stdio/ lin,lout,lmess
      logical nonc
c-----breaks directive fields into destination,beginning,length.
      lenp = 0
    1 if(string(ipos:ipos).eq.del)goto 3
      lenp = lenp+1 
      par(lenp:lenp) = string(ipos:ipos)
c-----if integer field check that character is integer
      if (.not.nonc) goto 2 
      call intck (par(lenp:lenp)) 
    2 ipos = ipos+1 
      if(ipos.gt.50)goto 4
      goto 1
    3 ipos = ipos+1 
      if(lenp.eq.0)then
      write (lmess,20)
   20 format (' *** fatal error  *** field empty ***')
      call jobend
      end if
      len = lenp
      return
    4 write (lmess,30) string 
   30 format (' *** fatal error ***  missing delimiter ...',a)
      stop
      end 
