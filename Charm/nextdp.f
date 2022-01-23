      subroutine nextdp (ipos,del,string,const)
c-----reads constant for adding or multiplying as double precision 
c-----update 10/10/86 to handle -ve constant (*cm /*ca)
      character string*80,del*1,par*10,fmt*8
      double precision const
      logical point
      common /limcal/ ifl,iam,isub
      point = .false.
      lenp = 0
      k = 0
    1 if (string(ipos:ipos).eq.del) goto 10
      if (string(ipos:ipos).eq.'.') then
      point = .true.
      k = lenp
      lenp = lenp+1
      i = lenp
      else
      lenp = lenp+1
      k = k+1
      par(k:k) = string(ipos:ipos)
      if (k.eq.1.and.par(1:1).eq.'-') goto 2
      call intck (string(ipos:ipos))
      end if
    2 if (lenp.gt.ifl) call jobend
      ipos = ipos+1
      goto 1
   10 if (.not.point) then
      write (6,99)
   99 format (' *** decimal point required for *ca/*cm factor')
      call jobend
      end if
      j = lenp-i
      call dpchar (k,j,fmt)
      read(par(1:k),fmt)const
      ipos = ipos+1
      return
      end
