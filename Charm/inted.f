      subroutine inted (lit,rec,irl,itp,icn,ptype,par,lenp,
     +ifield,nsf)
c-----intra-line editing for *s type directives
      dimension icn(10),ptype(10),par(10),lenp(10),ifield(10)
      character lit*1000,rec*1000,ptype*1,par*10
      common /stdio/ lin,lout,lmess
      j = 1
      jl = j
      do 1 i = 1,nsf
      k = icn(i)-1
      if (j.le.k) then
      kl = jl+(k-j)
      rec(j:k) = lit(jl:kl)
      else
      kl = jl-1
      end if
      j = k+1
      if (ptype(i).eq.'i') then
      k = k+lenp(i)
      rec(j:k) = par(i)(1:lenp(i))
      j = k+1
      jl = kl+1
      else if (ptype(i).eq.'r') then
      k = k+lenp(i)
      rec(j:k) = par(i)(1:lenp(i))
      j = k+1
      jl = kl+lenp(i)+1
      else if (ptype(i).eq.'d') then
      jl = kl+ifield(i)+1
      else if (ptype(i).eq.'b') then
      k = k+ifield(i)
      do 2 l = j,k
    2 rec(l:l) = ' '
      j = k+1
      jl = kl+ifield(i)+1
      else
      write (lmess,10) ptype(i)
   10 format (' *** fatal *** unrecognised *s parameter type ... ',a)
      call jobend
      end if
    1 continue
c-----intra-line edit completed, check current col. no's are
c-----ok then copy across remainder of record (if any).
      ml = irl+1
      if (j.gt.ml.or.jl.gt.ml) then
      write (lmess,20) ml,j,jl
   20 format (' ***fatal*** ml = ',i3,' new rl = ',i3,' at col. ',i3)
      call jobend
      else if (ml.eq.j) then
      goto 4
      else
      n = irl-j
      nn = irl-jl
      end if
      if (n.eq.nn) then
      rec(j:irl) = lit(jl:irl)
      else if (n.lt.nn) then
      jlt = jl+n
      rec(j:irl) = lit(jl:jlt)
      else
c-----n > nn
      jt = j+nn
      rec(j:jt) = lit(jl:irl)
      jt = jt+1
      do 3 m = jt,irl
    3 rec(m:m) = ' '
      end if
c-----check and add blank fill between 'irl' and 'itp' if needed
    4 if(itp.gt.irl) then
      ibeg = irl+1
      do 5 mm = ibeg,itp
    5 rec(mm:mm) = ' '
      end if
      return
      end
