      subroutine pivot (x,n,m,liop,ok)
      implicit double precision (a-h,o-z)
      dimension x(mvntr4,mvntr4)
      logical ok
      common /limits/ mvltr,mvltr2,mvntr,mvnw,mvndg,mvns,mvntr4
      common /stdio/ lc,lp,lerr
c     does a single pivot step on the diagonal element x(m,m)
      if(absf(x(m,m)).le.0.0000001)go to 5
      do 1 i=1,n
        do 1 j=1,n
          if(i.eq.m.or.j.eq.m)go to 1
          x(i,j)=x(i,j)-x(i,m)*x(m,j)/x(m,m)
    1 continue
      do 2 i=1,n
        if(i.eq.m)go to 2
        x(i,m)=-x(i,m)/x(m,m)
    2 continue
      do 3 j=1,n
        if(j.eq.m)go to 3
        x(m,j)=x(m,j)/x(m,m)
    3 continue
      x(m,m)=1.0/x(m,m)
      ok=.true.
      return
    5 if(liop.eq.1) then
        write(lp,6)m
    6   format('resi3: pivot step zero',i3)
      endif
      ok=.false.
      return
      end
