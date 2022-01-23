      subroutine pivot (x,n,m,ok)
      implicit double precision (a-h,o-z)
      dimension x(mxn,mxn)
      logical ok
      common /limits/ mxn
      common /stdio/ lc,lp,lerr
c      check bounds
      if(n .gt. mxn) then
	write(lp,5) n 
    5   format('pivot: n exceeds max dimension', i4)
	ok = .false.
        return
c      check pivot not zero
      else if(absf(x(m,m)).le.0.0000001) then
        write(lp,6)m
    6   format('pivot: step zero',i3)
        ok=.false.
	return
c      a single pivot step on the diagonal element x(m,m)
      else
        do 1 i=1,n
          do 1 j=1,n
            if(i.eq.m.or.j.eq.m)go to 1
            x(i,j)=x(i,j)-x(i,m)*x(m,j)/x(m,m)
    1   continue
        do 2 i=1,n
          if(i.eq.m)go to 2
          x(i,m)=-x(i,m)/x(m,m)
    2   continue
        do 3 j=1,n
          if(j.eq.m)go to 3
          x(m,j)=x(m,j)/x(m,m)
    3   continue
        x(m,m)=1.0/x(m,m)
        ok=.true.
      end if
      return
      end
