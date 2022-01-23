      program pmain
      implicit double precision (a-h,o-z)
      common /stdio/ lc,lp,lerr
      common /limits/ mxn
      dimension x(20,20)
      logical ok
      data mxn /20/
      data lc,lp,lerr /5,6,6/

      write(lp,5)
    5 format('pmain:')
      read(lc,*)n
      write(lp,1)n
    1 format('n = ',i4)
      read(lc,*)((x(i,j),j=1,n),i=1,n)
      read(lc,*) m
      write(lp,2) m
    2 format('m = ',i4)
      call pivot(x,n,m,ok)
      write(lp,*) ok
c      if(ok) then
	write(lp,3)((x(i,j),j=1,n),i=1,n)
    3   format(8f8.2)
c      endif
      stop
      end
