      subroutine pathse(p,q,r,xm,xmfix,dm,ndim,ii)
      implicit double precision (a-h,o-z)
      dimension p(40,40),r(40,40),q(40,40)
      dimension xm(40),xmfix(40),dm(40),b(40)
      if(ii.le.0) then
        do 2 i=1,ndim
          dm(i)=0.d0
          do 2 j=1,ndim
    2   dm(i)=dm(i)+r(i,j)*xmfix(j)
      else
        do 3 i=1,ndim
          b(i)=0.d0
          do 3 j=1,ndim
    3   b(i)=b(i)+q(i,j)*xmfix(j)
        do 5 i=1,ndim
    5   xmfix(i)=b(i)
        do 4 i=1,ndim
          dm(i)=0.d0
          do 4 j=1,ndim
    4   dm(i)=dm(i)+r(i,j)*b(j)+p(i,j)*xm(j)
      endif
      do 6 i=1,ndim
    6 xm(i)=dm(i)
      return
      end
