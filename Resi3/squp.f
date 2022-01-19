      subroutine squp(x,n)
      implicit double precision (a-h,o-z)
      dimension x(mvntr4,mvntr4)
      common /limits/ mvltr,mvltr2,mvntr,mvnw,mvndg,mvns,mvntr4
c     squares up a lower triangular matrix
      n1=n-1
      do 1 i=1,n1
      i1=i+1
      do 1 j=i1,n
    1 x(i,j)=x(j,i)
      return
      end
