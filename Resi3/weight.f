      subroutine weight(new,ndg,idg,a,g,p,d,ntr,ntr4)
      implicit double precision (a-h,o-z)
      dimension a(mvnw,mvntr),d(mvndg,mvntr)
      dimension p(mvntr4,mvntr4),g(mvntr4,mvntr4)
      common /limits/ mvltr,mvltr2,mvntr,mvnw,mvndg,mvns,mvntr4
      common /stdio/ lc,lp,lerr
c-----computes implied weights w=ginv*p*ginv*d
c----- & stores in a(,) after economic weights
c-----note - uses ntr rows but ntr4 cols of G_inverse
c-----     - v.v. of (G_inverse)transposed
c-----     - because only I traits have non-zero economic weights
      do 3 i=1,ntr
      sumj=0.0
      do 4 j=1,ntr
      suml=0.0
      do 5 l=1,ntr4
      sumk=0.0
      do 6 k=1,ntr4
    6 sumk=sumk+g(i,k)*p(k,l)
    5 suml=suml+sumk*g(l,j)
    4 sumj=sumj+suml*d(idg,j)
    3 a(idg+new,i)=sumj
      return
      end
