c==========================================================================
      integer function ipsmin(vec,big,n)
c==========================================================================

      double precision big,vv,vec(n)

      vv=big
      do 1 i=1,n
      if(vec(i).lt.vv)then
         vv=vec(i)
         ii=i
      end if
1     continue
      ipsmin=ii
      return
      end

c==========================================================================
      integer function ipsmax(vec,big,n)
c==========================================================================

      double precision big,vv,vec(n)

      vv=-big
      do 1 i=1,n
      if(vec(i).gt.vv)then
         vv=vec(i)
         ii=i
      end if
1     continue
      ipsmax=ii
      return
      end

