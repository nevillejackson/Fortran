      subroutine rltst (nb,nd,ipb,ipd,irl3)
c-----checks whether results returned by BXA routine will extend record
c-----length beyond initial length, sets new length if required.
      dimension ipb(100,3),ipd(100,2)
      if (nb.ge.1) then
      do 1 i = 1,nb
      irl = ipb(i,1)+ipb(i,2)-1
      if (irl.gt.irl3) irl3=irl
    1 continue
      end if
      if (nd.ge.1) then
      do 2 j = 1,nd
      irl = ipd(j,1)+ipd(j,2)-1
      if (irl.gt.irl3) irl3=irl
    2 continue
      end if
      return
      end
