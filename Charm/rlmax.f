      subroutine rlmax (idest,ifl,lemf,knt,rfdt,lmax)
c-----estimate new rl for reformed records.
      dimension idest(100),ifl(100),lemf(100),rfdt(100)
      character rfdt*1
      do 1 i = 1,knt
         if(rfdt(i).eq.'c')then
            itl = idest(i)+ifl(i)-1
         else
            itl = idest(i)+lemf(i)-1
         end if
         if (i.eq.1) then
            lmax =itl
         else if (itl.gt.lmax) then
            lmax = itl
         end if
    1 continue
      return
      end
