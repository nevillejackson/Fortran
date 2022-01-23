      subroutine fldckf (irl,ib,ie,dt,knt)
c-----checks that field being copied from does exceed input record length
      dimension ib(100),ie(100),dt(100)
      character dt*1
      common /stdio/ lin,lout,lmess
      do 1 i=1,knt
         if (dt(i).eq.'c') then
            if (ib(i).gt.irl.or.ie(i).gt.irl) then
               write (lmess,10) irl,ib(i),ie(i)
   10          format (' ***fatal*** rl =',i4,' cannot copy from',i4,' -
     +',i4)
               call jobend
            end if
         end if
    1 continue
      return
      end
