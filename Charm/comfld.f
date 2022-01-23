      subroutine comfld (rec,cf,knt,ib,il)
c-----make a composite of all the fields in a record for matching 
      dimension ib(50),il(50)
      character rec*1000,cf*100
      common /stdio/ lin,lout,lmess
      ip = 1
      do 1 i=1,knt
      j = ip+il(i)-1
      ie = ib(i)+il(i)-1
      if (j.gt.100) then
      write (lmess,10) i
   10 format (' ***fatal ***field',i4,' exceeds 100 character limit')
      call jobend
      end if
      cf(ip:j) = rec(ib(i):ie)
    1 ip = j+1
      return
      end
