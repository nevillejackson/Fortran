      subroutine selcomr (keep,litf,j,sdt,i,rel,val,nval)
      dimension sdt(50),rel(50,2),val(50,10),nval(50)
      character litf*80,sdt*1,rel*2,val*10
      logical keep,same
      common /stdio/ lin,lout,lmess
c-----checks record field against directive
      ll = nval(i)
      same = .false.
      if (rel(i,1).eq.'eq') then
      do 10 l=1,ll
      if (litf(1:j).eq.val(i,l)) goto 5
   10 continue
      goto 6
      else if (rel(i,1).eq.'lt'.and.litf(1:j).lt.val(i,1)) then
      goto 5
      else if (rel(i,1).eq.'gt'.and.litf(1:j).gt.val(i,1)) then
      if (rel(i,2).eq.'  ') then
      goto 5
      else if (litf(1:j).lt.val(i,2)) then
      goto 5
      else
      goto 6
      end if
      else
      goto 6
      end if
    5 same = .true.
c-----not selected ie. same (false)
    6 if (sdt(i).eq.'i'.and.same) then
      keep = .true.
      else if (sdt(i).eq.'e'.and.(.not.same))then
      keep = .true.
      else
      keep = .false.
      end if
      return
      end
