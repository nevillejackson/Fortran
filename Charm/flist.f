      subroutine flist (ipb,ipl,ipd,kld,lit,iseq)
c-----lists named record field(s) to output with decimal points
c-----inserted if ipd > 1
      dimension ipb(10),ipl(10),ipd(10)
      character lit*1000,rec*100
      common /tio/ lti,lto,lto2
      data rec/1*' '/
      write (rec(2:7),'(i6)') iseq
      ie = 9
      do 1 i = 1,kld
      ie = ie+11
      ipe = ipb(i)+ipl(i)-1
      ib = ie+1-ipl(i)
      if (ipd(i).eq.0) then
c-----no digits after decimal .. decimal point not inserted
      rec(ib:ie) = lit(ipb(i):ipe)
      else
c-----decimal point to be inserted
      ib = ib-1
      id = ipl(i)-1-ipd(i)
      ip = ipb(i)+id
      ibe = ib+id
      rec(ib:ibe) = lit(ipb(i):ip)
      ibe = ibe+1
      rec(ibe:ibe) = '.'
      ibe = ibe+1
      ip = ip+1
      rec(ibe:ie) = lit(ip:ipe)
      end if
    1 continue
      write (lto,10) rec
   10 format (a)
      return
      end
