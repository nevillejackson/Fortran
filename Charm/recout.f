      subroutine recout (rec,bfld,bchar,nb,tb,fmt,ipb,ieb,fmtn, 
     +knt)
c-----writes double precision results to output record without decimal
      dimension bfld(100),bchar(100),tb(100),fmt(100),ipb(100,3), 
     +ieb(100),fmtn(100)
      character rec*1000,bchar*10,fmt*8,fmtn*8
      double precision bfld
      logical tb
      common /stdio/ lin,lout,lmess
      do 1 i = 1,nb
      if (tb(i)) then
      if (bfld(i).ge.0.0) then
      jfld = (bfld(i) * 10**ipb(i,3)) + 0.5
      write(rec(ipb(i,1):ieb(i)),fmt(i))jfld
      else
      jfld = (bfld(i) * 10**ipb(i,3)) - 0.5
      write(rec(ipb(i,1):ieb(i)),fmtn(i))jfld 
      end if
c-----check integer field (jfld) fitted format specified by user
      if(rec(ieb(i):ieb(i)).eq.'*') goto 98
      else
      rec(ipb(i,1):ieb(i)) = bchar(i)(1:ipb(i,2))
      end if
    1 continue
      return
   98 write (lmess,10) jfld,knt 
   10 format (' specified field width inadequate for result',i10, 
     +' at record',i6)
      call jobend
      return
      end
