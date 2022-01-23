      subroutine recoutc (rec,ibeg,ndec,ie,rparo,fmt,fmtn,knt)
c-----writing double precision *cm/*ca results to record as type char
      double precision rparo
      character rec*1000,fmt*8,fmtn*8 
      common /stdio/ lin,lout,lmess
      if (rparo.ge.0.0) then
      jparo = (rparo * 10**ndec) + 0.5
      write (rec(ibeg:ie),fmt)jparo 
      else
      jparo = (rparo * 10**ndec) - 0.5
      write (rec(ibeg:ie),fmtn)jparo
      end if
c-----check integer field (jparo) fitted format specified by user
      if (rec(ie:ie).eq.'*') goto 98
      return
   98 write (lmess,10) jparo,knt
   10 format (' specified field width inadequate for result',i10, 
     +' at record',i6)
      call jobend 
      return
      end
