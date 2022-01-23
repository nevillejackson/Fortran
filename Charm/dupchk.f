      program dupchk
c-----written by anne swinton 25/9/85 
c-----UNIX version 27/9/85 ... no "call colseq", only ASCII on UNIX.
c-----extracts a composite field (according to dupchk directives) 
c-----from each record for matching.
c-----matches a record with the one following ,unique records
c-----plus the first-occuring records of sets of records with
c-----identical fields are written to tape11. the remaining
c-----records .. duplicates,triplicates etc are written to
c-----tape12. then input file (tape10) is assumed to be pre-
c-----sorted on the user defined fields. input is sequence
c-----checked automatically.
c-----limit of 50 directives per run.
c-----  "   " 100 characters in a composite field for matching
c-----latest update 17/12/87 subroutine comfld fixed, 3 'call jobend' added
      dimension ibp(50),ilp(50) 
      character lit*1000,comp*100,compl*100,lit2*1000 
      logical rdend,order 
      common /stdio/ lin,lout,lmess
      common /duplim/ md
      data comp,compl/2*' '/
      data lit,lit2/2*' '/
      data md/50/
      lti = 10
      lto = 11
      lto2 = 12
      write (lmess,10)
   10 format ((/),' dupchk run')
      call argopn (lti,1,'old')
      call argopn (lto,2,'new')
      call argopn (lto2,3,'new')
      rdend = .false.
      order = .false.
c-----read and count dupchk directives
      call duprd (knt,ibp,ilp,order)
      ipk = 0
      iuk = 0 
      idk = 0
c-----read a record from input (tape 10)
    1 call readch (irl,iseq,lti,lit,rdend,ipk) 
      if (rdend) goto 3 
      if (ipk.eq.1) then
      write (lmess,15) lti,irl
   15 format(' file tape ',i3,' record length read =',i7,' characters')
      end if
c-----extract composite field for match with secondary input
      call comfld (lit,comp,knt,ibp,ilp)
      if (ipk.eq.1) then
      iuk = iuk+1
      call recwr (irl,iuk,lit,lto)
      lit2 = lit
      compl = comp
      write (lmess,16) lto,irl 
   16 format (' file tape ',i3,' record length written =',i5,
     +' characters')
      write (lmess,16) lto2,irl
      goto 1
      end if
c-----check ascending order .. current field/previous field 
      if ((order).and.comp.lt.compl) then 
      write (lmess,20) ipk,lti
   20 format ('*** fatal *** record',i6,' on tape',i3,
     +' out of sequence') 
      call jobend
c-----check descending order
      else if ((.not.order).and.comp.gt.compl) then 
      write (lmess,20) ipk,lti
      call jobend
c-----check for match .. duplicate record
      else if (comp.eq.compl) then
      idk = idk+1
      call recwr (irl,idk,lit,lto2)
c-----record does not match previous one, unique or first of
c-----another set.
      else
      iuk = iuk+1
      call recwr (irl,iuk,lit,lto)
      end if
      compl = comp
      lit2 = lit
      goto 1
c-----dupchk completed rewind files 
    3 call eofr (lti,ipk)
      call eofw (lto,iuk)
      call eofw (lto2,idk)
      stop
      end
