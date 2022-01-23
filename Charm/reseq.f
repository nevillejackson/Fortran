      program reseq
c-----written for UNIX 29/7/86 by Anne Swinton
c-----program resequences a file, this is necessary only after UNIX sort
c-----as all CHARM programs resequence records automatically in writing
c-----them to output.
      character lit*1000 
      common /stdio/ lin,lout,lmess
      common /tio/ lti,lto
      common /limits/ lc,ll,lp,lr,nv
      common /zrec/ lit
      logical rdend
      data lit/1*' '/
      data lti,lto/10,11/
      write (lmess,10)
   10 format ((/),' reseq run')
      call argopn (lti,1,'old')
      call argopn (lto,2,'new')
      nseq = 0
    1 call readch (irl,iseq,lti,lit,rdend,nseq)
      if (rdend) goto 2
      call recwr (irl,nseq,lit,lto)
      goto 1
    2 call eofr (lti,nseq)
      call eofw (lto,nseq)
      stop
      end
