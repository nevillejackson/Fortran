      program match
c-----Written by anne swinton 23/9/85   latest update 25/9/85
c-----UNIX version 27/9/85 ... no "call colseq" UNIX only has ASCII.
c-----Extracts a composite field (according to match directives)
c-----from each record for matching.
c-----Matches primary file one record at a time against second- 
c-----ary file until a match is found or field on secondary
c-----exceeds that on primary (sort=ascending) .. ie. no match.
c-----Next primary record is then matched starting with the 
c-----last secondary record read (in case of non-match) or
c-----record following last matched secondary record.
c-----p(10),s(11),pm(12),sm(14),pnm(13),snm(15) 
c-----UNIX update 11/11/87 ... added ckseq for primary/secondary
c-----latest update 26/5/89 ... check for *m directive in sub matrd
      dimension ibp(50),ilp(50),ibs(50)
      character lit*1000,lit2*1000,comp*100,coms*100,compl*100,         
     +comsl*100
      logical rdend,rdends,order,seqc,offm,ready,offmat,prec 
      common /stdio/ lin,lout,lmess
      common /matlim/ md
      common /ca/ lit
      common /cb/ lit2
      common /cc/ comp
      common /cd/ coms
      common /ce/ compl
      common /cf/ comsl
      data comp,compl,coms,comsl/4*' '/
      data md/50/
      lti = 10
      lti2 = 11
      write (lmess,10)
   10 format ((/),' match run')
      call argopn (lti,1,'old')
      call argopn (lti2,2,'old')
      call argopn (12,3,'new')
      call argopn (14,4,'new')
      call argopn (13,5,'new')
      call argopn (15,6,'new')
      rdend = .false.
      rdends = .false.
      order = .false.
      seqc = .false.
      offm = .false.
      ready = .false.
      offmat = .false.
      prec = .false.
c-----read and count match directives 
      call matrd (knt,ibp,ilp,ibs,order,seqc,offm)
c-----set counts to zero
      ipk = 0
      isk = 0
      im = 0
      ims = 0
      inm = 0
      inms = 0
c-----read a record from primary input (unit 10) .. LOOP 1
    1 call readch (irl,iseq,lti,lit,rdend,ipk)
      if (rdend) then
         if (.not.rdends) then
            if (ready) then
               inms = inms+1
               call recwr (irl2,inms,lit2,15)
               ready = .false.
            end if
            goto 2
         else
            goto 4
         end if
      end if
      prec = .true.
c-----extract composite field for match with secondary input
      call comfld (lit,comp,knt,ibp,ilp)
c-----check primary file is in sequence
      if(ipk.gt.1.and.(seqc)) call ckseq(order,comp,compl,ipk,1,lti)
      compl = comp
      if (rdends) then
         inm = inm+1
         call recwr (irl,inm,lit,13)
         goto 1
      end if
c-----read a record from secondary input (unit 11) .. LOOP 2
      if (ready) goto 3
    2 call readch (irl2,iseq2,lti2,lit2,rdends,isk)
      if (rdends) then
         if (prec) then
            inm = inm+1
            call recwr (irl,inm,lit,13)
         end if
         if (rdend) goto 4
         goto 1
      end if
      if (isk.eq.1) then
         write (lmess,40) lti,irl
   40    format (' file unit',i3,' record length read =',i8,' characters
     +')
         write (lmess,40) lti2,irl2
         write (lmess,50) 12,irl
   50    format (' file unit',i3,' record length written =',i5, 
     +' characters')
         write (lmess,50) 13,irl
         write (lmess,50) 14,irl2
         write (lmess,50) 15,irl2
      end if
c-----extract composite field for match with primary input
      call comfld (lit2,coms,knt,ibs,ilp)
c-----check secondary file in sequence
      if(isk.gt.1.and.(seqc)) call ckseq(order,coms,comsl,isk,2,lti2)
      comsl = coms
      if (rdend) then
         inms = inms+1
         call recwr (irl2,inms,lit2,15)
         goto 2
      end if
c-----match primary input field with secondary input field
    3 ready = .false.
      prec = .false.
      if (comp.eq.coms) then
         im = im+1
         ims = ims+1
         call recwr (irl,im,lit,12)
         call recwr (irl2,ims,lit2,14)
         if (offm) then
            offmat = .true.
            goto 2
         end if
      else if (offmat) then
         offmat = .false.
         ready = .true.
      else if (order) then
         if (comp.gt.coms) then
            inms = inms+1
            call recwr (irl2,inms,lit2,15)
            prec = .true.
            goto 2
         else
            inm = inm+1
            call recwr (irl,inm,lit,13)
            ready = .true.
         end if
      else if (comp.lt.coms) then
         inms = inms+1
         call recwr (irl2,inms,lit2,15)
         prec = .true.
         goto 2
      else
         inm = inm+1
         call recwr (irl,inm,lit,13)
         ready = .true.
      end if
      goto 1
c-----match completed rewind files
    4 call eofr (lti,ipk)
      call eofr (lti2,isk)
      call eofw (12,im)
      call eofw (14,ims)
      call eofw (13,inm)
      call eofw (15,inms)
      stop
      end
