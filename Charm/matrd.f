      subroutine matrd (knt,ibp,ilp,ibs,order,seqc,offm)
c-----reads ands counts "match" directives from input 
      dimension par(3),len(3),ipar(3),ibp(50),ilp(50),ibs(50) 
      character par*3,string*80
      logical order,seqc,offm,nonc,ndir
      common /stdio/ lin,lout,lmess
      common /matlim/ md
      knt = 0
      nonc = .true.
      ndir = .true.
      j = 0
    1 read (lin,10,end=99,err=98,iostat=ios) string
   10 format (a)
      write (lmess,20) string
   20 format (' ',a)
      knt = knt+1
      if (knt.gt.md) then
         write (lmess,30) md
   30    format (' *** fatal *** directive limit',i3,' exceeded')
         call jobend
      end if
      if (string(:3).eq.'*m,') then
         ndir = .false.
         j = j+1
         ipos = 4
         do 2 i = 1,3
            call nextrf (ipos,',',string,par(i),len(i),nonc)
    2    call chint (len(i),par(i),ipar(i))
         nonc = .false.
         ibp(j) = ipar(1)
         ilp(j) = ipar(2)
         ibs(j) = ipar(3)
         if (string(ipos:ipos).eq.'a') then
            order = .true.
         else if (string(ipos:ipos).ne.'d') then
            write (lmess,40)
   40       format (' *** fatal *** unrecognised sort order')
            call jobend
         end if
      else if (string(:7).eq.'*seqchk') then
         seqc = .true.
      else if (string(:7).eq.'*offset') then
         offm = .true.
         seqc = .true.
      else
         write (lmess,60)
   60    format (' *** fatal *** unrecognised "match" directive')
         call jobend
      end if
      goto 1
   98 call errr (lin,knt,ios)
      return
   99 knt = j
      if (ndir) then
         write (lmess,70)
   70    format (' *** fatal *** no *m directive(s)')
         call jobend
      end if
      return
      end
