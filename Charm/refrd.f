      subroutine refrd (idest,iorb,iore,ifl,emf,lemf,rfdt,knt)
      dimension idest(100),iorb(100),iore(100),ifl(100),emf(100)
     +,lemf(100),rfdt(100)
      character string*80,rfdt*1,dest*3,orb*3,fl*3,emf*10
      common /limits/ lc,ll,lp,lr,nv
      common /stdio/ lin,lout,lmess
      common /rflim/ lrf
      logical nonc
      data dest,orb,fl/1*' ',1*' ',1*' '/
c-----reads and interprets reform directives.
c-----field to be emitted into new record .. max length 10 chars.
      i = 0
   10 read (lin,15,end=99,err=98,iostat=ios) string
   15 format (a)
      write (lmess,20) string
   20 format (' ',a)
      ipos = 4
      i = i+1
      if (i.gt.lrf) then
         write (lmess,25)
   25    format (' number of directives exceeds limit ... 100')
         call jobend
      end if
      if (string(1:3).eq.'*c,'.or.string(1:3).eq.'*e,') then
         rfdt(i) = string(2:2)
      else
         goto 97
      end if
      nonc = .true.
      call nextrf (ipos,',',string,dest,len,nonc)
      call chint (len,dest,idest(i))
      nonc = .false.
      if (rfdt(i).eq.'c') then
c-----'copy' directive, check characters all numeric (nextrf)
         nonc = .true.
         call nextrf (ipos,',',string,orb,len,nonc)
         call chint (len,orb,iorb(i))
         call nextrf (ipos,' ',string,fl,len,nonc)
         call chint (len,fl,ifl(i))
c-----parameters .. integer ... field beginning (iorb) and
c-----length (ifl) give end (iore)
         iore(i) = iorb(i)+ifl(i)-1
      else
c-----'emit' directive
         nonc = .false.
         rfdt(i) = 'e'
         if (string(ipos:ipos).ne.'''') goto 97
         ipos = ipos+1
         call nextrf (ipos,'''',string,emf(i),len,nonc)
         lemf(i) = len
         if (lemf(i).gt.lp) then
            call exceed (3,lemf(i))
         end if
      end if
      nonc = .false.
      goto 10
   97 write (lmess,35) i
   35 format (' directive no. ',i3,' not a reform directive')
      call jobend
      return
   98 call errr (lin,i,ios)
      return
c-----end of reform directives
   99 knt = i
      return
      end
