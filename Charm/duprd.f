      subroutine duprd (knt,ibp,ilp,order)
c-----reads ands counts "dupchk" directives from input
      dimension par(2),len(2),ipar(2),ibp(50),ilp(50) 
      character par*3,string*80
      logical order,nonc
      common /stdio/ lin,lout,lmess
      common /duplim/ md
      nonc = .true.
      j = 0
    1 read (lin,10,end=99,err=98,iostat=ios) string
   10 format (a)
      write (lmess,20) string
   20 format (' ',a)
      if (knt.gt.md) then
      write (lmess,30) md
   30 format (' *** fatal *** directive limit',i3,' exceeded')
      call jobend
      end if
      if (string(:3).eq.'*f,') then 
      j = j+1
      ipos = 4
      do 2 i = 1,2
      call nextrf (ipos,',',string,par(i),len(i),nonc)
    2 call chint (len(i),par(i),ipar(i))
      nonc = .false.
      ibp(j) = ipar(1)
      ilp(j) = ipar(2)
      if (string(ipos:ipos).eq.'a') then
      order = .true.
      else if (string(ipos:ipos).ne.'d') then
      write (lmess,40)
   40 format (' *** fatal *** unrecognised sort order')
      call jobend
      end if
      else
      write (lmess,60)
   60 format (' *** fatal *** unrecognised "dupchk" directive') 
      call jobend
      end if
      goto 1
   98 call errr (lin,knt,ios)
      return
   99 knt = j
      return
      end
