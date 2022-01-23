      subroutine recadd (string,recin,irec,lines,irl,itp,keep,fin)
c-----reads records for insertion following a *i directive
      dimension recin(10)
      character string*80,recin*1000
      logical keep,fin,check,unfinr
      common /limits/ lc,ll,lp,lr,nv
      common /edlim/ limed,limrec
      common /stdio/ lin,lout,lmess
      check = .false.
      unfinr = .false.
      irec = 0
      j = 0
      j2 = 0
      j3 = 0
      j1 = 1
      if (irl.lt.lc) then
      j2 = irl
      check = .true.
      l = 1
      goto 2
      else
      j2 = lc
      end if
    1 l = 1
      check = .false.
      unfinr = .false.
    2 irec = j
      j = j+1
      if (j.gt.limrec) goto 96
      call setlen (string)
      if (irl.lt.itp) then
      k = irl+1
      do 9 n = k,itp
    9 recin(j)(n:n) = ' '
                      end if
    3 read (lin,10,end=99,err=98,iostat=ios) string
   10 format (a)
      if (string(1:1).eq.'*') then
      if (irec.eq.0) then
      write (lmess,20)
   20 format (' *** fatal *** no records for insertion after *i')
      goto 96
      else if (unfinr) then
      write (lmess,30)
   30 format (' ***fatal*** record for insertion < irl')
      goto 96
      end if
      keep = .true.
      goto 97
      else
      write (lmess,40) string
   40 format (' ',a)
      if (check) then
      j3 = (j2-j1)+1
      recin(j)(j1:j2) = string(1:j3)
      else
      recin(j)(j1:j2) = string
      end if
      if (lines.eq.1) goto 2
      if (lines.eq.l) then
      j1 = 1
      j2 = lc
      goto 1
                      else
      unfinr = .true.
      l = l+1
      j1 = j1+j2
      if (l.lt.lines) then
      j2 = j2+lc
      else
      j2 = irl
      check = .true.
      end if
      goto 3
      end if
      end if
   96 call jobend
   98 call errr (lin,j,ios)
      return
   99 fin = .true.
   97 return
      end
