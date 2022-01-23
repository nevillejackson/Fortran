      subroutine edrd (windup,keep,type,lno,icn,ptype,par,lenp,
     +ifield,nsf,lines,itp,recin,irec,kntd,fin,irl)
c-----reads a directive for 'editr' and records for insertion
c-----following *i directive.
      dimension icn(10),ptype(10),par(10),recin(10),lenp(10),
     +ifield(10)
      character string*80,type*1,del*1,ptype*1,par*10,
     +recin*1000,seq*3,cn*3
      logical windup,keep,fin,nonc,mfield,last
      common /edlim/ limed,limrec
      common /stdio/ lin,lout,lmess
      common /limits/ lc,ll,lp,lr,nv
      last =.false.
      nonc = .true.
      ipos = 4
      j = 0
      irec = 0
      if (keep) goto 2
      call setlen (string)
    1 read (lin,10,end=99,err=98,iostat=ios) string
   10 format (a)
    2 keep = .false.
      kntd = kntd+1
      if (kntd.gt.limed) then
      write (lmess,20) limed
   20 format (' *** error *** directive limit ',i3,' exceeded')
      windup = .true.
      goto 99
      else
      write (lmess,30) string
   30 format (' ',a)
      end if
      if (string(1:1).ne.'*'.or.string(3:3).ne.',') goto 97
      type = string(2:2)
      if (type.eq.'s') then
      del = ','
      mfield = .true.
      else
      del = ' '
      mfield = .false.
      end if
      call nexted (ipos,del,string,seq,len,nonc,last)
      call chint (len,seq,lno)
    3 if (mfield) then
      j = j+1
      ptype(j) = string(ipos:ipos)
      ipos = ipos+1
      if (string(ipos:ipos).ne.'(') goto 97
      ipos = ipos+1
      call nexted (ipos,',',string,cn,len,nonc,last)
      call chint (len,cn,icn(j))
      if (ptype(j).eq.'d'.or.ptype(j).eq.'b') goto 4
      nonc = .false.
    4 call nexted (ipos,')',string,par(j),lenp(j),nonc,last)
      if (nonc) then
      call chint (lenp(j),par(j),ifield(j))
      else
      nonc = .true.
      end if
      if (.not.last) goto 3
      nsf = j
      else if (type.eq.'i') then
      call recadd (string,recin,irec,lines,irl,itp,keep,fin)
      end if
      return
   97 write (lmess,40)
   40 format (' *** fatal *** invalid editr directive')
      call jobend
   98 call errr (lin,kntd,ios)
      return
   99 windup = .true.
      return
      end
