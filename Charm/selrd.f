      subroutine selrd (i,sdt,j,ibeg,ifl,ifend,nfield,rel,k
     +,val,nval)
      dimension sdt(50),ibeg(50,5),ifl(50,5),ifend(50,5),rel(50,2),
     +val(50,10),nfield(50),nval(50)
      character sdt*1,beg*4,fl*2,string*80,rel*2,val*10,del2*1
      common /stdio/ lin,lout,lmess
      common /limits/ lc,ll,lp,lr,nv
      common /tio/ lti,lto,lto2
      logical check,nonc,last
c------reads select directives
      i = 0
    1 read (lin,15,end=99,err=98,iostat=ios)string
   15 format (a)
      write (lmess,20) string
   20 format (' ',a)
      check = .false.
      ipos = 4
      del2 = ' '
      j = 1
      i = i+1
      rel(i,2) = '  '
      if(i.gt.50)goto 97
c------check directive type ... include or exclude allowed
      if (string(1:3).eq.'*i,'.or.string(1:3).eq.'*e,') then
      sdt(i) = string(2:2)
      else if (string(1:4).eq.'*el,') then
      sdt(i) = string(3:3)
      ipos = 5
      if (string(5:6).eq.'f(') then
      check = .true.
      del2 = ')'
      ipos = 7
      end if
      else
      goto 97
      end if
c------check for combined fields
      if (string(4:5).eq.'f(') then
      ipos = 6
      check = .true.
      del2 = ')'
      end if
      ifmax = 4
      nonc = .true.
    2 call nextsf (ipos,',',string,beg,len,check,del2,nonc
     +,ifmax,last)
      call chint (len,beg,ibeg(i,j))
      call nextsf (ipos,',',string,fl,len,check,del2,nonc
     +,ifmax,last)
      call chint (len,fl,ifl(i,j))
      ifend(i,j) = ibeg(i,j)+ifl(i,j)-1
c------continue if more fields
      if (.not.check) goto 3
      j = j+1
      if (j.gt.5) goto 97
      goto 2
c------read equivalence parameter "rel"
    3 nonc = .false.
      k = 1
      ifmax = 2
      call nextsf (ipos,',',string,rel(i,k),len,check,del2,nonc
     + ,ifmax,last)
c------determine field length of values to be equated
      ifmax = 0
      do 100 n = 1,j
      ifmax = ifmax+ifl(i,n)
  100 continue
      nfield(i) = j
      if (ifmax.gt.lp) then
      call exceed (3,ifmax)
      end if
c------value parameters
    4 call nextsf (ipos,',',string,val(i,k),len,check,del2,nonc
     + ,ifmax,last)
      if (len.ne.ifmax) goto 97
      if (last) goto 5
      k = k+1
c------check for second equivalence, ie. range > <
      if (k.eq.2) then
      l = ipos+1
      if (string(ipos:l).eq.'lt') then
      rel(i,k) = string(ipos:l)
      ipos = ipos+3
      end if
      end if
      if(k.gt.nv) then
      call exceed (5,k)
      end if
      goto 4
c------no more values directives complete
    5 last = .false.
      nval(i) = k
      goto 1
   97 write (lmess,40)
   40 format (' invalid select directive')
      call jobend
      return
   98 call errr(lin,i,ios)
      call jobend
   99 return
      end
