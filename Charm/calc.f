      program calc
c-----NOS version written 27/11/85 ... anne swinton
c-----nb. previously  calc/means on NOS, complete calc first on UNIX
c-----UNIX version 18/11/85,  update 22/4/87 (reseq new file). 
c-----latest update incorporate "ckcalc" and "unlockcalc" 24/9/87
c-----limits  ...  10 *ca/*cm directives  (iam) 
c-----            100 *a/*b/*c/*d directives  (isub)
c-----             10 character field lengths  (ifl)
c-----when using a user written subroutine as well as *ca/*cm
c-----directives these directives may be mixed in with the
c-----subroutine directives (*a/*b/*c/*d) but they will always be 
c-----executed after the bxa directives.
c-----the order of bxa directives is vital to the execution of
c-----the subroutine.
      dimension com(10),const(10),ans(10),ipar(10,6),npar(10),
     +ipa(100,3),ipb(100,3),ipc(100,2),ipd(100,2),
     +fmta(100),fmt(100),fmti(10),fmto(10),fmtn(100),fmtno(10), 
     +cfld(100),dfld(100),achar(100),bchar(100),afld(100),bfld(100),
     +iei(10),ieo(10),iea(100),ieb(100),iec(100),ied(100),
     +ta(100),tb(100) 
      character com*1,ans*1,par*1,fmta*8,fmt*8,fmti*8,fmto*8, 
     +cfld*10,dfld*10,achar*10,bchar*10,rec*1000,lit*1000,
     +fmtn*8,fmtno*8,string*25
      double precision const,afld,bfld,rpar,rparo
      logical rdend,ta,tb,usrsub,ex
      common /stdio/ lin,lout,lmess
      common /limcal/ ifl,iam,isub
      common /ca/ com
      common /cb/ ans
      common /cc/ par
      common /cd/ fmta
      common /ce/ fmt
      common /cf/ fmti
      common /cg/ fmto
      common /ch/ cfld
      common /ci/ dfld
      common /cj/ achar
      common /ck/ bchar
      common /cl/ rec
      common /cm/ lit
      common /co/ fmtn
      common /cp/ fmtno
      data ifl,iam,isub/10,10,100/
      data fmta,fmt,fmti,fmto/200*' ',20*' '/ 
      data com,ans,par/10*' ',10*' ',1*' '/
      data cfld,dfld,achar,bchar/400*' '/ 
      data rec,lit/2*' '/
      data irl2/1/
      data nam,na,nb,nc,nd/5*0/
      data const,afld,bfld,rpar,rparo/10*0.0,200*0.0,2*0.0/
      lti = 10
      lto = 11
      rdend = .false.
      ex = .false.
      write (lmess,10)
   10 format ((/),' calc run')
      call argopn (lti,1,'old')
      call argopn (lto,2,'new')
c-----read all calc directives
      call calcrd (com,const,ans,ipar,npar,nam,ipa,ipb,ipc,ipd,na,
     +nb,nc,nd,usrsub) 
c-----read a record to obtain record length
      knt = 0
      call readch (irl,iseq,lti,lit,rdend,knt)
c-----check for any cm/ca directives that extend record length
c-----allocate field ends for all ca/cm directives
      if (nam.ge.1) then
         irlt = irl
         do 1 i = 1,nam
            if (ans(i).eq.'e') then
               irl2 = irlt+ipar(i,5)
               ieo(i) = irl2
               ipar(i,4) = irlt+1
               irlt = irl2
            else if (ans(i).eq.'o') then
               ipar(i,6) = ipar(i,5)
               ipar(i,4) = ipar(i,1)
               ipar(i,5) = ipar(i,2)
               ieo(i) = ipar(i,1)+ipar(i,2)-1
            else
               ieo(i) = ipar(i,4)+ipar(i,5)-1
            end if
            iei(i) = ipar(i,1)+ipar(i,2)-1
c-----formats for i/o conversion char/double precision, int/char
            call dpchar (ipar(i,2),ipar(i,3),fmti(i))
            call intch (ipar(i,5),fmto(i),fmtno(i)) 
    1    continue
      end if
c-----check for BXA results extending record length
      if ((nb+nd).ge.1) call rltst (nb,nd,ipb,ipd,irl2)
      if (irl.gt.irl2) irl2 = irl
c-----check for double precision or character type fields for sub. bxa
      if (na.ge.1) then
         do 2 i  = 1,na
            call dpchar (ipa(i,2),ipa(i,3),fmta(i))
    2    iea(i) = ipa(i,1)+ipa(i,2)-1
      end if
      if (nb.ge.1) then
         do 3 i = 1,nb
            call intch (ipb(i,2),fmt(i),fmtn(i))
    3    ieb(i) = ipb(i,1)+ipb(i,2)-1
      end if
      if (nc.ge.1) then
         do 4 i = 1,nc
    4    iec(i) = ipc(i,1)+ipc(i,2)-1
      end if
      if (nd.ge.1) then
         do 5 i = 1,nd
    5    ied(i) = ipd(i,1)+ipd(i,2)-1
      end if
c-----read a record from input ... unit 10
      if (knt.eq.1) goto 16
    6 call readch (irl,iseq,lti,lit,rdend,knt) 
      if (rdend) goto 99
   16 rec(1:irl) = lit(1:irl)
c-----check if there is a call to subroutine bxa
      if (usrsub) then
         if (na.ge.1) then
            do 7 i = 1,na
               par = lit(iea(i):iea(i))
               if (par.ge.'0'.and.par.le.'9') then
                  ta(i) = .true.
                  read (lit(ipa(i,1):iea(i)),fmta(i)) afld(i)
               else
                  ta(i) = .false.
                  achar(i) = lit(ipa(i,1):iea(i))
               end if
    7       continue
         end if
         if (nc.ge.1) then
            do 8 i = 1,nc
    8       cfld(i) = lit(ipc(i,1):iec(i))
         end if
c-----call user written subroutine bxa to do calculations 
c-----required for this job.
         call bxa (afld,bfld,cfld,dfld,ta,tb,achar,bchar)
         if (nb.ge.1) call recout(rec,bfld,bchar,nb,tb,fmt,ipb,ieb,
     +fmtn,knt) 
         if (nd.ge.1) then
            do 12 i = 1,nd
   12       rec(ipd(i,1):ied(i)) = dfld(i)(1:ipd(i,2))
         end if
      end if
c-----constant add/multiply calculations
      if (nam.ge.1) then
         do 13 i = 1,nam
            par = lit(iei(i):iei(i))
            if (par.eq.' '.or.par.eq.'-'.or.par.eq.'+') goto 13
            read(lit(ipar(i,1):iei(i)),fmti(i))rpar
            if (com(i).eq.'a') then
               rparo = const(i)+rpar
            else
               rparo = const(i)*rpar
            end if
            call recoutc (rec,ipar(i,4),ipar(i,6),ieo(i),rparo,fmto(i), 
     +fmtno(i),knt) 
   13    continue
      end if
      call recwr (irl2,knt,rec,lto)
      goto 6
c-----all calculations done for all records on input
   99 call eofr (lti,knt)
      write (lmess,30) lto,irl2
   30 format (' file unit',i3,' record length written =',i6,
     +' characters')
      call eofw (lto,knt)
c-----check if "ckcalc" and lock file was used ... unlock if so
      inquire (file='lockcalc',iostat=ios,exist=ex)
      if (ex) then
         string = 'mv lockcalc unlockcalc'
         lstat = system(string)
      end if
      stop
      end
