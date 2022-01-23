      subroutine ptrig (n,com,para,newpage,
     +newhead,new,par,pph,ipl,bline,head,ltp2,lk,lkc,k,lit)
c-----called by `tset` to determine content and position (? lines) of
c-----head when (1) new or (2) *tph field content has changed.
      dimension com(100),icom(100,10),para(100,3),lenp(100,3)
      dimension ncom(100)
      character com*10,para*130,par*80,pph*80,bline*130,head*130
      character lit*1000
      logical new,newpage,newhead
      common /cint/ knt,icom,ncom,lenp
      common /stdio/ lin,lout,lmess
      save
c------first page of a sheet or trigger field changed
      if((newpage).or.(newhead)) then
      write (lmess,10)
   10 format (' ***fatal***only one "*tph" allowed/run')
      call jobend
      end if
c------"pph" previous page heading set to field content "par"
      pph = par
      if (ncom(n).eq.5.and.(.not.new)) then
c------lines parameter .. check if new page necessary
      if (lkc.eq.0.and.k.gt.1) then
c------no *tch or *tcc in job, normal filling of col to 'ipl'
      newpage = .true.
      else
      if (lkc.gt.lk) lk = lkc
      if ((ipl-lk).ge.icom(n,5)) then
      newhead = .true.
      ltp2 = lk+icom(n,3)
      else
      newpage = .true.
      end if
      end if
      else
      newpage = .true.
      end if
      head = bline
      nn = n+1
      m = icom(n,4)
      mm = m+icom(n,2)-1
      if (nn.gt.knt) goto 3
c------check for *equiv(s) immediately following *tph
      do 2 i = nn,knt
      if (com(i).ne.'*equiv') goto 3
      if (ncom(i).eq.1) then
      j = m+lenp(i,1)-1
      head(m:j) = para(i,1)
      return
      else if (para(i,1).eq.pph) then
      if (ncom(i).eq.3) then
      it = icom(i,2)+icom(i,3)-1
      j = m+icom(i,3)-1
      head(m:j) = lit(icom(i,2):it)
      return
      end if
      j = m+lenp(i,2)-1
      head(m:j) = para(i,2)
      return
      end if
    2 continue
c------no *equiv(s) or no matching ones, default heading is
c------content of trigger field
    3 head(m:mm) = pph
      return
      end
