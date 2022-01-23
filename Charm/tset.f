      subroutine tset (com,para,ipl,lk,ilk,bline,head,chead,
     +new,newpage,newhead,newcol,newch,lit,npage,ncol,nline,
     +nbl,lenc,k,itch,ltp2,lkc)
c------checks for altered trigger field(s) and for matching
c------*equiv(s) gives new page and column headings head/chead
      dimension com(100),para(100,3)
      character com*10,par*80,pph*80,pch*80,ptp*80,ptc*80,ptl*80
      character para*130,bline*130,head*80,chead*80
      character lit*1000
      logical new,newpage,newhead,newcol,newch
      logical npage,ncol,nline
      common /stdio/ lin,lout,lmess
      common /cpar/ pph,pch,ptp,ptc,ptl,par
      common /cint/ knt,icom(100,10),ncom(100),lenp(100,3)
      save
      data pph,pch,ptp,ptc,ptl,par/6*' '/
      newpage = .false.
      newhead = .false.
      newcol = .false.
      npage = .false.
      ncol = .false.
      nline = .false.
      kk = ipl-lk
      do 1 n = 1,knt
      if (com(n).eq.'*equiv') then
      j = n-1
      if (com(j).eq.'*tph'.or.com(j).eq.'*tch') then
      goto 1
      else if (com(j).eq.'*equiv') then
      goto 1
      else
      call jobend
      end if
      end if
      if (com(n).eq.'*fph'.or.com(n).eq.'*fch') goto 1
      l = icom(n,1)
      ll = l+icom(n,2)-1
      par = '          '
      par(1:icom(n,2)) = lit(l:ll)
      if (com(n).eq.'*tph') then
      if ((new).or.pph.ne.par) call ptrig (n,com,para,newpage,
     +newhead,new,par,pph,ipl,bline,head,ltp2,lk,lkc,k,lit)
      else if (com(n).eq.'*tch') then
      if ((new).or.pch.ne.par) call ctrig (n,com,para,newcol,
     +newch,new,par,pch,kk,bline,chead,lenc,k,itch,lk,lkc,lit)
      else if (com(n).eq.'*tpc') then
      if ((new).or.ptp.ne.par) then
      ptp = par
      npage = .true.
      end if
      else if (com(n).eq.'*tcc') then
      if ((new).or.ptc.ne.par) then
      ptc = par
      ncol = .true.
      if (lk.gt.lkc) lkc=lk
      end if
      else if (com(n).eq.'*tbl') then
      if ((new).or.ptl.ne.par) then
      ptl = par
      nline = .true.
      nbl = icom(n,3)
      end if
      end if
    1 continue
      return
      end
