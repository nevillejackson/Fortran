      subroutine nopage (ip,page,ipw,lto,irk,icnt,rdend,limp)
c------numbers pages after a *tph or at end of data on input
c------adds printer control char. to first line of each page.
c------writes page(s) to output file (tape 11)
      dimension page(100,62)
      character page*130
      logical rdend,limp
      common /slim/ limc,lpar,limx,limo,limm
      common /stdio/ lin,lout,lmess
      k = ipw-18
      kk = k+9
      j = k+5
      jj = j+1
      l = jj+5
      ll = l+1
      do 1 n = 1,ip
      page(n,1)(1:1) = '1'
      page(n,2)(k:kk) = 'page    of'
      write (page(n,2)(j:jj),'(i2)') n
      write (page(n,2)(l:ll),'(i2)') ip
      write (lto,10) page(n,1)
   10 format (a)
      do 2 m = 2,62
      write (lto,20) page(n,m)
   20 format (' ',a)
    2 continue
    1 continue
      write (lmess,30) ip
   30 format (' block of',i3,' page(s) written to sheet')
      if (icnt) 4,4,3
    3 icnt = irk-icnt1
      icnt1 = irk
      goto 5
    4 icnt = irk-1
      icnt1 = irk
    5 if (ip.eq.limm) then
      write (lmess,35) limm
   35 format(' *** fatal *** ',i4,' page block limit exceeded ... stop')
      rdend = .true.
      else if (ip.eq.limo.and.limp) then
      write (lmess,32) limo
   32 format(' *** fatal *** ',i3,' page block limit exceeded ... stop')
      rdend = .true.
      end if
      if (rdend) icnt=icnt+1
      write (lmess,40) icnt
   40 format (' block record count = ',i6)
      return
      end
