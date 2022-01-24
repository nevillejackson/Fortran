      program indx
c     program =index= ( niklaus kuenzi,eth,zuerich )
c     this is a general purpose program for estimations of
c     breeeding values in form of actual measures (kg,cm,etc)or as
c     selection indexes from information on
c      1 or several relatives(m half-sib groups with n full-sibs each)
c      1 or several traits per relative
c      1 or several measurements per trait
c     maternal genetic effects and inbred relativescan be included
c     the main program sets up the index equations
c     program =selind= from e.p. cunningham is used as a subroutine
c     to solve the equations and to compute reduced and/or restried
c     selection indexes
c-----------------------------------------------------------------------
      character xna*4,ra*4,blank*10,xname*4,yname*4,yna*4
      dimension ir(4),sp(25),rp(325),h2(25),h(25),rg(325),c2(25),rc(325)
     1,q2(25),rq(325),e2(25),re(325),a(231),ma(16),ra(4),pp(3),
     2kk(25),ll(25),mm(25),nn(25),nre(25),rep(25),nmr(25),nre1(25),ire(2
     35),aa(25,25),
     4proj(16),me(25),xna(21),yna(25)  ,ic(25),iq(25),r(7),ai(7),
     5ig1(11),ig2(11),kontr(25)
      common/kar3/v(25)
      common/kar4/iversi(3)
      common/kar21/xname(25,5),yname(25,5)
      common/seli/p(325),c(25,25),g(325),sg(25),ret(25),b(25)
      common/seli1/isub,iret,mti(25),nti(25)
      common/kovmat/sdind,sdiz(30,2),corr
      common/zwistu/qr(25,25),b1(25),b2(25)
c     ---------------------------------------------------------------
      data xna/'ind ','vat ','mut ','vava','vamu','muva','mumu','hgv ',
     1'vgv ','hgm ','vgm ','vhg ','vg  ','mhg ','nk 1','nk 2','mhgv',
     2'mhgm','mvhg','mnk1','mnk2'/
      data a/
     11.  ,.5  ,.5  ,.25 ,.25 ,.25 ,.25 ,.125,.25 ,.125,.25 ,.25 ,.5  ,
     2.25 ,.5  ,.25 ,.0  ,.0  ,.0  ,.0  ,.0  ,1.  ,.0  ,.5  ,.5  ,.0  ,
     3.0  ,.25 ,.5  ,.0  ,.0  ,.5  ,.5  ,.0  ,.25 ,.125,.0  ,.0  ,.0  ,
     4.0  ,.0  ,1.  ,.0  ,.0  ,.5  ,.5  ,.0  ,.0  ,.25 ,.5  ,.0  ,.5  ,
     5.5  ,.25 ,.125,.0  ,.0  ,.0  ,.0  ,.0  ,1.  ,.0  ,.0  ,.0  ,.5  ,
     6.5  ,.0  ,.0  ,.25 ,.25 ,.0  ,.125,.063,.0  ,.0  ,.0  ,.0  ,.0  ,
     71.  ,.0  ,.0  ,.0  ,.5  ,.0  ,.0  ,.25 ,.25 ,.0  ,.125,.063,.0  ,
     8.0  ,.0  ,.0  ,.0  ,1.  ,.0  ,.0  ,.0  ,.5  ,.5  ,.0  ,.25 ,.25 ,
     9.125,.063,.0  ,.0  ,.0  ,.0  ,.0  ,1.  ,.0  ,.0  ,.0  ,.5  ,.0  ,
     9.25 ,.25 ,.125,.063,.0  ,.0  ,.0  ,.0  ,.0  ,1.  ,.25 ,.0  ,.0  ,
     1.125,.125,.0  ,.063,.031,.5  ,.0  ,.0  ,.0  ,.0  ,1.  ,.0  ,.0  ,
     2.125,.125,.0  ,.063,.031,.0  ,.0  ,.0  ,.0  ,.0  ,1.  ,.25 ,.0  ,
     3.125,.125,.063,.031,.0  ,.5  ,.0  ,.0  ,.0  ,1.  ,.0  ,.125,.125,
     4.063,.031,.0  ,.0  ,.0  ,.0  ,.0  ,1.  ,.25 ,.0  ,.125,.063,.0  ,
     5.0  ,.5  ,.0  ,.0  ,1.  ,.25 ,.25 ,.125,.0  ,.0  ,.0  ,.0  ,.0  ,
     61.  ,.125,.063,.0  ,.0  ,.0  ,.0  ,.0  ,1.  ,.5  ,.0  ,.0  ,.0  ,
     7.5  ,.0  ,1.  ,.0  ,.0  ,.0  ,.0  ,.5  ,1.  ,.0  ,.0  ,.0  ,.0  ,
     81.  ,.0  ,.0  ,.0  ,1.  ,.0  ,.0  ,1.  ,.0  ,1.  /
      data ma/3,5,7,0,0,0,0,17,5,18,7,19,3,3,20,21/
      data ra/'a-i ','a-vg','a-hg','a=r='/
      data blank/'          '/
      data ig1/ 1, 1, 1,12,13, 2, 2, 8, 3, 3,10/
      data ig2/12,13,14,13,14, 8, 9, 9,10,11,11/
       open(unit=4,file='indinp.dat',status='old')
       open(unit=6,file='indout.dat',status='new')
    1 do 2 i=1,25
      sp(i)=.0
      h2(i)=.0
      h(i)=.0
      c2(i)=.0
      q2(i)=.0
      e2(i)=.0
      kk(i)=0
      ll(i)=0
      mm(i)=0
      nn(i)=0
      sg(i)=.0
      v(i)=.0
      nmr(i)=0
      nre(i)=1
      rep(i)=1.
      mti(i)=0
      do 2 j=i,25
      k=(i-1)*25-(i-1)*i/2+j
      rp(k)=.0
      rg(k)=.0
      rc(k)=.0
      rq(k)=.0
      re(k)=.0
      p(k)=.0
      c(i,j)=.0
      c(j,i)=.0
      g(k)=.0
    2 continue
c================================================*
c****      read and print first parameter card   *
c------------------------------------------------*
      read(4,100) nm,(ir(i),i=1,4),mate,iversi(1),iversi(2),proj
  100 format(8i2,16a4)
      if(iversi(1).ge.3)goto 1001
      write(6,101) proj,nm,(ir(i),i=1,4),mate
  101 format(1h ,'projekt:',5x,16a4   /90(1h=)//1x,' nm= ',i2,' ir(i)=',
     14i2,' mat.eff.= ',i2/)
1001  continue
c------------------------------------------------------------*
c**** read and print given phenotypic and genetic parameters *
c------------------------------------------------------------*
      nm2=nm
      if (mate .le. 0) go to 3
      nm2 = nm + mate
      nmax = nm2 * (nm2 - 1) / 2 + nm2
c**** bezeichnung der leistungs-(hilfs)-merkmalen
c------------------------------------------------
    3 read(4,98)  (yna(i),i=1,nm2)
      if(iversi(2).eq.1)goto 1002
      write(6,99)  (i,yna(i),i=1,nm2)
   99 format((1h ,'*  name      =', 9(i5,'. ',a4)/))
1002  continue
   98 format(8x,18a4)
      do 5 i=1,4
      if(ir(i).lt.1) go to 5
      go to (11,12,13,14) i
11    if(iversi(2).ge.2)goto 1003
      write(6,102)(yna(j),j=1,nm)
  102 format(//1x,'*phenotypic standard-deviations and correlations
     1'/1x,'  i    sp(i)   rp(i,j)'/1x,11x,12(5x,a4)/1x,13(5x,a4))
      write(6,2222)
 2222 format(1x)
1003  continue
c**** phaenotypische standardabweichungen und korrelationen
c-----------------------------------------------------------
      do 6 j=1,nm
      k1=(j-1)*j/2+1
      k2=k1+j-1
      read(4,103) sp(j),(rp(k),k=k1,k2)
      if(iversi(2).ge.2)goto 6
      write(6,104) j,sp(j),(rp(k),k=k1,k2)
6     continue
  103 format(10f8.3)
  104 format(1h ,i2,13f9.3/2x,'-',13f9.3)
      go to 5
12    continue
      if(iversi(2).ge.2)goto 1004
      write(6,105)  (yna(j),j=1,nm2)
  105 format (/1x,'*heritabilities and genetic correlations'/1x,
     1'  i    h2(i)   rg(i,j)'/1x,11x,12(5x,a4)/1x,13(5x,a4))
      write(6,2222)
1004  continue
c**** heritabilitaeten und genetischen korrelationen
c---------------------------------------------------
      do 7 j=1,nm2
      k1=(j-1)*j/2+1
      k2=k1+j-1
      read(4,103) h2(j),(rg(k),k=k1,k2)
      h(j)=sqrt(h2(j))
      sg(j)=h(j)*sp(j)
      if(j.gt.nm) sg(j)=h(j)*sp(j-nm)
      if(iversi(2).ge.2)goto 7
      write(6,104) j,h2(j),(rg(k),k=k1,k2)
7     continue
      go to 5
c**** littereffects
c----------------
13    continue
      if(iversi(2).ge.2) goto 1005
      write(6,106)  (yna(j),j=1,nm)
  106 format (/1x,'*littereffects'/1x,'  i    c2(i)   rc(i,j)'/1x,11x,
     112(5x,a4)/1x,13(5x,a4))
      write(6,2222)
1005  continue
      do 8 j=1,nm
      k1=(j-1)*j/2+1
      k2=k1+j-1
      read(4,103) c2(j),(rc(k),k=k1,k2)
      if(iversi(2).ge.2) goto 8
      write(6,104) j,c2(j),(rc(k),k=k1,k2)
8     continue
      go to 5
c**** herdeffekte
c------------------
14    continue
      if(iversi(2).ge.2) goto 1006
      write(6,107)  (yna(j),j=1,nm)
  107 format (/1x,'*herdeffects'/1x,'  i    q2(i)   rq(i,j)'/1x,11x,
     112(5x,a4)/1x,13(5x,a4))
      write(6,2222)
1006  continue
      do 9 j=1,nm
      k1=(j-1)*j/2+1
      k2=k1+j-1
      read(4,103) q2(j),(rq(k),k=k1,k2)
      if(iversi(2).ge.2) goto 9
      write(6,104) j,q2(j),(rq(k),k=k1,k2)
9     continue
    5 continue
c----------------------------------------------------*
c****     compute residual effects and correlations  *
c----------------------------------------------------*
      if(iversi(2).ge.2) goto 1007
      write(6,108)  (yna(j),j=1,nm)
  108 format (/1x,'residualeffects'/1x,'  i    e2(i))   re(i,j)'/1x,11x,
     112(5x,a4)/1x,13(5x,a4))
      write(6,2222)
1007  continue
      do 10 i=1,nm
      k1=(i-1)*i/2+1
      k2=k1+i-1
      do 16 k=k1,k2
      j=k-k1+1
      xme=0.
      if(mate.le.0) go to 15
      k3=(nm+i-1)*(nm+i)/2+j
      k4=(nm+j-1)*(nm+j)/2+i
      k5=k3+nm
      if (k3 .gt. nmax) go to 15
      xme=.5*rg(k3)*h(i)*h(nm+j)+.5*rg(k4)*h(nm+i)*h(j)+rg(k5)
     1*h(nm+i)*h(nm+j)
   15 re(k)=(rp(k)-rg(k)*h(i)*h(j)-xme-rc(k)*sqrt(c2(i)*c2(j
     1))-rq(k)*sqrt(q2(i)*q2(j)))
   16 continue
      e2(i)=re(k2)
      do 4 k=k1,k2
      j=k-k1+1
    4 re(k)=re(k)/sqrt(e2(i)*e2(j))
      if(iversi(2).ge.2) goto 10
      write(6,104) i,e2(i),(re(k),k=k1,k2)
10    continue
c-----------------------------------------------------------------*
c****       read and print relative economic values v             *
c-----------------------------------------------------------------*
      if(iversi(2).ge.2)goto 1172
      write(6,109)  (yna(j),j=1,nm2)
  109 format (/1x,'*relative economic values      (fr)    '/1x,11x,12(5x
     1,a4)/1x,13(5x,a4))
      write(6,2222)
1172  read(4,117) (v(j),j=1,nm2)
      if(iversi(2).ge.2)goto 1173
      write(6,118) (v(j),j=1,nm2)
1173  continue
  117 format(8x,9f8.3)
  118 format(12x,12f9.3//3x,13f9.3)
c-----------------------------------------------------------------------
c****            read dimensions for particular index
c-----------------------------------------------------------------------
90    continue
      la=0
      read(4,110) inr,nrel,iges,isub,ia,iret,liop,mpop,is1,is15
     x,jsda,izsme,inr1
 110  format(40i2)
      if(iversi(2).ge.3)goto 8558
      write(6,119) inr,nrel,iges,isub,ia,iret,liop,mpop,is1,is15
     x,jsda,izsme,inr1
  119 format(/1x,90(1h=)/1x,'* index     relatives iges isub  ia  iret l
     1iop mpop is1  is15 jsda izsme inr1'//1x,i5,2i9,8i5,3i6/)
8558  continue
      if(is1.eq.1) ma(15)=1
      if(is15.eq.1) ma(16)=15
      i1=0
      ninf1=0
      do 25 i=1,nrel
      read(4,1111) k1,m1,n1,nme,(me(j),j=1,nme),ic1,iq1,nr,(nmr(j)
     1,nre1(j),ire(j),j=1,nr)
 1111 format(i2,i4,37i2)
 8585 if(i.le.izsme)ninf1=ninf1+nme
c-----------------------------------------------*
c**** arrange k,l,m,n,ic,iq,nre, rep - arrays   *
c-----------------------------------------------*
      do 26 j=1,nme
      i1=i1+1
      kk(i1)=k1
      ll(i1)=me(j)
      mm(i1)=m1
      mti(i1)=m1
      nn(i1)=n1
      nti(i1)=n1
      ic(i1)=ic1
      iq(i1)=iq1
      nre(i1)=1.
      rep(i1)=1.
      if(nr.lt.1) go to 26
      do 27 k=1,nr
      if(nmr(k).ne.me(j))  go to 27
      nre(i1)=nre1(k)
      rep(i1)=float(ire(k))/100.
   27 continue
   26 continue
   25 continue
      ninf=i1
      if(iversi(2).ge.3)goto 8559
      write(6,123)
  123 format (/1x,'*information for:      k  l  m  n  ic iq nre rep'/)
      do 18 i=1,ninf
      k=kk(i)
      l=ll(i)
      write(6,121) i,xna(k), yna(l)         ,kk(i),ll(i),mm(i),
     1nn(i),ic(i),iq(i),nre(i),rep(i)
  121 format(1h ,i3,2x,a4,6x,1a4,2x,7i3,f5.1)
   18 continue
8559  continue
c**** read particular coefficients of relationship
c-------------------------------------------------
      if(ia.le.0) go to 19
      do 17 i=1,ia
      read(4,114) i1,i2,xa
  114 format(2i2,f10.5)
      i3=(i1-1)*21-(i1-1)*i1/2+i2
   17 a(i3)=xa
c--------------------------------------------------------*
c****   compute elements of phenotypic covariance matrix *
c--------------------------------------------------------*
   19 if(liop.ne.2) go to 39
      write(6,120)
  120 format (1h /'* elements of vectors r,a and product r=a'//)
   39 do 20 i=1,ninf
      do 20 j=i,ninf
      li=ll(i)
      ni=nn(i)
      mi=mm(i)
      lj=ll(j)
      nj=nn(j)
      mj=mm(j)
      if(ni.lt.nj) ni=nj
      if(mi.lt.mj) mi=mj
      l1=li
      l2=lj
      l3=0
      l4=0
      l5=0
      if(li.ge.lj) go to 24
      l1=lj
      l2=li
   24 l=(l1-1)*l1/2+l2
c**** set up r-array
c-------------------
      r(1)=rg(l)*h(li)*h(lj)
      r(2)=0.
      r(3)=0.
      r(4)=0.
      if(mate.le.0) go to 21
      l3=(nm+lj-1)*(nm+lj)/2+li
      l4=(nm+li-1)*(nm+li)/2+lj
      l5=(nm+l1-1)*(nm+l1)/2+nm+l2
      if (l3 .le. nmax)  r(2)=rg(l3)*(h(li)*h(nm+lj))
      if (l4 .le. nmax)  r(3)=rg(l4)*h(nm+li)*h(lj)
      if (l5 .le. nmax)  r(4)=rg(l5)*h(nm+li)*h(nm+lj)
   21 r(5)=rc(l)*sqrt(c2(li)*c2(lj))
      r(6)=rq(l)*sqrt(q2(li)*q2(lj))
      r(7)=re(l)*sqrt(e2(li)*e2(lj))
      if(liop.ne.2) go to 40
      write(6,130)i,j,li,lj,(r(k),k=1,7)
  130 format (1h ,'0i=',i2,' j=',i2,' li=',i2,' lj=',i2,'    r=(',
     17f6.3,'),  a=r=')
   40 do 29 i2=1,3
      pp(i2)=0.
      do 28 i1=1,4
   28 ai(i1)=0.
      do 32 i1=5,7
   32 ai(i1)=1.
      if(i2.eq.2.and.ni.le.1.and.iges.ne.1) go to 29
      if(i2.eq.3.and.mi.le.1.and.iges.ne.1) go to 29
      if(i2.gt.1.and.kk(i).ne.kk(j).and.iges.ne.1) go to 29
      do 30 i1=1,4
c-----------------------------------------------*
c**** selection of a(i),i=1,4  from table 1     *
c-----------------------------------------------*
      ki=kk(i)
      kj=kk(j)
      if(mate.le.0.and.i1.gt.1) go to 30
      if(i1.eq.2) kj=ma(kj)
      if(i1.eq.3) ki=ma(ki)
      if(i1.lt.4) go to 22
      ki=ma(ki)
      kj=ma(kj)
   22 if(ki.lt.kj) go to 23
      kx=ki
      ki=kj
      kj=kx
   23 k=(ki-1)*21-(ki-1)*ki/2+kj
      ai(i1)=a(k)
      if(i2.eq.2.and.ni.le.1) go to 29
      if(i2.eq.3.and.mi.le.1) go to 29
      if(i2.ge.2.and.kk(i).ne.kk(j).and.iges.ne.1) go to 29
      if(i2.eq.2) ai(1)=.5+a(k)-1.
      if (i2 .lt. 3) go to 30
      ai(1) = .25 + a(k) - 1.
      if (ki .lt. 17 .and. kj .lt. 17) go to 30
      ai(2) = 0.
      ai(3) = 0.
      ai(4) = 0.
   30 continue
c**** choose a(i),i=5,7
c----------------------
      if(kk(i).ne.kk(j)) go to 37
      if(i2.ge.2) ai(7)=0.
      if(i2.eq.3) ai(5)=0.
      go to 38
   37 ai(7)=0.
      if(ic(i).ne.1.or.ic(j).ne.1) ai(5)=0.
      if(iq(i).ne.1.or.iq(j).ne.1) ai(6)=0.
c**** sib performance inclusive
c------------------------------
   38 if(iges.ne.1.or.ki.eq.kj) go to 42
      isib=1
      do 52 i4=1,11
      if(ki.eq.ig1(i4).and.kj.eq.ig2(i4)) go to 41
      if(kj.eq.ig1(i4).and.ki.eq.ig2(i4)) go to 41
   52 continue
      isib=0
      if(i2-1) 42,42,29
   41 k=(kj-1)*21-(kj-1)*kj/2+kj
      ai(1)=a(k)
      if(i2.eq.2) ai(1)=.5+a(k)-1.
      if(i2.eq.3) ai(1)=.25+a(k)-1.
      ai(5)=1.
      ai(6)=1.
      ai(7)=1.
      if(i2.ge.2) ai(7)=0.
      if(i2.eq.3) ai(5)=0.
c**** comput and print pp(i)=a=r
c-------------------------------
   42 do 33 ii=1,7
   33 pp(i2)=pp(i2)+ai(ii)*r(ii)
      if(liop.ne.2) go to 29
      write(6,125) kk(i),kk(j),ra(i2),(ai(ii),ii=1,7),pp(i2)
  125 format (11x,'ki=',i2,' kj=',i2,1x,a4,'=(',7f6.3,')',f8.4)
   29 continue
      w=1.
      if(i.eq.j.and.li.eq.lj) w=(1+(nre(i)-1)*rep(i))/nre(i)
      xmi=0.
      xmj=0.
      xi=0.
      xj=0.
      ki=kk(i)
      kj=kk(j)
      k=(ki-1)*21-(ki-1)*ki/2+ki
      xi=(a(k)-1.)*h2(li)
      k=(kj-1)*21-(kj-1)*kj/2+kj
      xj=(a(k)-1.)*h2(lj)
      if (li .gt. mate) go to 70
      k1=(ma(ki)-1)*21-(ma(ki)-1)*ma(ki)/2+ma(ki)
      k3=ki
      k4=ma(ki)
      if(k3.lt.k4) go to 35
      kx=k3
      k3=k4
      k4=kx
   35 k2=(k3-1)*21-(k3-1)*k3/2+k4
      l1=ll(i)
      l2=l1+nm
      if(l1.ge.l2) go to 65
      l1=l2
      l2=ll(j)
   65 l3=(l1-1.)*l1/2+l2
      xmi=(a(k1)-1.)*h2(nm+li)+2.*(a(k2)-.5)*rg(l3)*h(li)*h(li+nm)
   70 if (lj .gt. mate) go to 34
      k1=(ma(kj)-1)*21-(ma(kj)-1)*ma(kj)/2+ma(kj)
      k3=kj
      k4=ma(kj)
      if(k3.lt.k4) go to 36
      kx=k3
      k3=k4
      k4=kx
   36 k2=(k3-1)*21-(k3-1)*k3/2+k4
      l1=ll(i)
      l2=l1+nm
      if(l1.ge.l2) go to 66
      l1=l2
      l2=ll(i)
   66 l4=(l1-1)*l1/2+l2
      xmj=(a(k1)-1.)*h2(nm+lj)+2.*(a(k2)-.5)*rg(l4)*h(lj)*h(lj+nm)
   34 spi=sp(li)*sqrt(w+xi+xmi)
      spj=sp(lj)*sqrt(w+xj+xmj)
      if(kk(i).ne.kk(j).and.isib.ne.1)  goto 99999
      goto 99998
99999 mi=1
       ni=mi
99998 k=(i-1)*ninf-(i-1)*i/2+j
      p(k)=spi*spj*(pp(1)+(ni-1)*pp(2)+ni*(mi-1)*pp(3))/(ni*mi)
      isib=0
      if(liop.ne.2) go to 20
      write(6,126) ni,mi,spi,spj,i,j,p(k)
  126 format (11x,'ni=',i3,'  mi=',i3,'  spi=',f10.4,'  spj=',f10.4,
     1'  p(',i2,',',i2,')=',f10.4)
   20 continue
77    if(iversi(1).ge.2) goto 8001
      write(6,111)
  111 format (/1x,'*phenotypic (co)variance matrix p'/)
c**** --------------------------------------------------------
      do 31 i=1,ninf
      do 31 j=i,ninf
      k=(i-1)*ninf-(i-1)*i/2+j
      k1=kk(i)
      k2=kk(j)
      l1=ll(i)
      l2=ll(j)
      write(6,112) i,xna(k1),yna(l1),j,xna(k2),yna(l2),p(k)
  112 format(1h ,2(i3,2x,a4,4x,1a6),f15.5)
   31 continue
8001  continue
c------------------------------------------------*
c****         set up and print a-matrix          *
c------------------------------------------------*
      do 60 i=1,ninf
      ki=kk(i)
      km=ma(ki)
      li=ll(i)
      do 64 j=1,nm2
   64 aa(i,j)=0.
      aa(i,li)=a(ki)
      if (li .gt. mate) go to 60
      aa(i,li+nm)=a(km)
   60 continue
      if(iversi(1).ge.2) goto 8002
      write(6,127)
  127 format (/1x,'* a-matrix: add. genet. coeff. of relationship',
     1' (ohne nullstellen)'/)
      do 61 i=1,ninf
      do 61 j=1,nm2
      if(aa(i,j).eq.0.0) go to 61
      k1=kk(i)
      k2=1
      l1=ll(i)
      l2 = j
      write(6,112) i,xna(k1),yna(l1),j,xna(k2),yna(l2),aa(i,j)
   61 continue
8002  continue
c-----------------------------------------------------*
c**** compute elements of genetic covariance matrix g *
c-----------------------------------------------------*
      do 44 i=1,nm2
      do 44 j=i,nm2
      k=(i-1)*nm2-(i-1)*i/2+j
      l=(j-1)*j/2+i
   44 g(k)=rg(l)*sg(i)*sg(j)*a(1)
      if(iversi(1).ge.2)goto 8003
      write(6,115)
  115 format (/1x,'*genetic (co)variance matrix g'/)
c-----------------------------------------------------------
      do 45 i=1,nm2
      do 45 j=i,nm2
      k=(i-1)*nm2-(i-1)*i/2+j
      write(6,116) i,yna(i),j,yna(j),g(k)
  116 format(1h ,i3,4x,1a6,i9,4x,1a6,5x,f16.5)
   45 continue
8003  continue
c------------------------------------------------*
c****         compute elements of c=ag           *
c------------------------------------------------*
      do 62 i=1,ninf
      do 62 j=1,nm2
      c(i,j)=0.
      do 62 k=1,nm2
      k1=k
      k2=j
      if(k1.lt.k2) go to 63
      kx=k1
      k1=k2
      k2=kx
  63  k3=(k1-1)*nm2-(k1-1)*k1/2+k2
   62 c(i,j)=c(i,j)+aa(i,k)*g(k3)
      if(iges.ne.1) go to 59
      do 53 i=1,ninf
   53 kontr(i)=0
      do 54 i=1,ninf
      ki=kk(i)
      do 55 j=1,ninf
      kj=kk(j)
      do 56 k=1,11
      if(ki.eq.ig1(k).and.kj.eq.ig2(k)) go to 57
      if(kj.eq.ig1(k).and.ki.eq.ig2(k)) go to 57
   56 continue
      go to 55
   57 if(ll(i).ne.ll(j)) go to 55
      if(ki.eq.13.or.ki.eq.9.or.ki.eq.11) jj=1
      if(ki.eq.12.or.ki.eq.14.or.ki.eq.8.or.ki.eq.10) jj=2
      if(jj.eq.1) m1=nn(i)
      if(jj.eq.2) m1=mm(i)
      ii=i
      if(kj.eq.13.or.kj.eq.9.or.kj.eq.11) jj=3
      if(kj.eq.12.or.kj.eq.14.or.kj.eq.8.or.kj.eq.10) jj=4
      if(jj.eq.3) m1=nn(j)
      if(jj.eq.4) m1=mm(j)
      if(jj.gt.2) ii=j
      if(kontr(ii).eq.1) go to 55
      do 58 l=1,nm2
      if(jj.le.2) c(ii,l)=(a(kj)+(m1-1)*a(ki))/float(m1)
     1*c(ii,l)/a(ki)
      if(jj.gt.2) c(ii,l)=(a(ki)+(m1-1)*a(kj))/float(m1)
     1*c(ii,l)/a(kj)
   58 continue
      kontr(ii)=1
   55 continue
   54 continue
59    continue
      if(iversi(1).ge.2) goto 8004
      write(6,113)
  113 format (/1x,'*genetic covariancematrix c=ag'/)
c-----------------------------------------------------
      do 43 i=1,ninf
      do 43 j=1,nm2
      k1=kk(i)
      l1=ll(i)
      k2=1
      l2=j
      write(6,112)i,xna(k1),yna(l1),j,xna(k2),yna(l2),c(i,j)
   43 continue
8004  continue
      if(iret.ne.1) go to 47
      read(4,128)(ret(j),j=1,ninf)
      if(iversi(1).ge.2) goto 8005
      write(6,122)
  122 format(/1x,'*selectiondifferentials for retrospective indices'/)
c---------------------------------------------------------------------
  128 format(8f10.3)
      do 49 j=1,ninf
      k1=kk(j)
      l1=ll(j)
   49 write(6,124) j,xna(k1),yna(l1),ret(j)
  124 format(1h ,i3,2x,a4,4x,1a6,f15.5)
8005  continue
   47 do 50 i=1,ninf
      ki=kk(i)
      li=ll(i)
      xname(i,1)=xna(ki)
      xname(i,2)=blank
      xname(i,3)=yna(li)
      xname(i,4) = blank
      xname(i,5)=blank
   50 continue
      do 51 i=1,nm2
      yname(i,1)=xna(1)
      yname(i,2)=blank
      yname(i,3) = yna(i)
      yname(i,4) = blank
      yname(i,5)=blank
   51 continue
c-----------------------------------------------------------------------
c*    berechnung der zweistufenselektion mit subroutine 'zwstre'
c*    izsme = anzahl der informations-karten (-quellen) vom 1.index
c-----------------------------------------------------------------------
      if(izsme.gt.0) goto 1110
      call selind (ninf,nm2,inr,la)
      sdiz(inr,2)=sdind
      if(mpop-1) 89,90,1
 1110 call zwstre (nm2,ninf,ninf1,inr)
      if(mpop-1) 89,90,1
   89 stop
      end



      subroutine selind(n,m,inr,la)
c=======================================================================
c****  program selind by e.p.cunningham
c**** program selind. computes selection index b*x for aggregate genotyp
c**** v*y. requires n(no. of x*s), m(no.of y*s), p(n*n cov matrix of
c**** x*s), g(n*m cov matrix of x*s with y*s), c(m*m cov matrix of y*s),
c**** v(m*1 vector of econ values of y*s), xnames, ynames. reduced index
c**** and/or restried indexes can be called by trailer cards
c=======================================================================
      character num*3,xn1*4,xn2*4,xn3*4,xn4*4
      character xname*4,yname*4,xnames*4,ch1r*4
      dimension work(25),w(325),km(25),
     1rhm(25),rval(25),
     2change(25) ,psave(325),gsave(25,25),xnames(25,5),limit(5),cash(25)
     3,num(25),ntis(25),mtis(25),nx(3)
      common/kar3/v(25)
      common/kar4/iversi(3)
      common/kar21/xname(25,5),yname(25,5)
      common/seli/p(325),g(25,25),c(325),yvar(25),ret(25),b(25)
      common/seli1/isub,iret,mti(25),nti(25)
      common/kovmat/sdind,sdiz(30,2),corr
      common/restri/pres(325),gres(25,25)
      common/restr1/irestr,mtir(25),ntir(25),km1(25)
      common/kar22/ch1r(25,5)
      data num/'  1','  2','  3','  4','  5','  6','  7','  8','  9',
     1' 10',' 11',' 12',' 13',' 14',' 15',' 16',' 17',' 18',' 19',' 20'
     2,' 21',' 22',' 23',' 24',' 25'/
      data xn1,xn2,xn3,xn4/'dumm','y= t','rait','    '/
      lx=la+1
      nx(lx)=n
      lp=0
c     input n,m,xnames,p,ynames+v,g,c
      nsave=n
      do 2 i=1,n
      mtis(i) = mti(i)
      ntis(i) = nti(i)
      do 302 j=1,5
  302 xnames(i,j)=xname(i,j)
    2 continue
      n1  =(n*(n+1))/2
      do 6 i=1,n1
      psave(i)=p(i)
    6 continue
      do 11 i=1,n
      do 11 j=1,m
      gsave(i,j)=g(i,j)
   11 continue
c------------------------------------------------------*
c****        get var of aggregate genotype = vcv       *
c------------------------------------------------------*
      n1=n
      n=m
      vartbv=0
      do 202 i=1,n
      sum=0
      do 203 j=1,n
      if(i-j)204,204,205
  204 l=(i-1)*n-((i-1)*i)/2+j
      go to 203
  205 l=(j-1)*n-((j-1)*j)/2+i
  203 sum=sum+c(l)*v(j)
  202 work(i)=sum
      do 17 i=1,m
      vartbv=vartbv+work(i)*v(i)
   17 continue
      n=n1
c**** multiply g by v
c--------------------
  150 k=n*(n+1)/2
      do 1811 i=1,k
 1811 pres(i)=p(i)
      do 1831 i=1,n
      mtir(i)=mti(i)
      ntir(i)=nti(i)
      do 1841 jj=1,5
 1841 ch1r(i,jj)=xname(i,jj)
      do 1831 j=1,m
 1831 gres(i,j)=g(i,j)
      do 14 i=1,n
      rhm(i)=0
      do 15 j=1,m
      rhm(i)=rhm(i)+g(i,j)*v(j)
   15 continue
      if(iret.eq.1) rhm(i)=ret(i)
   14 continue
c**** invert p into w
c--------------------
  992 do 995 i=2,n
      im1=i-1
      do 993 k=1,im1
 770  ki=n*(k-1)-(k*(k-1))/2+i
      kk1=n*(k-1)-(k*(k-3))/2
  993 work(k)=p(ki)/p(kk1)
      do 995 j=i,n
      ij=n*(i-1)-(i*(i-1))/2+j
      sum=p(ij)
      do 994 k=1,im1
      kj=n*(k-1)-(k*(k-1))/2+j
  994 sum=sum-p(kj)*work(k)
  995 p(ij)=sum
      do 911 i=1,n
      ip=n+1-i
      do 911 j=i,n
      jp=n+1-j
      if(ip-jp)997,996,997
  996 sum=1.
      if(ip-n)998,953,998
  997 sum=0.
  998 jp1=jp+1
      do 999 k=jp1,n
      jpk=n*(jp-1)-(jp*(jp-1))/2+k
      if(ip-k)950,950,951
  950 ipk=n*(ip-1)-(ip*(ip-1))/2+k
      go to 999
  951 ipk=n*(k-1)-(k*(k-1))/2+ip
  999 sum=sum-p(jpk)*w(ipk)
      if(ip-jp)952,952,953
  952 ipj=n*(ip-1)-(ip*(ip-1))/2+jp
      go to 954
  953 ipj=n*(jp-1)-(jp*(jp-1))/2+ip
  954 jpj=n*(jp-1)-(jp*(jp-3))/2
  911 w(ipj)=sum/p(jpj)
      nlim=(n*(n+1))/2
      if(iversi(1).ge.1) goto 8006
      write(6,61)
   61 format(//70(1h-),/1x,'* inverse von p'/)
c---------------------------------------------
      do 31 i=1,n
      do 31 j=i,n
      k=(i-1)*n-(i-1)*i/2+j
      write(6,62) i,(xname(i,l),l=1,4),j,(xname(j,l),l=1,4),w(k),w(k)
   62 format(1h ,2(i3,2x,a4,a2,2a4),f12.5,e16.8)
   31 continue
8006  continue
c**** get b-values=wgv
c---------------------
      do 232 i=1,n
      sum=0
      do 213 j=1,n
      if(i-j)214,214,215
  214 l=(i-1)*n-((i-1)*i)/2+j
      go to 213
  215 l=(j-1)*n-((j-1)*j)/2+i
  213 sum=sum+w(l)*rhm(j)
  232 b(i)=sum
c**** get var of index =bpb = bgv
c--------------------------------
      varind=0
      do 16 i=1,n
      varind = varind+rhm(i)*b(i)
   16 continue
      if(varind)556,557,557
  557 sdind=sqrt(varind)
      sdtbv=sqrt(vartbv)
      corr=sdind/sdtbv
c**** get rel values of variates
c-------------------------------
      do 19 i=1,n
      l=(i-1)*n-((i-1)*i)/2+i
      if(varind-(b(i)*b(i))/w(l)) 250,250,251
  251 rval(i)=100-sqrt((varind-(b(i)*b(i))/w(l))/varind)*100
      go to 19
  250 rval(i)=100
   19 continue
c**** get regr and corr of traits with index
c-------------------------------------------
      lx=la+1
      do 20 j=1,m
      change(j)=0
      work(j)=0
      do 21 i=1,n
      change(j)=change(j)+g(i,j)*b(i)
   21 continue
      work(j)=change(j)/(sdind*yvar(j))
      change(j)=change(j)/varind
      cash(j)=change(j)*v(j)*100
   20 continue
      corr2=varind/vartbv
c-------------------------------------------------------------*
c****                        begin output                     *
c-------------------------------------------------------------*
      write(6,23)
      write(6,405)
   23 format (/1x,'*','selection index : main index')
c-------------------------------------------------------
      do 30 i=1,n
      write(6,407) i,xname(i,1),mti(i),nti(i),(xname(i,j),j=3,5),b(i),
     1rval(i)
   30 continue
      write(6,24) varind,sdind
   24 format (1h0,' variance of index    =',f14.4,' stand. dev. index',
     1'  =',f10.4)
      write(6,25) vartbv,sdtbv
   25 format (1h ,' variance of ebv      =',f14.4,' stand. dev. ',
     1'ebv.   =',f10.4)
      write(6,26) corr2,corr
   26 format (1h ,' r-squared            =',f14.4,' corr. ',
     1'index-ebv     ',f10.4///)
      write(6,38)
   38 format (/1x,'*relationships between individual traits in ebv and 
     1index'/)
      write(6,37)
   37 format (1h ,12x,' trait ',10x,'regression correlation %-response 
     1response/trait'/)
      do 39 i=1,m
      dgi=change(i)*sdind
      write(6,42) i,(yname(i,j),j=1,5),change(i),work(i),cash(i),dgi
   42 format(i3,2x,5a4,f10.4,4x,3f12.4)
   39 continue
      write(6,4242)
4242  format(3x)
c**** get single trait subindexes
c--------------------------------
      if(isub.lt.1) go to 559
  400 do 401 k=1,m
      do 402 i=1,n
      sum=0
      do 403 j=1,n
      if(i-j)414,414,415
  414 l=(i-1)*n-((i-1)*i)/2+j
      go to 403
  415 l=(j-1)*n-((j-1)*j)/2+i
  403 sum=sum+w(l)*g(j,k)
  402 b(i)=sum
c**** get variance of subindex
c-----------------------------
      varind=0
      do 416 i=1,n
  416 varind=varind+b(i)*g(i,k)
      if(varind)566,558,558
  558 corr=(sqrt(varind))/yvar(k)
c**** get rel values of variates in subindex
c-------------------------------------------
      do 419 i=1,n
      l=(i-1)*n-((i-1)*i)/2+i
      if(varind-(b(i)*b(i))/w(l))350,350,351
  351 rval(i)=100-sqrt((varind-(b(i)*b(i))/w(l))/varind)*100
      go to 419
  350 rval(i)=100
  419 continue
c-------------------------------------------------------*
c****               output subindex                     *
c-------------------------------------------------------*
      write(6,404)(yname(k,j),j=1,5),varind,corr
  404 format (1h0,' subindex for trait',2x,5a4/1x,'0 variance of ',
     1'subindices',f10.4,4x,'correlation bw. subindex and trait',f10.4)
      write(6,405)
  405 format(/1h ,5x,'phenotypic information          b-value',13x,
     1'value of information'/)
      do 406 i=1,n
      write(6,407) i,xname(i,1),mti(i),nti(i),(xname(i,j),j=3,5),b(i),
     1rval(i)
  407 format(i3,2x,a4,i4,2x,i2,2x,3a4,4x,f10.4,14x,f10.4)
  406 continue
      go to 401
  566 write(6,567)(yname(k,i),i=1,5),varind
  567 format (1h ,'1 variance of subindices for trait',5x,5a4,5x,
     1'is negative:',f10.4)
  401 continue
c------------------------------------------------------------*
c****           begin reduced and/or restried indexes      *
c------------------------------------------------------------*
  559 n=nsave
      lp=lp+1
c****
c**** restriktion
c****
      if(la.eq.1.and.lp.eq.2) goto 1893
      if(la.eq.2.and.lp.eq.1) goto  553
      read(4,110)(km(i),i=1,25),(limit(i),i=1,5)
  110 format(30i2)
      do 1890 i=1,25
 1890 km1(i)=km(i)
553   if(km(1)+limit(1))5,5,555
  555 nn=n
      ii=0
      do 188 i=1,25
      if(km(i))189,188,189
  189 goto (1889,1887,1888) ,lx
 1887 if(km(i).le.nx(lx))         goto 1889
      km(i)=0
      goto 188
 1888 if(km(i).gt.(nx(lx-1)+ipr)) goto 1889
      km(i)=0
      goto 188
 1889 nn=nn-1
      irestr=irestr-1
      ii=ii+1
      km(ii)=km(i)
  188 continue
      do 190 i=1,5
      if(limit(i))191,190,191
  191 nn=nn+1
      irestr=irestr+1
  190 continue
c**** restore p g and xname
c--------------------------
  155 l=n*(n+1)/2
      do 235 i=1,l
      p(i)=psave(i)
      w(i)=0
  235 continue
      do 206 i=1,n
      do 207 j=1,m
      g(i,j)=gsave(i,j)
  207 continue
  206 continue
      do 208 i=1,n
      nti(i) = ntis(i)
      mti(i) = mtis(i)
      do 209 j=1,5
      xname(i,j)=xnames(i,j)
  209 continue
  208 continue
c****  any restriions +
c-------------------------
      if(limit(1))187,186,187
c**** impose restriions
c------------------------
  187 do 184 k=1,5
      if(limit(k))184,184,185
  185 nr=n+1
      kl=limit(k)
      xname(nr,1)=xn1
      xname(nr,2)=xn2
      xname(nr,3)=xn3
      xname(nr,4)=num(kl)
      xname(nr,5)=xn4
      do 183 j=1,m
  183 g(nr,j)=0
c**** add a column of g to p
c---------------------------
      do 180 iii=1,n
      i=n-iii+1
      do 180 jjj=1,n
      j=n-jjj+1
      l=(i-1)*n-((i-1)*i)/2+j
      ln=l+i-1
  180 p(ln)=p(l)
      l=0
      do 181 i=1,nr
      l=l+nr-i+1
  181 p(l)=g(i,kl)
  184 n=nr
c**** any deletions+
c-------------------
  186 if(km1(1))198,112,198
c**** mark elements of g and p for deletion
c------------------------------------------
  198 do 100 i=1,n
      if(km(i))101,101,102
  102 ii=km(i)
      g(ii,1)=999999
      do 103 j=1,n
      if(ii-j)130,130,131
  131 l=(j-1)*n-((j-1)*j)/2+ii
      go to 132
  130 l=(ii-1)*n-((ii-1)*ii)/2+j
  132 p(l)=999999
  103 continue
  100 continue
c***  remove rows and cols from p matrix
c---------------------------------------
  101 n1=n*(n+1)/2
      do 106 i=1,n1
      if (p(i)-999999)106,107,106
  107 ik=i+1
  122 if(p(ik)-999999)120,121,120
  121 ik=ik+1
      if(ik-n1)122,122,161
  120 p(i)=p(ik)
      p(ik)=999999
  106 continue
c     remove rows from g matrix and xname matrix
  161 do 111 i=1,n
      if (g(i,1)-999999)111,115,111
  115 ik=i+1
  116 if(g(ik,1)-999999)113,119,113
  119 ik=ik+1
      if(ik-n)116,116,112
  113 do 118 j=1,m
      g(i,j)=g(ik,j)
  118 continue
      g(ik,1)=999999
      do 114 j=1,5
      xname(i,j)=xname (ik,j)
  114 continue
      nti(i) = nti(ik)
      mti(i) = mti(ik)
  111 continue
c     start calculations for new index
  112 n=nn
      go to 150
  556 write(6,560)varind
  560 format (1h ,'1 variance of index is negative :',f10.4)
      go to 559
 1893 do 1892 i=1,25
      if(km1(i)) 1891,1892,1891
 1891 km(i)=km1(i)+irestr
 1892 continue
      ipr=irestr
    5 return
      end
      subroutine zwstre(nm,ninf,ninf1,inr)
c**** subroutine zur erstellung der matrizen fuer die berechnung **
c**** der zwischenstufenkorrelationskoeffizient unter berueck-   **
c**** sichtigung von reduktion   und restriktion                 **
      character * 4 ch,ch1,ch1s,ch1r
      dimension psave(325),gsave(25,25),ch1s(25,5),mtis(25),ntis(25)
     *,ixy(25)
      common/kovmat/sdind,sdiz(30,2),corr
      common/seli/pneu(325),gneu(25,25),c(325),re(25),rere(25),b(25)
      common/seli1/int(2),mtin(25),ntin(25)
      common/restri/pres(325),gres(25,25)
      common/restr1/irestr,mtir(25),ntir(25),km1(25)
      common/zwistu/qr(25,25),b1(25),b2(25)
      common/kar4/iversi(3)
      common/kar21/ch1(25,5),ch(25,5)
      common/kar22/ch1r(25,5)
c-------------------------------------------------------------------*
c            speicherung der gesamtmatrizen und namen               *
c-------------------------------------------------------------------*
      do 795 i=1,25
      km1(i)=0
      mtis(i)=mtin(i)
      ntis(i)=ntin(i)
      do 796 j=1,5
  796 ch1s(i,j)=ch1(i,j)
      do 795 j=1,25
      if(j.lt.i) go to 795
      k=(i-1)*25-(i-1)*i/2+j
      psave(k)=pneu(k)
  795 gsave(i,j)=gneu(i,j)
c---------------------------------------------------------------*
c     berechnung der 1.stufe   durch selind-aufruf              *
c---------------------------------------------------------------*
      k1=0
      do 2 i=1,ninf1
      do 2 j=i,ninf1
      k1=k1+1
      k2=(i-1)*ninf-(i-1)*i/2+j
    2 pneu(k1)=psave(k2)
      do 3 i=1,ninf1
      do 3 j=1,nm
    3 gneu(i,j)=gsave(i,j)
      irestr=0
      call selind (ninf1,nm,inr,1)
      ni=ninf1+irestr
      nin=ninf+irestr
      do 4 i=1,ni
    4 b1(i)=b(i)
      sdiz(inr,1)=sdind
c-----------------------------------------------------------------------
c     erstellung der matrix fuer die 2.stufe  unter beruecksichtigung
c                von reduktion   und    restriktion
c-----------------------------------------------------------------------
      do 252 i=1,ninf1
  252 ixy(i)=i
      do 253 i=1,25
      if(km1(i).eq.0.or.km1(i).gt.ninf1) goto 254
  253 ixy(km1(i))=0
  254 ii=0
      do 255 i=1,ninf1
      if(ixy(i).eq.0) goto 255
      ii=ii+1
      ixy(ii)=ixy(i)
  255 continue
      n=ninf1-ii
      do 1 i=1,nin
      ix=ixy(i)
      if(i-ni) 750,750,76
  750 mtin(i)=mtir(i)
      ntin(i)=ntir(i)
      do 20 j=1,nm
   20 gneu(i,j)=gres(i,j)
      do 755 jj=1,5
  755 ch1(i,jj)=ch1r(i,jj)
      go to 77
   76 ix=i-irestr
      mtin(i)=mtis(ix)
      ntin(i)=ntis(ix)
      do 21 j=1,nm
   21 gneu(i,j)=gsave(ix,j)
      do 756 jj=1,5
  756 ch1(i,jj)=ch1s(ix,jj)
   77 do 1 j=i,nin
      jx=j
      k2=(i-1)*(nin)-(i-1)*i/2+j
      if(i.le.(ninf1-n).or.i.gt.ni) goto 25
      pneu(k2)=0.
      goto 1
   25 if(j.gt.ni) goto 26
      k3=(i-1)*ni-(i-1)*i/2+j
      pneu(k2)=pres(k3)
      goto 1
   26 if(j.gt.ni) jx=j-irestr
      k1=(ix-1)*ninf-(ix-1)*ix/2+jx
      pneu(k2)=psave(k1)
    1 continue
      irestr=0
c----------------------------------------------------------------*
c     berechnung der 2.stufe  durch  selind-aufruf               *
c----------------------------------------------------------------*
      call selind(nin,nm,inr,2)
      do 6 i=1,nin+irestr
    6 b2(i)=b(i)
      sdiz(inr,2)=sdind
      ninf2=nin+irestr
c--------------------------------------------------------*
c       erstellung der matrix   fuer die berechnung      *
c       des zwischenstufekorrelationskoeffizienten       *
c--------------------------------------------------------*
      write(6,799)
799   format(1h1,'***variance-covariance-matrix between the indices***')
800   do 74 i=1,ni
      do 74 j=i,ninf2
      k=(i-1)*ninf2-(i-1)*i/2+j
      qr(i,j)=pres(k)
      if(j.gt.ni) goto 74
      qr(j,i)=qr(i,j)
 74   continue
c***  die zahl 13 gibt die max zahl der informationsmerkmale vom 1.index
      npr=ninf2
      if(ninf2.gt.13) npr=13
      write(6,797) (j,j=1,npr)
      write(6,802)
      do 80 i=1,ni
  80  write(6,798) i,(qr(i,j),j=1,npr)
      if(npr.ne.13)  goto 801
      write(6,797) (j,j=14,ninf2)
      write(6,802)
      do 81 i=1,ni
  81  write(6,798) i,(qr(i,j),j=14,ninf2)
801   call indkor(inr,ni,ninf2)
      return
797   format(//1h0,6x,13(i2,8x))
798   format(1x,i2,13(f10.4))
802   format(/1h0)
      end
      subroutine indkor(inr,nme1,nme2)
c**** ===========================================================**
c**** subroutine zur ermittlung der zwieschenstufenkorrelations- **
c**** koeffizient                                                **
c**** ========================================================== **
      common/kar4/iversi(3)
      common/kovmat/sdind,sdiz(30,2),corr
      common/zwistu/pkov(25,25),b1(25),b2(25)
      s=0
      zwst=0
      do 1 i=1,nme1
      do 2 j=1,nme2
2     s=s+b1(i)*b2(j)*pkov(i,j)
1     continue
      zwst=s/(sdiz(inr,1)*sdiz(inr,2))
      write(6,10)s,sdiz(inr,1),sdiz(inr,2),zwst
10    format(1x,'covariance between both indices       = ',f15.5/
     1       1x,'std.def. index 1. selection stage     = ',f15.5/
     2       1x,'std.def. index 2. selection stage     = ',f15.5/
     3       1x,'correlation between both indices      = ',f15.5//)
5     return
      end
