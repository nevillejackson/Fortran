c      program selind by e.p.cunningham
c     program selind. computes selection index b'x for aggregate genotype
c     v'y. requires n(no. of x's), m(no.of y's), p(n*n cov matrix of
c     x's), g(n*m cov matrix of x's with y's), c(m*m cov matrix of y's),
c     v(m*1 vector of econ values of y's), xnames, ynames. reduced indexes
c     and/or restricted indexes can be called by trailer cards
c
c     multi-stage version by e.p.cunningham + g.a.t.mahon.
c     two stage indexes can be called by trailer cards, which should
c     specify variates * not * included in first round of selection,
c     and percentage of candidates selected in first round.
c
c	double precision seldat,selres
      character*10 seldat,selres
      common p(325),g(25,25),bsave(25),c(325),
     1psave(325),gsave(25,25),csave(325),npc,varins
      dimension   v(25),work(25),w(325),km(25),
     1xname(25,5),yvar(25),yname(25,5) ,rhm(25),rval(25),
     2change(25) , xnames(25,5),limit(5),cash(25),b(25)
      equivalence(w(1),c(1))
      character*3 num(25)
      character*3 xname,xnames
      character*4 xn1,xn2,xn3,xn4
      data num/'  1','  2','  3','  4','  5','  6','  7','  8','  9',' 1
     1 ',' 11',' 12',' 13',' 14',' 15',' 16',' 17',' 18',' 19',' 20',' 2
     21',' 22',' 23',' 24',' 25'/
      data xn1,xn2,xn3,xn4/'dumm','y= t','rait','    '/
c     input n,m,xnames,p,ynames+v,g,c
      mult=0
c	type 5000
 5000	format(1x,'type data file name')
c	accept 5200,seldat
 5200  format(a10)
      seldat='dat.inp'
	open(unit=5,file=seldat)
c      type 5500
5500  format(1x,'t pe result file name')
c      accept 5700,selres
700   format(a10)
      selres='result.out'
	open(unit=6,file=selres)
   55 read(5,*)n,m,nr
    1 format (3i5)
      nsave=n
      msave=m
      write(6,60)n,m
  60  format('1',54('*')/' selection index input data .',i5,
     1 ' variates',i5,' traits'/' ',54('*'))
      do 2 i=1,n
      read(5,3)(xname(i,j),j=1,5)
    3 format(5a4)
      write(6,61)i,(xname(i,j),j=1,5)
  61  format(i3,2x,5a4)
      do 302 j=1,5
  302 xnames(i,j)=xname(i,j)
    2 continue
      n1  =(n*(n+1))/2
      write(6,62)
  62  format(///1x,'p-matrix'/)
      write(6,69)
  69  format(10x,'j         k            covariance'/)
      do 6 i=1,n1
      read(5,*)j,k,cov
    7 format(2i5,f10.0)
      l=(j-1)*n-((j-1)*j)/2+k
      p(l)=cov
      psave(l)=cov
      write(6,63)j,k,cov
  63  format(1x,2i10,f20.5)
    6 continue
      write(6,64)
  64  format(///)
      do 9 i=1,m
      read(5,10)(yname(i,j),j=1,5),v(i)
   10 format(5a4,f10.0)
      write(6,66)i,(yname(i,j),j=1,5),v(i)
  66  format(i3,2x,5a4,f20.5)
    9 continue
      n1=n*m
      write(6,67)
  67  format(///1x,'g-matrix'/)
      write(6,69)
      do 11 i=1,n1
      read(5,*)j,k,cov
      g(j,k)=cov
      gsave(j,k)=cov
      write(6,63)j,k,cov
   11 continue
      write(6,68)
  68  format(///1x'c-matrix'/)
      write(6,69)
      n1=(m*(m+1)/2)
      do 13 i=1,n1
      read(5,*)j,k,cov
      l=(j-1)*m-((j-1)*j)/2+k
      c(l)=cov
      csave(l)=cov
      write(6,63)j,k,cov
      if(j-k)13,40,13
  40  yvar(j)=sqrt(cov)
   13 continue
      write(6,1999)
1999  format('1',22(1h*)/' single stage selection'/' ',22(1h*))
      goto 2000
 2001 continue
      k=1
      do 2002 j=1,m
      yvar(j)=sqrt(c(k))
      k=k+m-j+1
 2002 continue
 2000 continue
c     get var of aggregate genotype = vcv
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
c     multiply g by v
      do 14 i=1,n
      rhm(i)=0
      do 15 j=1,m
      rhm(i)=rhm(i)+g(i,j)*v(j)
   15 continue
   14 continue
c     invert p into w
  992 do 995 i=2,n
      im1=i-1
      do 993 k=1,im1
 770  ki=n*(k-1)-(k*(k-1))/2+i
      kk=n*(k-1)-(k*(k-3))/2
  993 work(k)=p(ki)/p(kk)
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
c optional printing of p-inverse
      nlim=(n*(n+1))/2
c remove  c  from col. 1 of next line
c     write(6,1001)(w(i),i=1,nlim)
 1001 format(1h-,7(2x,7f11.6//))
c     get b-values=wgv
      do 232 i=1,n
      sum=0
      do 213 j=1,n
      if(i-j)214,214,215
  214 l=(i-1)*n-((i-1)*i)/2+j
      go to 213
  215 l=(j-1)*n-((j-1)*j)/2+i
  213 sum=sum+w(l)*rhm(j)
  232 b(i)=sum
c     get var of index =bpb = bgv
      varind=0
      do 16 i=1,n
      varind = varind+rhm(i)*b(i)
   16 continue
      varins=varind
      if(varind)556,557,557
  557 sdind=sqrt(varind)
      sdtbv=sqrt(vartbv)
      corr=sdind/sdtbv
c     get rel values of variates
      do 19 i=1,n
      l=(i-1)*n-((i-1)*i)/2+i
      if(varind-(b(i)*b(i))/w(l)) 250,250,251
  251 rval(i)=100-sqrt((varind-(b(i)*b(i))/w(l))/varind)*100
      go to 19
  250 rval(i)=100
   19 continue
c     get regr and corr of traits with index
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
c     begin output
      write(6,23)
      write(6,405)
   23 format(' selection index  =  main index'/' ',29('-'))
      do 30 i=1,n
      write(6,407)i,(xname(i,j),j=1,5),b(i),rval(i)
      bsave(i)=b(i)
   30 continue
      write(6,24)varind
   24 format(///22h variance of index    ,f10.4)
      write(6,25)sdind
   25 format(22h  std dev of index    ,f10.4,' (2)')
      write(6,26)vartbv
   26 format(//22h variance of agg genot,f10.4)
      write(6,27) sdtbv
   27 format(22h  std dev of agg genot ,f10.4)
      write(6,28)corr
   28 format(///35h correlation of index and agg genot,f10.4)
      write(6,38)
   38 format(///' regression of each trait on index,correlation of each
     1trait with index,percent of economic gains (3)')
      write(6,37)
   37 format(' ',12x,'trait',12x,'regression   correlation  pct of gain'
     1)
      do 39 i=1,m
      write(6,42) i,(yname(i,j),j=1,5),change(i),work(i),cash(i)
   42 format(i3,2x,5a4,4x,f10.4,4x,2f10.4)
   39 continue
      write(6,29)
   29 format(///' (1) value of each variate in the index = percent reduc
     1tion'/' in rate of overall genetic gain if that variate is omitted
     2'/'0(2) this is the value,in economic units,of the genetic gain in
     3'/' aggregate genotype achieved by one standard deviation of selec
     4tion on the index'/'0(3) these figures are the percentages of tota
     5l gain (2)'/' accounted for by gain in each trait')
c     get single trait subindexes
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
c     get variance of subindex
      varind=0
      do 416 i=1,n
  416 varind=varind+b(i)*g(i,k)
      if(varind)566,558,558
  558 corr=(sqrt(varind))/yvar(k)
c     get rel values of variates in subindex
      do 419 i=1,n
      l=(i-1)*n-((i-1)*i)/2+i
      if(varind-(b(i)*b(i))/w(l))350,350,351
  351 rval(i)=100-sqrt((varind-(b(i)*b(i))/w(l))/varind)*100
      go to 419
  350 rval(i)=100
  419 continue
c     output subindex
      write(6,404)(yname(k,j),j=1,5),varind,corr
  404 format(88(1h-)/' ',
     1         'subindex for trait',4x,5a4,/'0','var1ance 0f subindex'
     1,4x,f10.4,6x,'correlation of subindex and trait',4x,f10.4)
      write(6,405)
  405 format(///10x,'variate',15x,'b-value',13x,'value of variate (1)')
      do 406 i=1,n
      write(6,407)i,(xname(i,j),j=1,5),b(i),rval(i)
  407 format(i3,2x,5a4,4x,f10.4,14x,f10.4)
  406 continue
      go to 401
  566 write(6,567)(yname(k,i),i=1,5),varind
  567 format('-','variance of subindex for trait',5x,5a4,5x,'is negative
     2 =',f10.4)
  401 continue
c     begin reduced and/or restricted indexes
  559 n=nsave
      if(mult.eq.0)goto 561
      call multi(nsave,msave,nmvar)
      write(6,563)
  563 format(//'1***********************************'
     1/         ' multi-stage selection= second round'
     2/         ' **********************************')
      n=nsave
      m=msave
      mult=0
      goto 2001
  561 continue
      read(5,110)(km(i),i=1,25),(limit(i),i=1,5),mult,npc
  110 format(32i2)
      if(km(1)+limit(1))55,5,555
  555 nn=n
      write(6,570)
      write(6,571)(km(i),i=1,25)
      write(6,572)(limit(i),i=1,5)
 570  format(1h1)
  571 format(/'    deletions=',25i5)
  572 format(/' restrictions=',5i5)
      if(mult.eq.1)write(6,562)npc
  562 format(//'1**********************************'
     1/         ' multi-stage selection= first round'
     2/         ' *********************************'
     3/'0    proportion selected = ',i2,'(')
      do 188 i=1,25
      if(km(i))189,188,189
  189 nn=nn-1
  188 continue
      nmvar=nn
      do 190 i=1,5
      if(limit(i))191,190,191
  191 nn=nn+1
  190 continue
c     restore p g c and xname
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
      do 209 j=1,5
      xname(i,j)=xnames(i,j)
  209 continue
  208 continue
      l=m*(m+1)/2
      do 236 j=1,l
      c(j)=csave(j)
  236 continue
c      any restrictions
      if(limit(1))187,186,187
c     impose restrictions
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
c     add a column of g to p
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
c     any deletions
  186 if(km(1))198,112,198
c     mark elements of g and p for deletion
  198 do 100 i=1,n
      if(km(i))101,101,102
  102 ii=km(i)
      g(ii,1)=99999999
      do 103 j=1,n
      if(ii-j)130,130,131
  131 l=(j-1)*n-((j-1)*j)/2+ii
      go to 132
  130 l=(ii-1)*n-((ii-1)*ii)/2+j
  132 p(l)=99999999
  103 continue
  100 continue
c     remove rows and cols from p matrix
  101 n1=n*(n+1)/2
      do 106 i=1,n1
      if(p(i)-99999999)106,107,106
  107 ik=i+1
  122 if(p(ik)-99999999)120,121,120
  121 ik=ik+1
      if(ik-n1)122,122,161
  120 p(i)=p(ik)
      p(ik)=99999999
  106 continue
c     remove rows from g matrix and xname matrix
  161 do 111 i=1,n
      if(g(i,1)-99999999)111,115,111
  115 ik=i+1
  116 if(g(ik,1)-99999999)113,119,113
  119 ik=ik+1
      if(ik-n)116,116,112
  113 do 118 j=1,m
      g(i,j)=g(ik,j)
  118 continue
      g(ik,1)=99999999
      do 114 j=1,5
      xname(i,j)=xname (ik,j)
  114 continue
  111 continue
c     start calculations for new index
  112 n=nn
      go to 2000
  556 write(6,560)varind
  560 format('-','variance of index is negative =',f10.4)
      go to 559
    5 continue
      write(6,62621)
62621  format('   this is the last line i hope')
	close(unit=5)
	close(unit=6)
      stop
      end
      subroutine multi(n,m,nr)
      common p(325),g(25,25),b(25),c(325),
     1psave(325),gsave(25,25),csave(325),npc,varins
      dimension pe(15,15),gt(15,15),ce(15,15),gm(15,15)
     1,s(30,30),ss(30,30),ssp(30,30),
     2bt(30),btt(30),t(15,30),sk(99)
      data sk/0.9042,0.8897,0.8783,0.8695,0.8614,0.8547,0.8484,0.8421,
     10.8361,0.8305,0.8256,0.8201,0.8150,0.8101,0.8053,0.8001,0.7953,
     10.7906,0.7860,0.7814,0.7768,0.7721,0.7677,0.7629,0.7583,0.7539,
     10.7493,0.7447,0.7400,0.7355,0.7307,0.7262,0.7216,0.7168,0.7122,
     10.7074,0.7026,0.6979,0.6931,0.6882,0.6833,0.6783,0.6733,0.6682,
     10.6631,0.6580,0.6527,0.6474,0.6420,0.6366,0.6311,0.6256,0.6199,
     10.6141,0.6083,0.6024,0.5964,0.5903,0.5840,0.5777,0.5713,0.5648,
     10.5581,0.5513,0.5443,0.5372,0.5299,0.5225,0.5150,0.5072,0.4992,
     10.4911,0.4827,0.4741,0.4653,0.4562,0.4469,0.4372,0.4273,0.4170,
     10.4063,0.3953,0.3839,0.3719,0.3595,0.3466,0.3330,0.3188,0.3038,
     10.2879,0.2711,0.2532,0.2339,0.2131,0.1903,0.1652,0.1368,0.1039,
     10.0634/
      w=sk(npc)/varins
      j=n*(n+1)/2
      k=m*(m+1)/2
      nt=n+m
      np=n+1
c
      call store(psave,j,pe,n,1)
      call store(csave,k,ce,m,1)
      call smat(gsave,25,25,1,1,gm,n,m,1,1,n,m)
      call mtrans(gm,gt,n,m)
      call smat(pe,n,n,1,1,s,nt,nt,1,1,n,n)
      call smat(gm,n,m,1,1,s,nt,nt,1,np,n,m)
      call smat(gt,m,n,1,1,s,nt,nt,np,1,m,n)
      call smat(ce,m,m,1,1,s,nt,nt,np,np,m,m)
      call smat(s,nt,nt,1,1,t,nr,nt,1,1,nr,nt)
      call mmmult(b,t,bt,1,nr,nt)
      call mtrans(bt,btt,1,nt)
      call mmmult(btt,bt,ss,nt,1,nt)
      call msmult(ss,w,ssp,nt,nt)
      call msub(s,ssp,ss,nt,nt)
      call smat(ss,nt,nt,1,1,pe,n,n,1,1,n,n)
      call smat(ss,nt,nt,1,np,gm,n,m,1,1,n,m)
      call smat(ss,nt,nt,np,np,ce,m,m,1,1,m,m)
      call smat(gm,n,m,1,1,g,25,25,1,1,n,m)
      call store(p,j,pe,n,0)
      call store(c,k,ce,m,0)
      return
      end
      subroutine mtrans(ray1,ray2,i,j)
      dimension ray1(i,j),ray2(j,i)
      do 1 k=1,j
      do 1 l=1,i
    1 ray2(k,l)=ray1(l,k)
      return
      end
      subroutine smat(ray1,i,j,k,l,ray2,ii,jj,kk,ll,m,n)
      dimension ray1(i,j),ray2(ii,jj)
      do 1 ir=1,m
      ir1=ir+k-1
      ir2=ir+kk-1
      do 1 ic=1,n
      ic1=ic+l-1
      ic2=ic+ll-1
      ray2(ir2,ic2)=ray1(ir1,ic1)
    1 continue
      return
      end
      subroutine mmmult(ray1,ray2,rayp,l,m,n)
      dimension ray1(l,m),ray2(m,n),rayp(l,n)
      do 3 i=1,l
      do 2 j=1,n
      rayp(i,j)=0
      do 1 k=1,m
      rayp(i,j)=rayp(i,j)+ray1(i,k)*ray2(k,j)
    1 continue
    2 continue
    3 continue
      return
      end
      subroutine msmult(ray1,sca,rayp,n,m)
      dimension ray1(n,m),rayp(n,m)
      do 1 j=1,n
      do 1 k=1,m
    1 rayp(j,k)=ray1(j,k)*sca
      return
      end
      subroutine msub(ray1,ray2,rays,n,m)
      dimension ray1(n,m),ray2(n,m),rays(n,m)
      do 1 j=1,n
      do 1 k=1,m
    1 rays(j,k)=ray1(j,k)-ray2(j,k)
      return
      end
      subroutine store(rays,k,rayg,n,icode)
      dimension rays(k),rayg(n,n)
      m=0
      l=0
      if(icode.eq.0)goto 2
      do 1 i=1,n
      l=l+1
      do 1 j=l,n
      m=m+1
      rayg(i,j)=rays(m)
    1 rayg(j,i)=rays(m)
      goto 4
    2 do 3 i=1,n
      l=l+1
      do 3 j=l,n
      m=m+1
    3 rays(m)=rayg(i,j)
    4 return
      end

