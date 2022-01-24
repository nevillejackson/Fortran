      program gflow
c     This program produces geneflow and discounted expressions
c     by the method of Hill (1974) (Anim. Prod. 18: 117-139).
c     It is supposed to work for any type of population structure.
c     Population structure is defined by subdivision of the 
c     population in 'tiers', nucleus, sub_nucleus, etc.
c     As all standard programs it may contain errors, so always
c     check output carefully. Prepared for Scandanavian post-
c     graduate course in Helsinki, Aug 28-Sept 8 1978, E. W. Brascamp.
c     Res. Inst. for Animal Husbandry, Box 501, Zeist,
c     The Netherlands.
c
      implicit double precision (a-h,o-z)
      character*25 job,iname,jname(5)
      dimension cor(5)
      dimension nt(5),nm(5),ns(5,2),ne(5,2),q(40,40),p(40,40)
      dimension r(40,40),re(10),ks(10),ke(10),h(40)
      dimension xm(40),xmfix(40),ls(5,10)
      dimension dm(40),sde(5,5)
      dimension lsub(5)
c
   40 read(5,*)job
      write(8,205)job
  205 format(//'jobname: ',a//)
      write(9,205) job
c
      read(5,*)nh,nj,np,nr,(re(i),i=1,nr)
      do 1 i=1,nh
        read(5,*)nt(i),nm(i)
        if(i.eq.1) then
          ns(i,1)=1
        else
          j=i-1
          ns(i,1)=ne(j,2)+1
        endif
        ns(i,2)=ns(i,1)+nm(i)
        ne(i,1)=ns(i,2)-1
        ne(i,2)=ns(i,1)+nt(i)-1
    1 continue
c
      write(8,300)nh
  300 format(20x,'no of tiers = ',i2)
      write(8,301)
  301 format(14x,'tier  ageclasses  male ageclasses')
      do 302 i=1,nh
  302 write(8,303) i,nt(i),nm(i)
  303 format(15x,i2,7x,i2,12x,i2)
c
      j=0
      do 17 i=1,nh
        j=j+1
        if(nm(i).ne.0) then
          ks(j)=ns(i,1)
          ke(j)=ne(i,1)
          j=j+1
        endif
        ks(j)=ns(i,2)
        ke(j)=ne(i,2)
   17 continue
c
c     produce Q_matrix
      ndim=ne(nh,2)
      write(8,304) ndim
  304 format(21x,'total number of age classes = ',i2//)
      do 60 i=1,ndim
        do 60 j=1,ndim
   60 q(i,j)=0.d0
      do 5 i=2,ndim
        j=i-1
        q(i,j)=1.d0
        do 6 k=1,nh
          do 6 l=1,2
          if(i.eq.ns(k,l)) then
            q(i,j)=0.d0
            go to 5
          endif
    6   continue
    5 continue
c
c     define P_matrix
c     read non-zero elements only, identified by row & col number
c     do not read the 1's for ageing
c     end with a zero card
   50 continue
      do 51 i=1,ndim
        do 51 j=1,ndim
   51 p(i,j)=0.d0
    9 read(5,*) i,j,pp
      if(i.ge.1) then
        p(i,j)=pp
        go to 9
      endif
      do 11 i=1,ndim
        do 11  j=1,ndim
   11 p(i,j)=p(i,j)+q(i,j)
      do 310 i=1,ndim
        pp=0.00000000d0
        do 311 j=1,ndim
  311   pp=pp+p(i,j)
      ipp=pp*10**8-1
      if(ipp.gt.10**8) then
        write(8,313) i
  313   format(' row ',i2,' in matrix P gives a sum larger than 1.',
     +    /,' there may be more errors'/)
        do 314 ii=1,ndim
  314   write(8,200) (p(ii,k),k=1,ndim)
        stop
      endif
  310 continue
c
c     define incidence-vector (ndim elements)
      read(5,*)(h(i),i=1,ndim)
      write(8,206)
  206 format(' P_matrix',/)
      do 27 i=1,ndim
   27 write(8,200)(p(i,j),j=1,ndim)
  200 format(1x,21(f5.3,1x))
      write(8,201)
  201 format(//)
      write(8,207)
  207 format(' incidence vector')
      write(8,200) (h(i),i=1,ndim)
      write(8,201)
      do 12 jj=1,np
        read(5,*)iname
c
c       define one initial n-vector & R-matrix per pathway
c
        read(5,*)nm1
        do 13 i=1,ndim
          xm(i)=0.d0
   13   xmfix(i)=0.d0
        xm(nm1)=1.d0
        xmfix(nm1)=1.d0
        write(8,1208) iname
 1208   format(' initial vector for gene source ',3x,a/)
        write(8,200)(xm(i),i=1,ndim)
        write(8,201)
c
c       input R-matrix as P-matrix
c
        do 14 i=1,ndim
          do 14 j=1,ndim
   14   r(i,j)=0.d0
   10   read(5,*) i,j,rr
        if(i.ge.1) then
          r(i,j)=rr
          go to 10
        endif
        write(8,208) iname
  208   format(' R_matrix ',3x,a/)
        do 28 i=1,ndim
   28   write(8,200)(r(i,j),j=1,ndim)
        write(8,201)
c
c       define for discounted expr which tier and sex freq are to be included
        read(5,*) nd,(cor(i),i=1,nd)
        do 20 i=1,nr
          do 20 j=1,nd
   20   sde(i,j)=0.d0
        do 16 i=1,nd
          read(5,*)jname(i)
          read(5,*)nsub,(ls(i,j),j=1,nsub)
   16   lsub(i)=nsub
        ii=0
        write(8,209) iname
  209   format(' gene frequencies, originating from ',a/)
        write(9,210) iname
  210   format(' discounted expressions, originating from 'a/)
        write(9,305) nd
  305   format(' there are ',i2,' sets of discounted expressions',
     +  ' , identified as : ')
        do 306 i=1,nd
  306   write(9,307)i,jname(i),cor(i)
  307   format('   ',i2,2x,a,' time adj.= ',f7.3)
        write(9,308) nr,(re(i),i=1,nr)
  308   format(' within these there are ',i2,' interest rates : ',
     +   10(f4.2,1x)/1x)
        write(9,201)
        do 19 kj=2,nj
          call pathse(p,q,r,xm,xmfix,dm,ndim,ii)
          ii=ii+1
c       
c         write frequencies
          write(8,204) kj,(dm(i),i=1,ndim)
  204     format(' ',i2,1x,25(f4.3,1x))
            do 21 kd=1,nd
              do 21 kr=1,nr
                ren=(1.d0/(1.d0+re(kr)))**kj
                x=cor(kd)
                ren1=(1./(1.+re(kr)))**x
                ren=ren*ren1
                nsub=lsub(kd)
                do 21 ksub=1,nsub
                  i=ls(kd,ksub)
                  k=ks(i)
                  l=ke(i)
                  do 21 jx=k,l
   21       sde(kr,kd)=sde(kr,kd)+h(jx)*dm(jx)*ren
c
c         write standard discounted expressions
            write(9,214)kj,((sde(i,jx),i=1,nr),jx=1,nd)
  214       format(' ',i2,18(f7.3))
   19   continue      
c
        write(8,201)
        write(9,201)
   12 continue
c
      write(8,201)
      write(9,201)
      read(5,*) nend
      if(nend.lt.1) then
        stop
      else if(nend.eq.1) then
        go to 40
      else if (nend.gt.1) then
        go to 50
      endif
      stop
      end
