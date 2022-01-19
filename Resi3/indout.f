      subroutine indout(head,head2,a,chi,vi,vh,rih,gain,b,cyi,dgy,rgyi,
     + ntr,ntr4,l,inew,isr,label,nh,nf,no)
c-----prints output from the index job
c-----
      implicit double precision (a-h,o-z)
      common /limits/ mvltr,mvltr2,mvntr,mvnw,mvndg,mvns,mvntr4
      common /stdio/ lc,lp,lerr
c-----
      character*1 FF
      character*100 head,head2
      character*1 rel
      character*10 label(mvntr)
      dimension a(mvnw,mvntr)
      dimension b(mvnw,mvntr4)
      dimension cyi(mvntr4),dgy(mvntr4),rgyi(mvntr4),l(mvntr)
      dimension rel(4)  
c-----
      data FF /z'0c'/
      data rel(1),rel(2),rel(3),rel(4) /'I','H','F','O'/
c-----
      write(lp,1)FF
    1 format(a)
      write(lp,2)head2
    2 format(/a)
      write(lp,2)head
      write(lp,3)inew
    3 format(/'ECONOMIC WT SET',i3)
      if(isr.eq.0) then
        write(lp,44)
   44   format(/'NO RESTRICTIONS')
      else
        write(lp,4)isr
    4   format(/'RESTRICTION SET',i3)
      endif
      write(lp,5)
    5 format(/' LIB  IND TRAIT NAME *     ECON     INDEX_B  COV(TRAIT
     + DELTA_G   RG(TRAIT')
      write(lp,6)
    6 format('  NO   NO',11X,' *',7X,'WT COEFFICIENT    ,INDEX)'
     + '    (TRAIT)    ,INDEX)')
c-----

c-----loop over types of relatives in index
      count=0
      k=1
      do 10 i=1,ntr4
      j=(i-1)/ntr
      j=i-j*ntr
c     j goes 1 to ntr for each relative
      count=count+1
      if(count.gt.ntr)then
        k=k+1
c       k is relative number
        count=1
      endif
c-----print out the results!
      if(i.gt.ntr) then
        if(k.eq.2.and.nh.eq.0 .or. k.eq.3.and.nf.eq.0 .or.
     +     k.eq.4.and.no.eq.0) then
          continue
        else
          write(lp,7)l(j),i,label(j),rel(k),0.d0,b(inew,i),cyi(i),
     +     dgy(i),rgyi(i)
        endif
      else
        write(lp,7)l(j),i,label(j),rel(k),a(inew,j),b(inew,i),cyi(i),
     +   dgy(i),rgyi(i)
      endif
   10 continue
    7 format(1x,i3,2x,i3,1x,a10,1x,a1,f9.2,f12.4,3f11.4)
c-----
      write(lp,732)
  732 format(/' ** - I = individual value'
     + /'    - H = half sib family mean (individual included)'
     + /'    - F = full sib family mean (individual included)'
     + /'    - O = progeny test mean (individual not included,'
     +  ' parents unselected)')
      write(lp,733)
  733 format(/' genetic gains for each trait on selection for index'
     + ' (delta_G)'/'are in trait_units per unit selection differential'
     + ' per generation')
      write(lp,111) gain,chi
  111 format(//' genetic gain in aggregate genotype on selection for'
     +  ' index'/'   - in economic weight units' 
     +/'  - per unit selection differential per generation =',f15.4
     +//' covariance of aggregate genotype & index =',f15.4)
      write(lp,125)vi,vh,rih
  125 format(/' variance of index = ',f15.4//
     +' variance of aggregate genotype = ',f15.4//
     +' correlation of aggregate genotype & index = ',f15.4)
      write(lp,734) rih*rih
  734 format(/' heritability of index = ',f15.4)
      return
      end



