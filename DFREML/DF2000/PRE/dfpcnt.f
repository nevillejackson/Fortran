!============================================================================
      SUBROUTINE DFPCNT (nanim,iopt,npru,iun22,iun33,iun66,nmeta)
!============================================================================

      use pedigrees
      use list_of_ids
      use ped_nos
      USE form_of_inputs

      integer, intent(in)                 :: iopt,npru,iun22,iun33,
     &                                       iun66,nmeta

      CHARACTER(len=30)                   :: AA(6)
      integer, dimension(:), allocatable  :: jpdvec,jmdvec,kpdvec,kmdvec
      integer, dimension(:,:), allocatable :: noff
      integer, dimension(5)               :: ivec
      integer, dimension(6)               :: np,np1,np2,np3,np4,mp
      integer, dimension(0:8)             :: nggp
      integer, dimension(0:40)            :: nn40
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if(npru.gt.0)write(iun66,*)'*** Pedigrees have been "pruned" !'

c     try to find grand-parental ids
      allocate(jpdvec(nanim2),jmdvec(nanim2),kpdvec(nanim2),
     &         kmdvec(nanim2),noff(nanim2,6),nown(nanim2),stat=ii)
      if(ii>0)stop 'alloc grandparents'

      jpdvec=0
      jmdvec=0
      kpdvec=0
      kmdvec=0

      do ii=1,nanim2
      ian=nn(ii)
      if(ian.eq.0.and.npru>0)cycle
      jan=idvec(ii)
      idad=idsire(ian)
      if(idad.gt.0)then
         jpdvec(ian)=idsire(idad)
         jmdvec(ian)=iddam(idad)
         if(ian.eq.idad)write(*,*)'animal & sire ID identical',ian,jan
         if(ian.eq.idsire(idad))write(*,*)'animal & paternal grandsire',
     &                                  ' ID identical', ian,jan
         if(ian.eq.iddam(idad))write(*,*)'animal & paternal granddam',
     &                                  ' ID identical', ian,jan
      end if
      imum=iddam(ian)
      if(imum.gt.0)then
         kpdvec(ian)=idsire(imum)
         kmdvec(ian)=iddam(imum)
         if(ian.eq.imum)print *,'animal & dam ID identical',ian
         if(ian.eq.idsire(imum))write(*,*)'animal & maternal grandsire',
     &                                  ' ID identical', ian,jan
         if(ian.eq.iddam(imum))write(*,*)'animal & maternal granddam',
     &                                  ' ID identical', ian,jan
      end if
      end do

c     count no. of known grand-parents
      ngsp=0
      ngdp=0
      ngsm=0
      ngdm=0
      nggp=0

      do i=1,nanim2
      kk=0
      ian=nn(i)
      if(ian.eq.0)cycle
      ii=jpdvec(ian)
      if(ii.gt.0)then
          ngsp=ngsp+1
          if(idsire(ii).gt.0)kk=kk+1
          if(iddam(ii).gt.0)kk=kk+1
      end if
      ii=jmdvec(ian)
      if(ii.gt.0)then
          ngdp=ngdp+1
          if(idsire(ii).gt.0)kk=kk+1
          if(iddam(ii).gt.0)kk=kk+1
      end if
      ii=kpdvec(ian)
      if(ii.gt.0)then
          ngsm=ngsm+1
          if(idsire(ii).gt.0)kk=kk+1
          if(iddam(ii).gt.0)kk=kk+1
      end if
      ii=kmdvec(ian)
      if(ii.gt.0)then
          ngdm=ngdm+1
          if(idsire(ii).gt.0)kk=kk+1
          if(iddam(ii).gt.0)kk=kk+1
      end if
      nggp(kk)=nggp(kk)+1
      end do

      mm=sum( nggp(1:) )
      if(mm.gt.0)write(iun66,111)
     *           'no. of animals with great-grand parent(s)',mm

c     count no. of progeny (pedigree file)
      rewind(iun33)
      noff=0
      iend=0
350   call dfrd33(iun33,iofflp,ivec,1,iend,infmtp)
      if(iend.eq.99)go to 399
      call lnkfnd(ivec(1),ii)
      if (ii.eq.0)write(iun66,*)'err',ii,ivec(1)
      ian=nn(ii)
      if(ian.eq.0)go to 350                 ! this animal has been pruned
      idad=idsire(ian)
      if(idad.gt.0)noff(idad,1)=noff(idad,1)+1
      imum=iddam(ian)
      if(imum.gt.0)noff(imum,2)=noff(imum,2)+1
      ipgdad=jpdvec(ian)
      if(ipgdad.gt.0)noff(ipgdad,3)=noff(ipgdad,3)+1
      ipgmum=jmdvec(ian)
      if(ipgmum.gt.0)noff(ipgmum,4)=noff(ipgmum,4)+1
      imgdad=kpdvec(ian)
      if(imgdad.gt.0)noff(imgdad,5)=noff(imgdad,5)+1
      imgmum=kmdvec(ian)
      if(imgmum.gt.0)noff(imgmum,6)=noff(imgmum,6)+1
      go to 350

 399  do j=1,6
      mp(j)=count (mask=(noff(:,j)>0) )
      end do
      mss=count(mask=(noff(:,1)>0 .and. noff(:,3)+noff(:,5)>0) )
      mdd=count(mask=(noff(:,2)>0 .and. noff(:,4)+noff(:,6)>0) )
     
c     count no. of records & offspring (data file)
      ns0=0
      nd0=0
      nown=0
      noff=0
      m=1+nmeta

      rewind(iun22)
250   read(iun22,end=299)(i,k=1,m),ii
      nown(ii)=nown(ii)+1
      idad=idsire(ii)
      if(idad.gt.0)noff(idad,1)=noff(idad,1)+1
      if(idad.eq.0)ns0=ns0+1
      imum=iddam(ii)
      if(imum.gt.0)noff(imum,2)=noff(imum,2)+1
      if(imum.eq.0)nd0=nd0+1
      ipgdad=jpdvec(ii)
      if(ipgdad.gt.0)noff(ipgdad,3)=noff(ipgdad,3)+1
      ipgmum=jmdvec(ii)
      if(ipgmum.gt.0)noff(ipgmum,4)=noff(ipgmum,4)+1
      imgdad=kpdvec(ii)
      if(imgdad.gt.0)noff(imgdad,5)=noff(imgdad,5)+1
      imgmum=kmdvec(ii)
      if(imgmum.gt.0)noff(imgmum,6)=noff(imgmum,6)+1
      go to 250

 299  deallocate(jpdvec,jmdvec,kpdvec,kmdvec,stat=ii)
      if(ii>0)stop 'dealloc gp'
      do j=1,6
      np(j)=count(mask=(noff(:,j)>0))
      np1(j)=count(mask=(noff(:,j)>0.and.nown(:)>0))
      end do
      np2=0
      np3=0
      np4=0

      nss=count(mask=(noff(:,1)>0.and.(noff(:,3)+noff(:,5))>0))
      ndd=count(mask=(noff(:,2)>0.and.(noff(:,4)+noff(:,6))>0))
      nss1=count(mask=(noff(:,1)>0.and.(noff(:,3)+noff(:,5))>0.
     &                                              and.nown>0))
      ndd1=count(mask=(noff(:,2)>0.and.(noff(:,4)+noff(:,6))>0.
     &                                              and.nown>0))
      nbase=count(mask=(idsire.eq.0.and.iddam.eq.0))
      nbase1=count(mask=(idsire.eq.0.and.iddam.eq.0.and.nown>0))
      ns00=count(mask=(idsire.eq.0.and.nown>0))
      nd00=count(mask=(iddam.eq.0.and.nown>0))
      do i=1,nanim2
      do j=1,6
      if(noff(i,j)<1)cycle
         np4(j)=np4(j)+noff(i,j)
         if(nown(i).gt.0)np2(j)=np2(j)+noff(i,j)
         np3(j)=np3(j)+noff(i,j)*(noff(i,j)-1)
      end do
      end do

      write(iun66,111)'no. of  "base" animals',nbase
      write(*,111)'no. of animals in data w. unkn./pruned sire'
     *,           ns00
      write(*,111)'no. of animals in data w. unkn./pruned dam'
     *,           nd00
      write(*,111)'no. of rec.s w. missing/pruned sire ID',ns0
      write(*,111)'no. of rec.s w. missing/pruned dam ID',nd0
      write(iun66,111)'no. of animals in the data',count(nown>0)
      write(iun66,111)' ... which are "base" animals',nbase1
      write(iun66,111)' ... with unknown/pruned sire ',ns00
      write(iun66,111)' ... with unknown/pruned dam  ',nd00
      if(ns0.ne.ns00 .or. nd0.ne.nd00)then
         write(iun66,*)'no. of records'
         write(iun66,111)' ... with unknown/pruned sire ',ns0
         write(iun66,111)' ... with unknown/pruned dam  ',nd0
      end if
      write(*,111)'no. of sires  ... in total',mp(1)
      write(*,111)'              ... with progeny in the data',np(1)
      if(np(1)>0)then
         write(iun66,*)'no. of sires :'
         write(iun66,111)' ... in total',mp(1)
         write(iun66,111)' ... with progeny in the data',np(1)
         write(iun66,111)' ... with own record as well ',np1(1)
         write(iun66,111)' ... sire-offspring record pairs',np2(1)
         write(iun66,111)' ... which are also grand sires',mss
         write(iun66,111)' ... which also have grandprogeny records',nss
         write(iun66,111)' ... with own record as well ',nss1
         write(iun66,111)' ... paternal half-sib record pairs',np3(1)
         xx=np4(1)
         xx=xx/np(1)
         write(iun66,114)' ... av. no. progeny rec.s/sire',xx
      end if
      write(*,111)'no. of dams   ... in total',mp(2)
      write(*,111)'              ... with progeny in the data',np(2)
      write(iun66,*)'no. of dams :'
      write(iun66,111)' ... in total',mp(2)
      if(np(2).gt.0)then
         write(iun66,111)' ... with progeny in the data',np(2)
         write(iun66,111)' ... with own record as well ',np1(2)
         write(iun66,111)' ... dam-offspring record pairs',np2(2)
         write(iun66,111)' ... which are also grand dams',mdd
         write(iun66,111)' ... which also have grandprogeny records',ndd
         write(iun66,111)' ... with own record as well ',ndd1
         write(iun66,111)' ... maternal half-sib record pairs',np3(2)
         xx=np4(2)
         xx=xx/np(2)
         write(iun66,114)' ... av. no. progeny rec.s/dam',xx
      end if
      write(iun66,*)'no. of paternal grand sires :'
      write(iun66,111)' ... in total',mp(3)
      if(np(3).gt.0)then
         write(iun66,111)' ... with progeny in the data',np(3)
         write(iun66,111)' ... with own record as well ',np1(3)
         write(iun66,111)' ... grand sire-offspring record pairs',np2(3)
         write(iun66,111)' ... quarter-sib record pairs',np3(3)
         xx=np4(3)
         xx=xx/np(3)
         write(iun66,114)' ... av. no. progeny rec.s/grand sire',xx
      end if
      write(iun66,*)'no. of maternal grand sires :'
      write(iun66,111)' ... in total',mp(5)
      if(np(5).gt.0)then
         write(iun66,111)' ... with progeny in the data',np(5)
         write(iun66,111)' ... with own record as well ',np1(5)
         write(iun66,111)' ... grand sire-offspring record pairs',np2(5)
         write(iun66,111)' ... quarter-sib record pairs',np3(5)
         xx=np4(5)
         xx=xx/np(5)
         write(iun66,114)' ... av. no. progeny rec.s/grand sire',xx
      end if
      write(iun66,*)'no. of grand sires together :'
      write(iun66,111)' ... in total',mp(3)+mp(5)
      if(np(3)+np(5).gt.0)then
         write(iun66,111)' ... with progeny in the data',np(3)+np(5)
         write(iun66,111)' ... with own record as well ',np1(3)+np1(5)
         write(iun66,111)' ... grand sire-offspring record pairs',
     *                                               np2(3)+np2(5)
         write(iun66,111)' ... quarter-sib record pairs',np3(3)+np3(5)
         xx=np4(3)+np4(5)
         xx=xx/(np(3)+np(5))
         write(iun66,114)' ... av. no. progeny rec.s/grand sire',xx
      end if
      write(iun66,*)'no. of paternal grand dams :'
      write(iun66,111)' ... in total',mp(4)
      if(np(4).gt.0)then
         write(iun66,111)' ... with progeny in the data',np(4)
         write(iun66,111)' ... with own record as well ',np1(4)
         write(iun66,111)' ... grand dam-offspring record pairs',np2(4)
         write(iun66,111)' ... quarter-sib record pairs',np3(4)
         xx=np4(4)
         xx=xx/np(4)
         write(iun66,114)' ... av. no. progeny rec.s/grand dam',xx
      end if
      write(iun66,*)'no. of maternal grand dams :'
      write(iun66,111)' ... in total',mp(6)
      if(np(6).gt.0)then
         write(iun66,111)' ... with progeny in the data',np(6)
         write(iun66,111)' ... with own record as well ',np1(6)
         write(iun66,111)' ... grand dam-offspring record pairs',np2(6)
         write(iun66,111)' ... quarter-sib record pairs',np3(6)
         xx=np4(6)
         xx=xx/np(6)
         write(iun66,114)' ... av. no. progeny rec.s/grand dam',xx
      end if
      write(iun66,*)'no. of grand dams together :'
      write(iun66,111)' ... in total',mp(4)+mp(6)
      if(np(4)+np(6).gt.0)then
         write(iun66,111)' ... with progeny in the data',np(4)+np(6)
         write(iun66,111)' ... with own record as well ',np1(4)+np1(6)
         write(iun66,111)' ... grand dam-offspring record pairs',
     *                                            np2(4)+np2(6)
         write(iun66,111)' ... quarter-sib record pairs',np3(4)+np3(6)
         xx=np4(4)+np4(6)
         xx=xx/(np(4)+np(6))
         write(iun66,114)' ... av. no. progeny rec.s/grand dam',xx
      end if
      aa(1)='sires'
      aa(2)='dams'
      aa(3)='paternal grand sires'
      aa(4)='maternal grand sires'
      aa(5)='paternal grand dams'
      aa(6)='maternal grand dams'
      do kk=1,6
      nn40=0
      do i=1,nanim
      n=noff(i,kk)
      if(n.lt.20)then
        continue
      else if(n.le.100)then
        n=20+(n/10)
      else if(n.le.1000)then
        n=30+(n/100)
      else 
        n=40
      end if
      nn40(n)=nn40(n)+1
      end do
      write(iun66,*)' '
      write(iun66,*)'distribution over no.s of progeny for : ',aa(kk)
      write(iun66,112)'nprog',(i,i=0,9)
      write(iun66,112)'nanim',nn40(0:9)
      write(iun66,*)' '
      write(iun66,112)'nprog',(i,i=10,19)
      write(iun66,112)'nanim',nn40(10:19)
      write(iun66,*)' '
      do ii=22,29
      if(nn40(ii).gt.0)then
         write(iun66,113)'nprog','   20-29','   30-39','   40-49',
     *     '   50-59','   60-69','   70-79','   80-89','   90-99'
         write(iun66,112)'nanim',nn40(22:29)
         write(iun66,*)' '
         exit
      end if
      end do
      do ii=31,39
      if(nn40(ii).gt.0)then
         write(iun66,113)'nprog','  100-99','  200-99','  300-99',
     *                           '  400-99','  500-99','  600-99',
     *                           '  700-99','  800-99','  900-99'
         write(iun66,112)'nanim',nn40(31:39)
         write(iun66,*)' '
         exit
      end if
      end do
      if(nn40(40).gt.0)then
         write(iun66,113)'nprog','  1000+'
         write(iun66,112)'nanim',nn40(40)
      end if
      end do
      nnped(1)=nbase
      nnped(2)=count(nown>0)
      nnped(3)=ns00
      nnped(4)=nd00
      nnped(5)=np(1)
      nnped(6)=np(2)
      nnped(7)=np(3)+np(5)
      nnped(8)=np(4)+np(6)
 111  format(1x,a,t45,' =',i12)
 112  format(1x,a,10i8)
 113  format(1x,a,10a8)
 114  format(1x,a,t45,' =',f12.2)
      deallocate(noff)
      return
      end subroutine dfpcnt












