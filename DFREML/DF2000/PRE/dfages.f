C==========================================================================
      subroutine dfages(iq,iun11,iun66)
C==========================================================================

c     purpose : collect ages in the data (for covariance function analysis)
c-----------------------------------------------------------km--8/95------

      use parameters
      use c_numbers
      use ages

      integer, intent(in)  :: iq,iun11,iun66

      do imeta=1,nmeta

      iage=jvec(kint(iq)+imeta)
      do i=1,nage(imeta)
      if(iiage(i,imeta).eq.iage)then
c        code already exists
         return
      else if(iiage(i,imeta).gt.iage)then
c        new age found, slot into right place
         nage(imeta)=nage(imeta)+1
         if(nage(imeta).gt.maxage)go to 999
         do k=nage(imeta),i+1,-1
         iiage(k,imeta)=iiage(k-1,imeta)
         end do
         iiage(i,imeta)=iage
         go to 100
      end if
      end do
c     .... new age is highest so far
      nage(imeta)=nage(imeta)+1
      if(nage(imeta).gt.maxage)go to 999
      iiage(nage(imeta),imeta)=iage
 100  continue
      end do ! imeta
      return 
 999  write(*,*)'too many ages in the data ...'
      stop 'reset parameter "MAXAGE" '

C==========================================================================
      entry dfagec(iq,iun11,iun66)
C==========================================================================

      do im=1,nmeta

      iage=jvec(kint(iq)+im)
      K1=1                                                              
      K2=nage(im)+1
24    K=K1+(K2-K1)/2                                                    
      IF(iage-iiage(k,im))21,23,22
21    K2=K                     
      GO TO 24
22    IF(K1.EQ.K)THEN
         PRINT *,' "DFAGEC" search failed ?'
         PRINT *,'IQ=',IQ,'   NLEV=',Nage
         PRINT *,'CODE =',iage
         PRINT *,'K & K1',K,Iiage(k,im)
         PRINT *,'K2   ',K2,iiage(k2,im)
         STOP 'SOMETHING IS WRONG SOMEWHERE ...!!!'
      END IF
      K1=K                                                              
      GO TO 24                                                           
23    jage=k
      nnage(jage,iq,im)=nnage(jage,iq,im)+1
      xxage(jage,iq,im)=xxage(jage,iq,im)+xvec(kcov(iq)+1)
      iavec(im)=jage

      end do ! im
      return

C==========================================================================
      entry dfagew(iq,iun11,iun66)
C==========================================================================

      write(iun11)nmeta
      write(iun11)nage
      mage=maxval(nage)
      write(iun11)iiage(:mage,:nmeta)
      write(iun11)nnage(:mage,:nq,:nmeta)
      

      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'------------------------'
      WRITE(IUN66,*)'Ages found in the data  '
      WRITE(IUN66,*)'------------------------'
      WRITE(IUN66,*)' '
      open(20,file='DF20#DAT',status='unknown')
      where(nnage>0)xxage=xxage/nnage
      do im=1,nmeta
      write(iun66,'(1x,a,i4)')'"meta-meter" no.',im
      do i=1,nage(im)
      write(iun66,600)i,iiage(i,im),(nnage(i,jq,im),xxage(i,jq,im),
     &                                                     jq=1,nq)
      write(20,*)i,iiage(i,im),im
      end do
      end do ! im
 600  format(i4,i8,(t15,4(i8,g13.5)))

      return
      end subroutine dfages










