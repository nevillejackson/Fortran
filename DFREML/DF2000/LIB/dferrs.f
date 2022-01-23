C============================================================================
      SUBROUTINE DFERR0(MM,insert,ian)
C============================================================================

      write(*,*)'attempt to write out invalid code for 2nd animal',
     *                                                 ' effect !'
      write(*,*)'insert/recode option given  =',insert
      write(*,*)'code found                  =',mm
      write(*,*)'for animal no.              =',ian
      return
      end

C============================================================================
      SUBROUTINE DFERR1(MAXXHX,LIM1)
C============================================================================

      WRITE(*,*)'PROGRAM IS SET UP SO THAT EQUATIONS FOR ALL'
      WRITE(*,*)'REGRESSION COEFFICIENTS ARE "FULLSTORED"   '
      WRITE(*,*)'CURRENT LIMIT : "MAXXHX" = ',MAXXHX
      WRITE(*,*)'NO. REQUIRED  : "LIM1"   = ',LIM1
      STOP 'RESET PARAMETER "MAXXHX" !!'
      END

C=========================================================================     
      SUBROUTINE DFERR2(IOPMOD,JOPMOD)
C=========================================================================     

      CHARACTER PROG(3)*5
      DATA PROG /'DFUNI','DFMUV','DFMUW'/

      WRITE(*,*)'DFREML : PROGRAM CHOSEN & INPUT FILES INCOMPATIBLE !'
      WRITE(*,*)'         INPUT FILE (UNIT "51") FOUND FOR : "',
     *                                           PROG(IOPMOD),'"'
      WRITE(*,*)'         PROGRAM ATTEMPTED TO RUN IS      : "',
     *                                           PROG(JOPMOD),'" !"'
      STOP
      END

C============================================================================
      SUBROUTINE DFERR3(iorder,dford)
C============================================================================

      CHARACTER*6 DFORD

      WRITE(*,*)'ROUTINE "',DFORD,'" : INCONSISTENCY ENCOUNTERED '
      WRITE(*,*)'                    ORDERING OPTION FROM PREVIOUS RUN',
     *          ' IS',IORDER,' !'
      STOP      'RESTART WITH "SET-UP" STEP !!!'
      END

C============================================================================
      SUBROUTINE DFERR4 (ioprun,nparm,parvec)
C============================================================================

      double precision parvec(nparm)

      WRITE(*,*)'STARTING VALUES GIVEN ARE OUT OF BOUNDS OF THE',
     *                                 ' PARAMETER SPACE !!'
      print *,(parvec(l),l=1,nparm)
      if(ioprun.lt.0)then
         return
      else if(ioprun.eq.0)then
         STOP 'TRY A BIT HARDER NEXT TIME ...'
      else
         write(*,*)'do you want to modify vector of starting values ? '
         call yndef(inew,1)
         if(inew.eq.0)stop
 2       write(*,*)'give running no. of parameter to be changed',
     *                                           ' (0 for none)'
         call option(k,0,nparm)
         if(k.eq.0)return
         write(*,1)'"old" value for parameter no.',k,parvec(k)
         write(*,*)'give new value'
         read(*,*)parvec(k)
         go to 2
      END IF
 1    format(1x,a,i4,g16.6)
      end


C============================================================================
      SUBROUTINE DFERR5 (iq,jq,nrec,ianim)
C============================================================================

      WRITE(*,*)'SORTING ERROR IN DATA AT RECORD NO. :',NREC
      WRITE(*,*)'ANIMAL NO. IS                       :',IANIM
      WRITE(*,*)'PREVIOUS RECORD WAS FOR TRAIT NO.   :',JQ
      WRITE(*,*)'THIS RECORD IS FOR TRAIT NO.        :',IQ
      WRITE(*,*)'DATA *MUST* BE SORTED ACCORDING TO TRAITS WITHIN'
     *,         ' ANIMALS !!!' 
      STOP 'SORT DATA FILE AND TRY AGAIN ...'
      END

C============================================================================
      SUBROUTINE DFERR6 (nrec,ifix0,iq,l,ifixl,llev)
C============================================================================

      WRITE(*,*)'CODE OUT OF SPECIFIED RANGE ENCOUNTERED !'
      WRITE(*,*)'   RECORD NO.            =',NREC
      WRITE(*,*)'   ANIMAL NO.            =',IFIX0
      WRITE(*,*)'   TRAIT NO.             =',IQ
      WRITE(*,*)'   EFFECT NO.            =',L
      WRITE(*,*)'   CODE FOUND            =',IFIXL
      WRITE(*,*)'   NO. OF LEVELS GIVEN   =',LLEV
      STOP 'CHECK MODEL & TRY AGAIN !'
      END

C============================================================================
      SUBROUTINE DFERR7 (lq,mxobs,mark,janim)
C============================================================================

      WRITE(*,*)'FOUND MORE RECORDS PER TRAIT THAN ALLOWED !'
      WRITE(*,9)'TRAIT NO.                     =',LQ
      WRITE(*,9)'MAX. NO. OF RECORDS SPECIFIED =',MXOBS
      WRITE(*,9)'NO. OF RECORDS FOUND          =',MARK
      WRITE(*,9)'ANIMAL ID                     =',JANIM
      STOP 'MAKE SURE YOU KNOW YOUR BL... DATA !'
9     FORMAT(8X,A,I8)
      END
