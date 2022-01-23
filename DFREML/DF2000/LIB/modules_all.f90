!      ===========================================
!      =    MODULES used by all DFREML programs  =
!      ===========================================

      module platform
!        purpose : insert extra line feed for prompts for interactive inputs

!----------------------Customize as required ------------------------------

!        1=unix, 2=lahey, set before compilation
         integer, save                  :: ipltf=1

!---------------------end of customizable section---------------------------

         character(len=7),dimension(2)  :: fmt =(/'(1x,a )','(1x,a/)' /)
         character(len=9),dimension(2)  :: fmt2=(/'(1x,a,a )','(1x,a,a/)' /)
         character(len=10),dimension(2) :: fmt3=(/'(1x,a,i2 )','(1x,a,i2/)'/)
         character(len=11),dimension(2) :: fmt4=(/'(1x,a,a,a )','(1x,a,a,a/)'/)
         character(len=13),dimension(2) :: fmt5=(/'(1x,a,g16.6 )', &
     &                                            '(1x,a,g16.6/)'/)
         character(len=14),dimension(2)  :: fmt6=(/'(1x,a,a,i3,a )', &
     &                                             '(1x,a,a,i3,a/)' /)
         character(len=7),dimension(2)  :: fmt9=(/'(7x,a )','(7x,a/)' /)
      end module platform


      MODULE version
         character(len=15)           :: modat='May 11, 2001 ', &
     &                                  versn='3.1.000'

      contains

!        ------------------------
         subroutine dxversion(iun)
!        ------------------------
         integer, intent(in)           :: iun

         if(iun>0)then
            write(iun,'(1x,t20,''*** DFREML '',a,''***'')')versn
            write(iun,'(1x,t20,''Last modified : '',a/)')modat
         else
            write(*,'(a,2x,a,2x,a,2x,a/)')'"DFREML" version',versn, &
     &                                     'last modified',modat
         end if
         return
         end subroutine dxversion

      END MODULE version

      MODULE today
         character(len=40), save  :: cwd
         character(len=30),save   :: machine='Host name not determined     '

      contains
!        -------------------------
         subroutine today_is (iun)
!        -------------------------
         integer, intent(in)         :: iun
         character(len=8)            :: tday
         character(len=10)           :: now

!        fortran 90 intrinsic routine (should work on all systems)
         call date_and_time(tday,now)

         if(iun.eq.0)then
           write(*,'(10a)')' Today is ',tday(7:8),'/',tday(5:6),'/', &
     &                tday(1:4),' -- Time is ',now(1:2),':',now(3:4)
         else
           write(iun,'(10a)')' Today is ',tday(7:8),'/',tday(5:6),'/', &
     &                tday(1:4),' -- Time is ',now(1:2),':',now(3:4)
         end if

!        write out host name
         call dfhost(iun)

      return
      end subroutine today_is

      END MODULE today











