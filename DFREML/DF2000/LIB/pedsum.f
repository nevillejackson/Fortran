C============================================================================
      SUBROUTINE      PEDSUM (iun11,iun66)
C============================================================================

      integer, intent(in)   :: iun11,iun66
      integer, dimension(8) :: nnped
      character(len=25)     :: fped

      REWIND(IUN11)
      read(iun11)fped
      READ(IUN11)NNPED

      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'-----------------------------'
      WRITE(IUN66,*)'SUMMARY OF PEDIGREE STRUCTURE'
      WRITE(IUN66,*)'-----------------------------'
      WRITE(IUN66,*)' '
      WRITE(IUN66,90)'NO. OF "BASE" ANIMALS',NNPED(1)
      WRITE(IUN66,90)'NO. OF ANIMALS WITH RECORDS',NNPED(2)
      WRITE(IUN66,90)' ... WITH UNKNOWN/PRUNED SIRE ',NNPED(3)
      WRITE(IUN66,90)' ... WITH UNKNOWN/PRUNED DAM  ',NNPED(4)
      WRITE(IUN66,90)'NO. OF SIRES WITH PROGENY RECORDS',NNPED(5)
      WRITE(IUN66,90)'NO. OF DAMS WITH PROGENY RECORDS',NNPED(6)
      WRITE(IUN66,90)'NO. OF GRAND-SIRES W. PROGENY RECORDS',NNPED(7)
      WRITE(IUN66,90)'NO. OF GRAND-DAMS W. PROGENY RECORDS',NNPED(8)
      RETURN
 90   FORMAT(1X,A,T45,' =',I12)
      END subroutine pedsum















