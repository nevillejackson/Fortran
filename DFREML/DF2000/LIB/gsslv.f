!===========================================================================
      SUBROUTINE  GSSLV ( NEQNS, XLNZ, LNZ, XNZSUB, NZSUB,DIA, RHS ) 
!===========================================================================

      double precision DIA(*), LNZ(*), RHS(*), RHSJ, S       
      INTEGER NZSUB(*),XLNZ(*), XNZSUB(*)

!     FORWARD SUBSTITUTION ...                        
      DO J = 1, NEQNS
         RHSJ = RHS(J) / DIA(J)                      
         RHS(J) = RHSJ                                
         I = XNZSUB(J)                        
         DO II = XLNZ(J),XLNZ(J+1) - 1 
         ISUB = NZSUB(I)                      
         RHS(ISUB) = RHS(ISUB) - LNZ(II)*RHSJ 
         I = I + 1    
         end do
      end do

!     BACKWARD SUBSTITUTION ...                       
      DO J = NEQNS,1,-1
         S = RHS(J)         
         I = XNZSUB(J)      
         DO II = XLNZ(J),XLNZ(J+1) - 1  
         S = S - LNZ(II)*RHS( NZSUB(I) )  
         I = I + 1                  
         end do
         RHS(J) = S / DIA(J) 
      end do
      RETURN             
      END ! subroutine gsslv
