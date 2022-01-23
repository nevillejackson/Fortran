c=============================================================================
      SUBROUTINE  GSFCT ( NEQNS, XLNZ, LNZ, XNZSUB, NZSUB, DIAG, 
     *                    LINK, FIRST, TEMP, IFLAG )             
c=============================================================================

      double precision  lnz(*)
      INTEGER  NZSUB(*),XLNZ(*), XNZSUB(*)
      integer, intent(in)                      :: neqns
      integer, intent(out)                     :: iflag
      integer, intent(inout), dimension(neqns) :: link,first
      real(8), intent(inout), dimension(neqns) :: diag, temp
      real(8)                                  :: diagj, ljk          
      integer :: i,j,k,isub,newk,kfirst,istrt,istop,ii
      iflag=0
      link(:neqns)=0
      temp(:neqns)=0.d0

      DO  J = 1, NEQNS
      diagj = 0.0d0  
      newK  = LINK(J)    
      k=newk
      do while (k.ne.0)
          newk=link(k)
          KFIRST = FIRST(K)   
          LJK    = LNZ(KFIRST)
          DIAGJ = DIAGJ + LJK*LJK  
          ISTRT = KFIRST + 1  
          ISTOP = XLNZ(K+1) - 1 
          IF ( ISTOP .ge. ISTRT )then
              FIRST(K) = ISTRT
              I = XNZSUB(K) + (KFIRST-XLNZ(K)) + 1
              ISUB = NZSUB(I)                 
              LINK(K) = LINK(ISUB)            
              LINK(ISUB) = K                  
              DO  II = ISTRT, ISTOP  
              ISUB = NZSUB(I)              
              TEMP(ISUB) = TEMP(ISUB) + LNZ(II)*LJK 
              I = I + 1 
              end do
          end if
          k= newk
          end  do

          DIAGJ = DIAG(J) - DIAGJ
          if ( diagj .le. 0.d0 )then
              IFLAG = 1
              print *,'row',j,diagj,diag(j)
              RETURN   
          end if
          diagj = dsqrt(diagj)               
          DIAG(J) = DIAGJ 
          ISTRT = XLNZ(J)        
          ISTOP = XLNZ(J+1) - 1  
          IF ( ISTOP .ge. ISTRT )then
               FIRST(J) = ISTRT  
               I = XNZSUB(J)     
               ISUB = NZSUB(I)   
               LINK(J) = LINK(ISUB)
               LINK(ISUB) = J      
               DO II = ISTRT, ISTOP
                  ISUB = NZSUB(I)  
                  LNZ(II) = ( LNZ(II)-TEMP(ISUB) ) / DIAGJ  
                  temp(isub) = 0.0d0
                  I = I + 1                                 
               end do
          end if
      end do
      RETURN 
      END subroutine gsfct








