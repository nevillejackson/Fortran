      subroutine detlit (lit,litf,cpf,same)
c------determines the content of the data field "litf" from
c------input record "lit" in the case where prefix printed only
c------if it changes
      character lit*1000,litf*130,cpf*20
      logical same
      common /stdio/ lin,lout,lmess
      common /ctab/ ibegd,iendd,ilen,ibegp,ilenp,iendp,ibeg2
      save
      if (cpf.ne.lit(ibegp:iendp)) then
c------prefix has changed, whole field printed, "cpf" updated
      litf = lit(ibegd:iendd)
      cpf = lit(ibegp:iendp)
      else if (same) then
c------prefix at beginning of data field .. unchanged
      j = ilenp+1
      litf(j:ilen) = lit(ibeg2:iendd)
      else
c------prefix not at beginning of data field .. unchanged
      j = ibegp-ibegd
      jj = ibegp-1
      litf(1:j) = lit(ibegd:jj)
      k = j+ilenp+1
      litf(k:ilen) = lit(ibeg2:iendd)
      end if
      return
      end
