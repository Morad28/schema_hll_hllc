module mod_update
    Use mod_flux
    use mod_constante
    use mod_conservatif_primitif
    use mod_energie
    use mod_celerite
    use mod_CI
    use mod_ordreSup
    implicit none
    contains

    subroutine Update(flux)
        implicit None 
        character*4, intent(in) :: flux
        integer :: i, j, k, Bool = 0
        real ::  dt, tn=0, e, eR, Smax
        real, Dimension(1:4) :: var, primR, prim
        real, dimension(1:N*N-N):: flux1, flux2, flux3, flux4, fluy1, fluy2, fluy3, fluy4
        Loop : do while (tn<tout)
  
        Smax = 0
        do j = 1, N-1
         Do i = 1, N-1
          k = i+(j-1)*N
          ! ==================================
          ! On recupere rho, u et p (variables primitives)  
          ! ==== Axe x
  
          ! === Ordre 1
  
          prim = primitive(u1i(k),u2i(k),u3i(k), u4i(k))
          primR = primitive(u1i(k+1),u2i(k+1),u3i(k+1), u4i(k+1))
          e = energie(prim(1),prim(4))
          eR = energie(primR(1),primR(4))
          
          ! == Calcul du flux
          ! j+1/2
          ! Estimation de SL et SR sur cette interface
          if (flux == 'HLL') then
            call celerite_iso(prim(1), prim(2),prim(4),primR(1), primR(2), primR(4),SL,SR)
            var = flux_HLLx(prim(1), prim(2),prim(3), e, prim(4), primR(1), primR(2), primR(3), eR, primR(4), SL, SR)
          end if 
          if (flux == 'HLLC') then  
            call celerite_hyb(prim(1), prim(2),prim(4),primR(1), primR(2), primR(4),SL,SR)
            var = flux_hllc_x(prim(1), prim(2),prim(3), e, prim(4), primR(1), primR(2), primR(3), eR, primR(4), SL, SR)
          end if

          flux1(k) = var(1)
          flux2(k) = var(2)
          flux3(k) = var(3)
          flux4(k) = var(4)
          Smax = max(Smax, abs(SL), abs(SR))
  
          ! ==================================
  
          ! ==================================
          ! On recupere rho, u et p (variables primitives)  
          ! ==== Axe y
          ! === Ordre 1
          prim = primitive(u1i(k),u2i(k),u3i(k), u4i(k))
          e = energie(prim(1),prim(4))
          primR = primitive(u1i(k+N),u2i(k+N),u3i(k+N),u4i(k+N))
          eR = energie(primR(1),primR(4))
  
          ! j+1/2
          ! Estimation de SL et SR sur cette interface
          if (flux == 'HLL') then
            call celerite_iso(prim(1), prim(3),prim(4),primR(1), primR(3), primR(4),SL,SR)
            var = flux_HLLy(prim(1), prim(2),prim(3), e, prim(4), primR(1), primR(2), primR(3), eR, primR(4), SL, SR)
          end if
          if (flux == 'HLLC') then
            call celerite_hyb(prim(1), prim(3),prim(4),primR(1), primR(3), primR(4),SL,SR)
            var = flux_hllc_y(prim(1), prim(2),prim(3), e, prim(4), primR(1), primR(2), primR(3), eR, primR(4), SL, SR)
          end if

          fluy1(k) = var(1)
          fluy2(k) = var(2)
          fluy3(k) = var(3)
          fluy4(k) = var(4)
          Smax = max(Smax, abs(SL), abs(SR))
          ! ==================================
          ! Verification positivite de la pression
          if (prim(4)<0) then
            print*, "ERREUR PRESSION NEGATIVE", k, tn,  prim
            Bool=1
            EXIT
          end if
        
      end do
      end do
      ! Mis a jour du pas 
      dt = CFL*dx/(Smax)
      tn = tn + dt
      print* ,"ordre 1", tn, "pas =", dt,"CFL =" ,(dt/dx)*Smax
      do j = 2, N-1
        do i = 2, N-1
        
          k = i+(j-1)*N
          ! Mis a jour de la solution
          u1i(k) = u1i(k) - (dt/dx)*(flux1(k) - flux1(k-1) + fluy1(k)-fluy1(k-N))
          u2i(k) = u2i(k) - (dt/dx)*(flux2(k) - flux2(k-1) + fluy2(k)-fluy2(k-N))
          u3i(k) = u3i(k) - (dt/dx)*(flux3(k) - flux3(k-1) + fluy3(k)-fluy3(k-N))
          u4i(k) = u4i(k) - (dt/dx)*(flux4(k) - flux4(k-1) + fluy4(k)-fluy4(k-N))
        
        end do
      end do
      do j = 1, N
        do i = 1, N
        k = i+(j-1)*N
        if ( i == 1 ) then
          u1i(k) = u1i(k+1)
          u2i(k) = u2i(k+1) 
          u3i(k) = u3i(k+1) 
          u4i(k) = u4i(k+1) 
        endif
        if ( i == N ) then
          u1i(k) = u1i(k-1)
          u2i(k) = u2i(k-1) 
          u3i(k) = u3i(k-1) 
          u4i(k) = u4i(k-1) 
        endif
        if ( j == 1 ) then
          u1i(k) = u1i(k+N)
          u2i(k) = u2i(k+N) 
          u3i(k) = u3i(k+N) 
          u4i(k) = u4i(k+N) 
        endif
        if ( j == N ) then
          u1i(k) = u1i(k-N)
          u2i(k) = u2i(k-N) 
          u3i(k) = u3i(k-N) 
          u4i(k) = u4i(k-N) 
        endif
      end do
      end do 
  
      if (Bool==1) EXIT
    
    enddo LOOP
  
      end subroutine Update

      subroutine Update_2
        implicit None
        Integer :: i, j, k , Bool = 0
        real ::  dt, tn=0, e, eR, Smax
        real, dimension(1:4) :: UL, UR
        real, Dimension(1:4) :: var, primR, prim
        real, dimension(1:N*N-N):: flux1, flux2, flux3, flux4,fluy1, fluy2, fluy3, fluy4
        Loop : do while (tn<tout)
        Smax = 0
  
        do j = 2, N-2
          do i = 2, N-2
           k = i+(j-1)*N
          ! ==================================
          ! ==== Axe x
  
          UL(1) = reconstructionL(u1i(k-1),u1i(k),u1i(k+1))
          UL(2) = reconstructionL(u2i(k-1),u2i(k),u2i(k+1))
          UL(3) = reconstructionL(u3i(k-1),u3i(k),u3i(k+1))
          UL(4) = reconstructionL(u4i(k-1),u4i(k),u4i(k+1))
        
          UR(1) = reconstructionR(u1i(k),u1i(k+1),u1i(k+2))
          UR(2) = reconstructionR(u2i(k),u2i(k+1),u2i(k+2))
          UR(3) = reconstructionR(u3i(k),u3i(k+1),u3i(k+2))
          UR(4) = reconstructionR(u4i(k),u4i(k+1),u4i(k+2))
        
          prim = primitive(UL(1),UL(2),UL(3),UL(4))
          primR = primitive(UR(1),UR(2),UR(3),UR(4))
          e = energie(prim(1),prim(4))
          eR = energie(primR(1),primR(4))
  
          ! == Calcul du flux
          ! j+1/2
          ! Estimation de SL et SR sur cette interface
  
          ! call celerite_iso(prim(1), prim(2),prim(4),primR(1), primR(2), primR(4),SL,SR)
          ! var = flux_HLLx(prim(1), prim(2),prim(3), e, prim(4), primR(1), primR(2), primR(3), eR, primR(4), SL, SR)
          
          call celerite_hyb(prim(1), prim(2),prim(4),primR(1), primR(2), primR(4),SL,SR)
          var = flux_hllc_x(prim(1), prim(2),prim(3), e, prim(4), primR(1), primR(2), primR(3), eR, primR(4), SL, SR) 
  
          flux1(k) = var(1)
          flux2(k) = var(2)
          flux3(k) = var(3)
          flux4(k) = var(4)
          Smax = max(Smax, abs(SL), abs(SR))
  
          ! ==================================
  
          ! ==================================
          ! ==== Axe y
          UL(1) = reconstructionL(u1i(k-N),u1i(k),u1i(k+N))
          UL(2) = reconstructionL(u2i(k-N),u2i(k),u2i(k+N))
          UL(3) = reconstructionL(u3i(k-N),u3i(k),u3i(k+N))
          UL(4) = reconstructionL(u4i(k-N),u4i(k),u4i(k+N))
        
          UR(1) = reconstructionR(u1i(k),u1i(k+N),u1i(k+2*N))
          UR(2) = reconstructionR(u2i(k),u2i(k+N),u2i(k+2*N))
          UR(3) = reconstructionR(u3i(k),u3i(k+N),u3i(k+2*N))
          UR(4) = reconstructionR(u4i(k),u4i(k+N),u4i(k+2*N))
        
          prim = primitive(UL(1),UL(2),UL(3),UL(4))
          primR = primitive(UR(1),UR(2),UR(3),UR(4))
          e = energie(prim(1),prim(4))
          eR = energie(primR(1),primR(4))
  
          ! j+1/2
          ! Estimation de SL et SR sur cette interface
          ! call celerite_iso(prim(1), prim(3),prim(4),primR(1), primR(3), primR(4),SL,SR)
          ! var = flux_HLLy(prim(1), prim(2),prim(3), e, prim(4), primR(1), primR(2), primR(3), eR, primR(4), SL, SR)
         
          call celerite_hyb(prim(1), prim(3),prim(4),primR(1), primR(3), primR(4),SL,SR)
          var = flux_hllc_y(prim(1), prim(2),prim(3), e, prim(4), primR(1), primR(2), primR(3), eR, primR(4), SL, SR)
            
          fluy1(k) = var(1)
          fluy2(k) = var(2)
          fluy3(k) = var(3)
          fluy4(k) = var(4)
          Smax = max(Smax, abs(SL), abs(SR))
  
          ! prim = primitive(u1i(k),u2i(k),u3i(k), u4i(k))
          ! primR = primitive(u1i(k+1),u2i(k+1),u3i(k+1), u4i(k+1))
          ! call celerite_hyb(prim(1), prim(2),prim(4),primR(1), primR(2), primR(4),SL,SR)
          ! Smax = max(Smax, abs(SL), abs(SR))
          ! primR = primitive(u1i(k+N),u2i(k+N),u3i(k+N),u4i(k+N))
          ! call celerite_hyb(prim(1), prim(3),prim(4),primR(1), primR(3), primR(4),SL,SR)
          ! Smax = max(Smax, abs(SL), abs(SR))
  
          ! ==================================
          ! Verification positivite de la pression
          if (prim(4)<0) then
            print*, "ERREUR PRESSION NEGATIVE", k, tn,  prim
            Bool=1
            EXIT
          end if
        
      end do
      end do
  
      ! === Mis a jour du pas ===
      dt = CFL*dx/(Smax)
      tn = tn + dt
      print* ,"ordre 2", tn, "pas =", dt,"CFL =" ,(dt/dx)*Smax
  
      do j = 3, N-2
        do i = 3, N-2
          k = i+(j-1)*N
          ! Mis a jour de la solution
          u1i(k) = u1i(k) - (dt/dx)*(flux1(k) - flux1(k-1) + fluy1(k)-fluy1(k-N))
          u2i(k) = u2i(k) - (dt/dx)*(flux2(k) - flux2(k-1) + fluy2(k)-fluy2(k-N))
          u3i(k) = u3i(k) - (dt/dx)*(flux3(k) - flux3(k-1) + fluy3(k)-fluy3(k-N))
          u4i(k) = u4i(k) - (dt/dx)*(flux4(k) - flux4(k-1) + fluy4(k)-fluy4(k-N))
        end do
      end do
      U_1 = u1i
      U_2 = u2i
      U_3 = u3i
      U_4 = u4i
      do j = 1, N
        do i = 1, N
        k = i+(j-1)*N
     
        if ( i == 2 ) then
          u1i(k) = U_1(k+1)
          u2i(k) = U_2(k+1) 
          u3i(k) = U_3(k+1) 
          u4i(k) = u4i(k+1) 
          u1i(k-1) = U_1(k+1)
          u2i(k-1) = U_2(k+1) 
          u3i(k-1) = U_3(k+1) 
          u4i(k-1) = U_4(k+1) 
        endif
        if ( i == N-1 ) then
          u1i(k) = U_1(k-1)
          u2i(k) = U_2(k-1) 
          u3i(k) = U_3(k-1) 
          u4i(k) = u4i(k-1) 
          u1i(k+1) = U_1(k-1)
          u2i(k+1) = U_2(k-1) 
          u3i(k+1) = U_3(k-1) 
          u4i(k+1) = U_4(k-1) 
        endif
        if ( j == 2 ) then
          u1i(k) = U_1(k+N)
          u2i(k) = U_2(k+N) 
          u3i(k) = U_3(k+N) 
          u4i(k) = U_4(k+N) 
          u1i(k-N) = U_1(k+N)
          u2i(k-N) = U_2(k+N) 
          u3i(k-N) = U_3(k+N) 
          u4i(k-N) = U_4(k+N) 
        endif
        if ( j == N-1 ) then
          u1i(k) = U_1(k-N)
          u2i(k) = U_2(k-N) 
          u3i(k) = U_3(k-N) 
          u4i(k) = U_4(k-N) 
          u1i(k+N) = U_1(k-N)
          u2i(k+N) = U_2(k-N) 
          u3i(k+N) = U_3(k-N) 
          u4i(k+N) = U_4(k-N) 
        endif
  
       
      end do
      end do    
      if (Bool==1) EXIT
    
    enddo LOOP
  
  
      end subroutine Update_2

end module mod_update