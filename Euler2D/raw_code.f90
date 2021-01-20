! ======================== UPDATE_2 ======================== 
   
    subroutine Update_2
        implicit None 
        real, dimension(1:4) :: UN, US, UE, UW, UCentre,Ugauche,Udroite,Ubas,Uhaut
        real, Dimension(1:4) :: var, primR, prim
        real, dimension(1:N*N-N):: flux1, flux2, flux3, flux4, fluy1, fluy2, fluy3, fluy4
  
        Smax = 0
  
        do j = 2, N-2
          do i = 2, N-2
           k = i+(j-1)*N
          ! ==================================
          ! ==== Axe x
          Ucentre = Uk(u1i(k),u2i(k),u3i(k),u4i(k))
          Ugauche = Uk(u1i(k-1),u2i(k-1),u3i(k-1),u4i(k-1))
          Udroite = Uk(u1i(k+1),u2i(k+1),u3i(k+1),u4i(k+1))
          Ubas = Uk(u1i(k-N),u2i(k-N),u3i(k-N),u4i(k-N))
          Uhaut = Uk(u1i(k+N),u2i(k+N),u3i(k+N),u4i(k+N))
  
          UE = get_U((i+0.5)*dx , j*dx , i*dx , j*dx , UCentre , Ugauche , Udroite , Ubas , Uhaut )
  
          Ucentre = Uk(u1i(k+1),u2i(k+1),u3i(k+1),u4i(k+1))
          Ugauche = Uk(u1i(k),u2i(k),u3i(k),u4i(k))
          Udroite = Uk(u1i(k+2),u2i(k+2),u3i(k+2),u4i(k+2))
          Ubas = Uk(u1i(k+1-N),u2i(k+1-N),u3i(k+1-N),u4i(k+1-N))
          Uhaut = Uk(u1i(k+1+N),u2i(k+1+N),u3i(k+1+N),u4i(k+1+N))
  
          UW = get_U((i+0.5)*dx , j*dx , (i+1)*dx , j*dx , UCentre , Ugauche , Udroite , Ubas , Uhaut )
          
          prim = primitive(UE(1),UE(2),UE(3),UE(4))
          primR = primitive(UW(1),UW(2),UW(3),UW(4))
          e = energie(prim(1),prim(4))
          eR = energie(primR(1),primR(4))
  
          ! == Calcul du flux
          ! j+1/2
          ! Estimation de SL et SR sur cette interface
          call celerite_iso(prim(1), prim(2),prim(4),primR(1), primR(2), primR(4),SL,SR)
          var = flux_HLLx(prim(1), prim(2),prim(3), e, prim(4), primR(1), primR(2), primR(3), eR, primR(4), SL, SR)
          
          ! call celerite_hyb(prim(1), prim(2),prim(4),primR(1), primR(2), primR(4),SL,SR)
          ! var = flux_hllc_x(prim(1), prim(2),prim(3), e, prim(4), primR(1), primR(2), primR(3), eR, primR(4), SL, SR)
          
  
          flux1(k) = var(1)
          flux2(k) = var(2)
          flux3(k) = var(3)
          flux4(k) = var(4)
          Smax = max(Smax, abs(SL), abs(SR))
  
          ! ==================================
  
          ! ==================================
          ! ==== Axe y
  
          Ucentre = Uk(u1i(k),u2i(k),u3i(k),u4i(k))
          Ugauche = Uk(u1i(k-1),u2i(k-1),u3i(k-1),u4i(k-1))
          Udroite = Uk(u1i(k+1),u2i(k+1),u3i(k+1),u4i(k+1))
          Ubas = Uk(u1i(k-N),u2i(k-N),u3i(k-N),u4i(k-N))
          Uhaut = Uk(u1i(k+N),u2i(k+N),u3i(k+N),u4i(k+N))
  
          UN = get_U(i*dx , (j+0.5)*dx , i*dx , j*dx , UCentre , Ugauche , Udroite , Ubas , Uhaut )
  
          Ucentre = Uk(u1i(k+N),u2i(k+N),u3i(k+N),u4i(k+N))
          Ugauche = Uk(u1i(k-1+N),u2i(k-1+N),u3i(k-1+N),u4i(k-1+N))
          Udroite = Uk(u1i(k+1+N),u2i(k+1+N),u3i(k+1+N),u4i(k+1+N))
          Ubas = Uk(u1i(k),u2i(k),u3i(k),u4i(k))
          Uhaut = Uk(u1i(k+2*N),u2i(k+2*N),u3i(k+2*N),u4i(k+2*N))
  
          US = get_U(i*dx , (j+0.5)*dx , i*dx , (j+1)*dx , UCentre , Ugauche , Udroite , Ubas , Uhaut )
          
  
            prim = primitive(UN(1),UN(2),UN(3),UN(4))
            primR = primitive(US(1),US(2),US(3),US(4))
            e = energie(prim(1),prim(4))
            eR = energie(primR(1),primR(4))
  
          ! j+1/2
          ! Estimation de SL et SR sur cette interface
          call celerite_iso(prim(1), prim(3),prim(4),primR(1), primR(3), primR(4),SL,SR)
          var = flux_HLLy(prim(1), prim(2),prim(3), e, prim(4), primR(1), primR(2), primR(3), eR, primR(4), SL, SR)
         
          ! call celerite_hyb(prim(1), prim(3),prim(4),primR(1), primR(3), primR(4),SL,SR)
          ! var = flux_hllc_y(prim(1), prim(2),prim(3), e, prim(4), primR(1), primR(2), primR(3), eR, primR(4), SL, SR)
            
          fluy1(k) = var(1)
          fluy2(k) = var(2)
          fluy3(k) = var(3)
          fluy4(k) = var(4)
          Smax = max(Smax, abs(SL), abs(SR))
  
          prim = primitive(u1i(k),u2i(k),u3i(k), u4i(k))
          primR = primitive(u1i(k+1),u2i(k+1),u3i(k+1), u4i(k+1))
          call celerite_iso(prim(1), prim(2),prim(4),primR(1), primR(2), primR(4),SL,SR)
          Smax = max(Smax, abs(SL), abs(SR))
          primR = primitive(u1i(k+N),u2i(k+N),u3i(k+N),u4i(k+N))
          call celerite_iso(prim(1), prim(3),prim(4),primR(1), primR(3), primR(4),SL,SR)
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
  
      ! === Mis a jour du pas ===
      dt = CFL*dx/(Smax)
      tn = tn + dt
      print* , tn, "pas =", dt,"CFL =" ,(dt/dx)*Smax
  
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
     
  
      end subroutine Update_2