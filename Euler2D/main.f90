program prog
    !============================
      Use mod_flux
      use mod_constante
      use mod_conservatif_primitif
      use mod_energie
      use mod_celerite
      use mod_CI
    !===========================
    
      Implicit None
      real::  dt, tn=0, e, eR, SR, SL, Smax
      real, Dimension(1:4) :: var, primR, prim
      real, Dimension(:), Allocatable::  u1i, u2i, u3i, u4i, flux1, flux2, flux3, flux4, fluy1, fluy2, fluy3, fluy4
      real, Dimension(:), Allocatable:: U_1, U_2, U_3, U_4
      Integer:: j, k, i,Bool=0,m
      NN=N*N

      Allocate(U_1(NN))
      Allocate(U_2(NN))
      Allocate(U_3(NN))
      Allocate(U_4(NN))
      Allocate(u1i(NN))
      Allocate(u2i(NN))
      Allocate(u3i(NN))
      Allocate(u4i(NN))
      Allocate(flux1(NN-N))
      Allocate(flux2(NN-N))
      Allocate(flux3(NN-N))
      Allocate(flux4(NN-N))
      Allocate(fluy1(NN-N))
      Allocate(fluy2(NN-N))
      Allocate(fluy3(NN-N))
      Allocate(fluy4(NN-N))

      ! Pas du maillage
      dx = 1./(N)

      ! Choix du cas test
      call Cas_test(2)
      ! Conditions initiales t = 0 
      call Initialisation(u1i,u2i,u3i,u4i)
      U_1 = u1i
      U_2 = u2i
      U_3 = u3i
      U_4 = u4i

    Loop : do while (tn<tout)
    !Loop : do m = 1, 2
      
      Smax = 0
      do j = 1, N-1
       Do i = 1, N-1
        k = i+(j-1)*N
        ! On recupere rho, u et p (variables primitives)  
        ! Axe x
        prim = primitive(u1i(k),u2i(k),u3i(k), u4i(k))
        primR = primitive(u1i(k+1),u2i(k+1),u3i(k+1), u4i(k+1))

        ! Calcul des energies
        e = energie(prim(1),prim(4))
        eR = energie(primR(1),primR(4))

        ! j+1/2
        ! Estimation de SL et SR sur cette interface
        call celerite_hyb(prim(1), prim(2),prim(4),primR(1), primR(2), primR(4),SL,SR)
        var = flux_hllc_x(prim(1), prim(2),prim(3), e, prim(4), primR(1), primR(2), primR(3), eR, primR(4), SL, SR)
        flux1(k) = var(1)
        flux2(k) = var(2)
        flux3(k) = var(3)
        flux4(k) = var(4)
        Smax = max(Smax, abs(SL), abs(SR))

        ! On recupere rho, u et p (variables primitives)  
        ! Axe y
        !prim = primitive(u1i(k),u2i(k),u3i(k),u4i(k))
        primR = primitive(u1i(k+N),u2i(k+N),u3i(k+N),u4i(k+N))
        eR = energie(primR(1),primR(4))

        ! j+1/2
        ! Estimation de SL et SR sur cette interface
        call celerite_hyb(prim(1), prim(3),prim(4),primR(1), primR(3), primR(4),SL,SR)
        var = flux_hllc_y(prim(1), prim(2),prim(3), e, prim(4), primR(1), primR(2), primR(3), eR, primR(4), SL, SR)
        fluy1(k) = var(1)
        fluy2(k) = var(2)
        fluy3(k) = var(3)
        fluy4(k) = var(4)
        Smax = max(Smax, abs(SL), abs(SR))
        
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
    print* , tn, "pas =", dt,"CFL =" ,(dt/dx)*Smax
  do j = 1, N
    do i = 1, N
      k = i+(j-1)*N
        ! Mis a jour de la solution
      if (i /= 1 .AND. i/=N .AND. j/=1 .AND. j/=N ) then
      U_1(k) = u1i(k) - (dt/dx)*(flux1(k) - flux1(k-1) + fluy1(k)-fluy1(k-N))
      U_2(k) = u2i(k) - (dt/dx)*(flux2(k) - flux2(k-1) + fluy2(k)-fluy2(k-N))
      U_3(k) = u3i(k) - (dt/dx)*(flux3(k) - flux3(k-1) + fluy3(k)-fluy3(k-N))
      U_4(k) = u4i(k) - (dt/dx)*(flux4(k) - flux4(k-1) + fluy4(k)-fluy4(k-N))
      endif
      ! if ( (i== 1 .OR. i == N) .AND. j>1) then
      !   U_1(k) = u1i(k) - (dt/dx)*(fluy1(k)-fluy1(k-N))
      !   U_2(k) = u2i(k) - (dt/dx)*(fluy2(k)-fluy2(k-N))
      !   U_3(k) = u3i(k) - (dt/dx)*(fluy3(k)-fluy3(k-N))
      !   U_4(k) = u4i(k) - (dt/dx)*( fluy4(k)-fluy4(k-N))
      ! endif
      ! if ( (j== 1 .OR. j == N) .AND. i>1) then
      !   U_1(k) = u1i(k) - (dt/dx)*(flux1(k) - flux1(k-1))
      !   U_2(k) = u2i(k) - (dt/dx)*(flux2(k) - flux2(k-1))
      !   U_3(k) = u3i(k) - (dt/dx)*(flux3(k) - flux3(k-1))
      !   U_4(k) = u4i(k) - (dt/dx)*(flux4(k) - flux4(k-1))
      ! endif
      if ( i == 1) then
        U_1(k) = u1i(k+1)
        U_2(k) = u2i(k+1) 
        U_3(k) = u3i(k+1) 
        U_4(k) = u4i(k+1) 
      endif
      if ( i == N) then
        U_1(k) = u1i(k-1)
        U_2(k) = u2i(k-1) 
        U_3(k) = u3i(k-1) 
        U_4(k) = u4i(k-1) 
      endif
      if ( j == 1) then
        U_1(k) = u1i(k+N)
        U_2(k) = u2i(k+N) 
        U_3(k) = u3i(k+N) 
        U_4(k) = u4i(k+N) 
      endif
      if ( j == N) then
        U_1(k) = u1i(k-N)
        U_2(k) = u2i(k-N) 
        U_3(k) = u3i(k-N) 
        U_4(k) = u4i(k-N) 
      endif
    Enddo
  end do    
  ! Quitter le programme si la positivite de pression est violee
  if (Bool==1) EXIT
    
  u1i = U_1
  u2i = U_2
  u3i = U_3
  u4i = U_4 

endDo Loop

! Sortie
open(unit=4,file='out.txt')
Do j=1,N
  Do i=1,N
    k = i + (j-1)*N
     write(4,*) i*dx, j*dx , primitive(u1i(k), u2i(k), u3i(k), u4i(k)), flux1(k)
     !if (i==N) write(4,*)
  EndDo   
Enddo 
close(4)

end program prog