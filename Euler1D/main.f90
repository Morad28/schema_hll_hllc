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
      real:: dx, dt, tn=0, e, eR, SR, SL, Smax
      real, Dimension(1:3) :: var, primR, prim
      real, Dimension(:), Allocatable:: U1, U2, U3, u1i, u2i, u3i, flux1, flux2, flux3
      Integer:: j, Bool=0

      Allocate(U1(N))
      Allocate(U2(N))
      Allocate(U3(N))
      Allocate(u1i(N))
      Allocate(u2i(N))
      Allocate(u3i(N))
      Allocate(flux1(N-1))
      Allocate(flux2(N-1))
      Allocate(flux3(N-1))

      dx = 1./(N)
      !dt = 0.00001
  

      call Cas_test(5)
      ! Conditions initiales t = 0 
      call Initialisation(u1i,u2i,u3i)
      U1(1) = u1i(1)
      U2(1) = u2i(1)
      U3(1) = u3i(1)

      U1(N) = u1i(N)
      U2(N) = u2i(N)
      U3(N) = u3i(N)

    Loop : do while (tn<tout)

      
      Smax = 0
      do j = 1, N-1
       
        !On recupere rho, u et p (variables primitives)
        prim = primitive(u1i(j),u2i(j),u3i(j))
        primR = primitive(u1i(j+1),u2i(j+1),u3i(j+1))

        ! Calcul des energies
        e = energie(prim(1),prim(3))
        eR = energie(primR(1),primR(3))

        ! j+1/2
        ! Estimation de SL et SR sur cette interface
        call celerite_iso(prim(1), prim(2),prim(3),primR(1), primR(2), primR(3),SL,SR)
        var = flux(prim(1), prim(2), e, prim(3), primR(1), primR(2), eR, primR(3), SL, SR)
        flux1(j) = var(1)
        flux2(j) = var(2)
        flux3(j) = var(3)
        Smax = max(Smax, abs(SL), abs(SR))

        ! Verification positivite de la pression
        if (prim(3)<0) then
          print*, "ERREUR PRESSION NEGATIVE", j, tn,  prim
          Bool=1
          EXIT
        end if

    end do
    ! Mis a jour du pas 
    dt = CFL*dx/Smax
    tn = tn + dt
    print* , tn, "pas =", dt,"CFL =" ,(dt/dx)*Smax
    do j = 2, N-1
        ! Mis a jour de la solution
      U1(j) = u1i(j) - (dt/dx)*(flux1(j)  - flux1(j-1))
      U2(j) = u2i(j) - (dt/dx)*(flux2(j)  - flux2(j-1))
      U3(j) = u3i(j) - (dt/dx)*(flux3(j)  - flux3(j-1))
    
      
    end do
      ! Quitter le programme si la positivite de pression est violee
      if (Bool==1) EXIT
      
      u1i = U1
      u2i = U2
      u3i = U3 
      print*, tn
    endDo Loop

    ! Sortie
    open(unit=4,file='out.txt')
    Do j=1,N
         write(4,*) xmin + j*(xmax-xmin)/(N),primitive(U1(j), U2(j), U3(j))
    EndDo    
    close(4)

end program prog