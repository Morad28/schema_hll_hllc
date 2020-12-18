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
      real:: dx, dt, tn=0, eL, e, eR, SR, SL, maxi, maxi2, tout
      real, Dimension(1:3) :: var, var2, primL, primR, prim
      real, Dimension(:), Allocatable:: U1, U2, U3, u1i, u2i, u3i
      Integer:: i,j, Bool=0

      Allocate(U1(N))
      Allocate(U2(N))
      Allocate(U3(N))
      Allocate(u1i(N))
      Allocate(u2i(N))
      Allocate(u3i(N))
      tout = 0.012
      dx = 1./(N)
      dt = 0.000001


      ! Conditions initiales t = 0 
      call Initialisation(u1i,u2i,u3i)
      U1(1) = u1i(1)
      U2(1) = u2i(1)
      U3(1) = u3i(1)

      U1(N) = u1i(N)
      U2(N) = u2i(N)
      U3(N) = u3i(N)

    Loop : do while (tn<tout)

      tn = tn + dt

      do j = 1, N
        if ( j/= 1 .and. j/=N ) then
        ! On recupere rho, u et p (variables primitives)
        primL = primitive(u1i(j-1),u2i(j-1),u3i(j-1))
        prim = primitive(u1i(j),u2i(j),u3i(j))
        primR =primitive(u1i(j+1),u2i(j+1),u3i(j+1))

        ! Verification positivite de la pression
        if (prim(3)<0) then
          print*, "ERREUR PRESSION NEGATIVE", j, tn,  prim
          Bool=1
          EXIT
        end if

        ! Calcul des energies
        eL = energie(primL(1),primL(3))
        e = energie(prim(1),prim(3))
        eR = energie(primR(1),primR(3))

        ! j+1/2
        ! Estimation de SL et SR sur cette interface
        call celerite_hyb(prim(1), prim(2),prim(3),primR(1), primR(2), primR(3),SL,SR)
        var = flux_hllc(prim(1), prim(2), e, prim(3), primR(1), primR(2), eR, primR(3), SL, SR)

        ! j-1/2
        call celerite_hyb(primL(1), primL(2),primL(3),prim(1), prim(2), prim(3),SL,SR)
        var2 = flux_hllc(primL(1), primL(2), eL, primL(3), prim(1),prim(2), e, prim(3), SL, SR)     
       
        ! Mis a jour de la solution
        U1(j) = u1i(j) - (dt/dx)*(var(1) - var2(1))
        U2(j) = u2i(j) - (dt/dx)*(var(2) - var2(2))
        U3(j) = u3i(j) - (dt/dx)*(var(3) - var2(3))
        else
          

      endif 
      
      end do
        ! U1(1) = u1i(2) 
        ! U2(1) = u2i(2) 
        ! U3(1) = u3i(2) 
        ! U1(N) = u1i(N-1) 
        ! U2(N) = u2i(N-1) 
        ! U3(N) = u3i(N-1) 
      if (Bool==1) EXIT
      u1i = U1
      u2i = U2
      u3i = U3 
      maxi = maxi2
      print*, tn
    endDo Loop

      open(unit=4,file='out.txt')

      Do j=1,N
           write(4,*) xmin + j*(xmax-xmin)/(N),primitive(U1(j), U2(j), U3(j))
      EndDo    

      close(4)
end program prog