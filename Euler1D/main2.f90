program prog
    !============================
      Use mod_flux
      use mod_constante
      use mod_conservatif_primitif
      use mod_energie
      use mod_celerite
    !===========================
    
      Implicit None
      real:: dx, dt, tn=0, eL, e, eR, rhoEtoile, uEtoile, pEtoile, SR=0, SL=0, S,maxi, maxi2
      real, Dimension(1:3) :: var, var2, fluxEtoile, primL, primR, prim
      real, Dimension(:), Allocatable:: U1, U2, U3, X, u1i, u2i, u3i, flux1, flux2, flux3, inter
      Integer:: i,j
      Allocate(U1(N))
      Allocate(U2(N))
      Allocate(U3(N))
      Allocate(u1i(N))
      Allocate(u2i(N))
      Allocate(u3i(N))
      Allocate(flux1(N-1))
      Allocate(flux2(N-1))
      Allocate(flux3(N-1))
      Allocate(X(N))
      Allocate(inter(N-1))
     
      dx = 1./(2*N)
      dt = 0.1/100

      ! Abscisses
      do i = 1, N
        X(i) = xmin + i*(xmax-xmin)/(N)
      end do
      do i = 1, N-1
        inter(i) = (X(i)+X(i+1))/2
        print*, X(i),inter(i), X(i+1)
      end do

      ! Estimation de SL et SR
      call celerite_iso(rhoL,uL,pL,rhoR,uR,pR,gamma,SL,SR)
      
      ! Conditions initiales t = 0 
      eL=energie(rhoL,pL,gamma,b)
      eR=energie(rhoR,pR,gamma,b)
      do i = 1, N
        if ( X(i)<x0  ) then
          U1(i) = rhoL
          U2(i) = rhoL*uL
          U3(i) = 0.5*rhoL*uL**2 + rhoL*eL
      
        else
          U1(i) = rhoR
          U2(i) = rhoR*uR
          U3(i) = 0.5*rhoR*uR**2 + rhoR*eR
        end if
      end do
      maxi = 0
      do i = 1, N-1
        prim = primitive(U1(i),U2(i),U3(i),gamma)
        primR = primitive(U1(i+1),U2(i+1),U3(i+1),gamma)
        call celerite_lin(prim(1), prim(2),prim(3),primR(1), primR(2), primR(3),gamma,SL,SR)
        maxi = max((a(prim(1),prim(3),gamma)+abs(prim(2))),maxi)
      end do
      print* , j , SL, SR, prim(1), prim(2),prim(3)
      !print*,  maxi
      u1i = U1
      u2i = U2
      u3i = U3


      Loop : Do while (tn<0.1)  
    !Loop : do i = 1, 2000
      tn = tn + dt

      maxi2=0
      do j = 1, N-1
        
        ! On recupere rho, u et p (variables primitives)
        !primL = primitive(u1i(j-1),u2i(j-1),u3i(j-1),gamma)
        prim = primitive(u1i(j),u2i(j),u3i(j),gamma)
        primR = primitive(u1i(j+1),u2i(j+1),u3i(j+1),gamma)

        ! Calcul des energies
        !eL = energie(primL(1),primL(3),gamma,b)
        e = energie(prim(1),prim(3),gamma,b)
        eR = energie(primR(1),primR(3),gamma,b)

        ! j+1/2
        ! Estimation de SL et SR sur cette interface
        call celerite_lin(prim(1), prim(2),prim(3),primR(1), primR(2), primR(3),gamma,SL,SR)
        print* , j , SL, SR, prim(1), prim(2),prim(3), maxi
        var = flux(prim(1), prim(2), e, prim(3), primR(1), primR(2), eR, primR(3), SL, SR)
        flux1(j) = var(1)
        flux2(j) = var(2)
        flux3(j) = var(3)
        maxi2 = max(maxi2, abs(SR), abs(SL))
      end do
      
      dt = 0.025*dx/maxi2
      print* , (dt/dx)*maxi
        !print* , j, var
        !maxi2 = max((a(prim(1),prim(3),gamma)+abs(prim(2))),maxi2, abs(SR), abs(SL))

        ! j-1/2
        !call celerite_iso(primL(1), primL(2),primL(3),prim(1), prim(2), prim(3),gamma,SL,SR)
        !var2 = flux(primL(1), primL(2), eL, primL(3), prim(1),prim(2), e, prim(3), SL, SR,S)     
        !maxi2 = max((a(primL(1),primL(3),gamma)+abs(prim(2))),maxi2, abs(SR), abs(SL))
 
      !print*, maxi2
      do j = 2, N-1
        ! Mis a jour de la solution
        U1(j) = u1i(j) - (dt/dx)*(flux1(j)  - flux1(j-1))
        U2(j) = u2i(j) - (dt/dx)*(flux2(j)  - flux2(j-1))
        U3(j) = u3i(j) - (dt/dx)*(flux3(j)  - flux3(j-1))

      end do
      u1i = U1
      u2i = U2
      u3i = U3 
      maxi = maxi2
      !dt = 0.05*dx/maxi
    endDo Loop

      open(unit=4,file='out.txt')

      write(4,*) tn
      Do j=1,N
           write(4,*) X(j),primitive(U1(j), U2(j), U3(j),gamma)
      EndDo    

      close(4)

      do i = 1, N
        !print*, tn ,  i , primitive(U1(i),U2(i),U3(i),gamma)
      end do
end program prog