module mod_CI
  use mod_energie
  use mod_constante
  implicit none
  real :: xmin = 0, xmax = 1, x0=0.5, x1 = 0.9
  !real :: rhoL = 1., uL = 0.75, pL = 1., rhoR = 0.125, uR = 0., pR = 0.1 ! sod test
  !real :: rhoL = 1., uL = -2., pL = 0.4, rhoR = 1., uR = 2., pR = 0.4
  real :: rhoL = 1., uL = 0., pL = 1000., rhoR = 1., uR = 0., pR = 0.01
  !real :: rhoL = 5.9924, uL = 19.5975, pL = 460.894, rhoR = 5.99242, uR = -6.19633, pR = 46.0950
  !real :: rhoL = 1., uL = 0., pL = 1000., rhoM = 1., uM = 0., pM = 0.01, rhoR = 1., uR = 0., pR = 100. ! Woodward et Colella test
  Integer :: N=3000

  contains

  subroutine Initialisation(U1,U2,U3)

    implicit none 
  
    real, dimension(1:3) :: U1, U2, U3
    real, dimension(1:N) :: X
    real ::  eL, eR
    integer :: i

    do i = 1, N
      X(i) = xmin + i*(xmax-xmin)/(N)
    end do

    eL=energie(rhoL,pL)
    eR=energie(rhoR,pR)
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


    end subroutine Initialisation

    ! subroutine Initialisation_m(U1,U2,U3)

    !   implicit none 
    
    !   real, dimension(1:3) :: U1, U2, U3
    !   real, dimension(1:N) :: X
    !   real ::  eL, eR, eM
    !   integer :: i
  
    !   do i = 1, N
    !     X(i) = xmin + i*(xmax-xmin)/(N)
    !   end do
  
    !   eL=energie(rhoL,pL,gamma,b)
    !   eR=energie(rhoR,pR,gamma,b)
    !   eM=energie(rhoM,pM,gamma,b)
    !   do i = 1, N

    !     if ( X(i)<x0  ) then
    !       U1(i) = rhoL
    !       U2(i) = rhoL*uL
    !       U3(i) = 0.5*rhoL*uL**2 + rhoL*eL
      
    !     end if
    !     if (X(i)>=x0 .AND. X(i)<x1) then
    !       U1(i) = rhoM
    !       U2(i) = rhoM*uM
    !       U3(i) = 0.5*rhoM*uM**2 + rhoM*eM
    !     end if
    !     if ( X(i) >= x1 ) then
    !       U1(i) = rhoR
    !       U2(i) = rhoR*uR
    !       U3(i) = 0.5*rhoR*uR**2 + rhoR*eR
    !     endif
    !   end do
  
  
    !   end subroutine Initialisation_m

end module mod_CI