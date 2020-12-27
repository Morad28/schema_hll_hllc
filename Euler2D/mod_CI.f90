module mod_CI
  use mod_energie
  use mod_constante
  implicit none
  real :: xmin = 0, xmax = 1, ymin = 0, ymax = 1, x0, x1
  real :: p1,p2,p3,p4,rho1,rho2,rho3,rho4,u1,u2,u3,u4,v1,v2,v3,v4, tout 
  Integer :: N=101, NN

  contains
  subroutine Cas_test(n)
    
    implicit None
    integer, intent(in) :: n

    if (n==1) then
      p1 = 1.; rho1 = 1.; u1 = 0.; v1 = 0.
      p2 = 0.5197; rho2 = 0.5197 ; u2 = -0.7259; v2 = 0.; 
      p3 = 0.0439; rho3 = 0.1072; u3 = -0.7259; v3 = -1.4045;
      p4 = 0.15; rho4 = 0.2579; u4 = 0.; v4 = -1.4045;
      tout = 0.2
      x0 = 0.5 ! sod test
    end if
    if (n==2) then
      p1 = 1.; rho1 = 1.; u1 = 0.; v1 = 0.
      p2 = 0.4; rho2 = 0.5197 ; u2 = -0.7259; v2 = 0.; 
      p3 = 1.; rho3 = 1.; u3 = -0.7259; v3 = -0.7259;
      p4 = 0.4; rho4 = 0.5197; u4 = 0.; v4 = -0.7259;
      tout = 0.2
      x0 = 0.5 !
    end if
    if (n==3) then
      p1 = 1.5; rho1 = 1.5; u1 = 0.; v1 = 0.
      p2 = 0.3; rho2 = 0.5323 ; u2 = 1.206; v2 = 0.; 
      p3 = 0.029; rho3 = 0.138; u3 = 1.206; v3 = 1.206;
      p4 = 0.3; rho4 = 0.5323; u4 = 0.; v4 = 1.206;
      tout = 0.3
      x0 = 0.5 !
    end if
    if (n==4) then
      !rhoL = 5.9924; uL = 19.5975; pL = 460.894; rhoR = 5.99242; uR = -6.19633; pR = 46.0950; tout = 0.035; x0 =0.4
    end if
    if (n==5) then
      !rhoL = 1.; uL = -19.59745; pL = 1000.; rhoR = 1.; uR = -19.59745; pR = 0.01; tout = 0.012; x0 = 0.8
    end if
    if (n==6) then
      !rhoL = 1.4; uL = 0.; pL = 1.; rhoR = 1.; uR = 0.; pR = 1.; tout = 2.; x0 = 0.5
    end if
    if (n==7) then
      !rhoL = 1.4; uL = 0.1; pL = 1.; rhoR = 1.; uR = 0.1; pR = 1.; tout = 2.; x0 = 0.5
    end if
    if (n==8) then
      !rhoL = 1.; uL = 0.; pL = 1000.; rhoM = 1.; uM = 0.; pM = 0.01; rhoR = 1.; uR = 0.; pR = 100.; tout = 0.028 ;x0 = 0.1; x1 = 0.9
    end if


  end subroutine Cas_test


  subroutine Initialisation(U_1,U_2,U_3,U_4)

    implicit none 
  
    real, dimension(1:N*N) :: U_1, U_2, U_3, U_4
    real, dimension(1:N) :: X, Y
    real ::  e1, e2, e3, e4
    integer :: i, k, j

    do i = 1, N
      X(i) = xmin + i*(xmax-xmin)/(N)
      Y(i) = ymin + i*(ymax-ymin)/(N)
    end do

    e1=energie(rho1,p1)
    e2=energie(rho2,p2)
    e3=energie(rho3,p3)
    e4=energie(rho4,p4)
    do i = 1, N
      do j = 1,N
      k = i + (j-1)*N
      if ( X(i)>x0 .AND. Y(j)>x0  ) then
        U_1(k) = rho1
        U_2(k) = rho1*u1
        U_3(k) = rho1*v1
        U_4(k) = 0.5*rho1*(u1**2+v1**2) + rho1*e1
      end if
      if ( X(i)<x0 .AND. Y(j)>x0  ) then
        U_1(k) = rho2
        U_2(k) = rho2*u2
        U_3(k) = rho2*v2
        U_4(k) = 0.5*rho2*(u2**2+v2**2) + rho2*e2
      end if
      if ( X(i)<x0 .AND. Y(j)<x0  ) then
        U_1(k) = rho3
        U_2(k) = rho3*u3
        U_3(k) = rho3*v3
        U_4(k) = 0.5*rho3*(u3**2+v3**2) + rho3*e3
      end if
      if ( X(i)>x0 .AND. Y(j)<x0  ) then
        U_1(k) = rho4
        U_2(k) = rho4*u4
        U_3(k) = rho4*v4
        U_4(k) = 0.5*rho4*(u4**2+v4**2) + rho4*e4
      end if
    end do
    end do


    end subroutine Initialisation


end module mod_CI