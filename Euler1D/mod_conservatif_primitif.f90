module mod_conservatif_primitif
    use mod_energie
    use mod_constante
    implicit none
contains 

  ! Transforme les variables primitives en conservatives
  function conservatif(rho,u,p) 
    
    implicit None
    real, intent(in) :: rho,u,p
    real, dimension(1:3) :: conservatif

    conservatif(1) = rho
    conservatif(2) = rho*u
    conservatif(3) = rho*((u**2)/2+energie(rho,p))
  end function conservatif

  function primitive(x1,x2,x3) 
    
    implicit None
    real, intent(in) :: x1,x2,x3
    real, dimension(1:3) :: primitive

    primitive(1) = x1 ! rho
    primitive(2) = x2/x1 ! u
    primitive(3) = (x3-0.5*((x2)**2)/x1)*(gamma-1) ! p
  end function primitive




end module  mod_conservatif_primitif