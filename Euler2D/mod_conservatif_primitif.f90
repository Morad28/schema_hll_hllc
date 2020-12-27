module mod_conservatif_primitif
    use mod_energie
    use mod_constante
    implicit none
contains 

  ! Transforme les variables primitives en conservatives
  function conservatif(rho,u,v,p) 
    
    implicit None
    real, intent(in) :: rho,u,v,p
    real, dimension(1:4) :: conservatif

    conservatif(1) = rho
    conservatif(2) = rho*u
    conservatif(3) = rho*v
    conservatif(4) = rho*((u**2+v**2)/2+energie(rho,p))
  end function conservatif

  function primitive(x1,x2,x3,x4) 
    
    implicit None
    real, intent(in) :: x1,x2,x3,x4
    real, dimension(1:4) :: primitive

    primitive(1) = x1 ! rho
    primitive(2) = x2/x1 ! u
    primitive(3) = x3/x1 ! v
    primitive(4) = (x4-0.5*x1*(primitive(2)**2+primitive(3)**2))*(gamma-1) ! p
  end function primitive




end module  mod_conservatif_primitif