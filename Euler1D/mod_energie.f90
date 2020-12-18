module mod_energie
  use mod_constante
    implicit none
contains 

  ! Donne l'energie en fonction de rho, p, gamma, et b (b=0 pour un gaz parfait)
  function energie(rho,p) 
    
    implicit none

    real, intent(in) :: rho,p
    real :: energie
    
    energie = (p*(1-b*rho))/((gamma-1)*rho)
    

  end function energie

end module mod_energie