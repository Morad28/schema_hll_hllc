Module mod_flux
  use mod_energie
    implicit none
contains 
function fluxstd(rho,u,e,p) 
    
    implicit none

    real, intent(in) :: rho,u,e,p
    real, dimension(1:3) :: fluxstd
    
    fluxstd(1)=rho*u
    fluxstd(2)=rho*u**2+p
    fluxstd(3)=(e+p)*u
    

  end function fluxstd

function flux_hll(rhoL,uL,eL,pL,rhoR,uR,eR,pR,SL,SR)
   implicit none 

   real, intent(in) :: rhoL,uL,eL,pL,rhoR,uR,eR,pR,SL,SR
   real, dimension(1:3) :: flux_L, flux_R, flux_hll

   flux_L = fluxstd(rhoL,uL,eL,pL)
   flux_R = fluxstd(rhoR,uR,eR,pR)

   flux_hll(1) = (SR*flux_L(1)-SL*flux_R(1)+SL*SR*(rhoR-rhoL))/(SR-SL)
   flux_hll(2) = (SR*flux_L(2)-SL*flux_R(2)+SL*SR*(rhoR*uR-rhoL*uL))/(SR-SL)
   flux_hll(3) = (SR*flux_L(3)-SL*flux_R(3)+SL*SR*(0.5*rhoR*uR**2+rhoR*eR-(0.5*rhoL*uL**2+rhoL*eL)))/(SR-SL)
   end function flux_hll

function flux(rhoL,uL,eL,pL,rhoR,uR,eR,pR,SL,SR)
  implicit none
  
  real, intent(in) :: rhoL,uL,eL,pL,rhoR,uR,eR,pR,SL,SR
  real, dimension(1:3) :: flux
  
  if ( SL >= 0 ) then
    flux = fluxstd(rhoL,uL,eL,pL)
  end if
  if ( SR <= 0 ) then
    flux = fluxstd(rhoR,uR,eR,pR)
  end if
  if ( SL <= 0 .AND. SR >= 0 ) then
    flux = flux_hll(rhoL,uL,eL,pL,rhoR,uR,eR,pR,SL,SR)
  end if 

  end function flux




end module mod_flux
