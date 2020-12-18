Module mod_flux
  use mod_energie
  Use mod_conservatif_primitif
  use mod_celerite
  use mod_constante
    implicit none
contains 
function fluxstd(rho,u,e,p) 
    
    implicit none

    real, intent(in) :: rho,u,e,p
    real, dimension(1:3) :: fluxstd
    
    fluxstd(1)=rho*u
    fluxstd(2)=rho*u**2+p
    fluxstd(3)=(0.5*rho*u**2+rho*e+p)*u
    

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

  function flux_hllc(rhoL,uL,eL,pL,rhoR,uR,eR,pR,SL,SR)
    implicit none
    
    real, intent(in) :: rhoL,uL,eL,pL,rhoR,uR,eR,pR,SL,SR
    real :: SM,rhoBarre, aBarre, aL, aR
    real, dimension(1:3) :: flux_hllc, ULEtoile, UREtoile, flux_L, flux_R, Q, R
    
   
    aL = a(rhoL,pL,gamma)
    aR = a(rhoR,pR,gamma)
    aBarre = 0.5*(aL+aR)
    rhoBarre = 0.5*(rhoL+rhoR)
    !SM = (pR-pL+rhoL*uL*(SL-uL)-rhoR*uR*(SR-uR))/(rhoL*(SL-uL)-rhoR*(SR-uR))
    
    SM = 0.5*(uL+uR)-0.5*(pR-pL)/(rhoBarre*aBarre)
    flux_L = fluxstd(rhoL,uL,eL,pL)
    flux_R = fluxstd(rhoR,uR,eR,pR)

    Q(1) = SL*rhoL - flux_L(1)
    Q(2) = SL*rhoL*uL - flux_L(2)
    Q(3) = SL*(0.5*rhoL*uL**2+rhoL*eL) - flux_L(3)

    R(1) = SR*rhoR - flux_R(1)
    R(2) = SR*rhoR*uR - flux_R(2)
    R(3) = SR*(0.5*rhoR*uR**2+rhoR*eR) - flux_R(3)

    ULEtoile(1) = Q(1)/(SL-SM)
    ULEtoile(2) = ULEtoile(1)*SM
    ULEtoile(3) = (Q(3)+SM*(SM*Q(1)-Q(2)))/(SL-SM)

    UREtoile(1) = R(1)/(SR-SM)
    UREtoile(2) = UREtoile(1)*SM
    UREtoile(3) = (R(3)+SM*(SM*R(1)-R(2)))/(SR-SM)
 


    if ( SL >= 0 ) then
      flux_hllc = flux_L
    end if
    if ( SR <= 0 ) then
      flux_hllc = flux_R
    end if
    if ( SL <= 0 .AND. SM >= 0 ) then
      flux_hllc(1) = flux_L(1) + SL*(ULEtoile(1)-rhoL)
      flux_hllc(2) = flux_L(2) + SL*(ULEtoile(2)-rhoL*uL)
      flux_hllc(3) = flux_L(3) + SL*(ULEtoile(3)-(0.5*rhoL*uL**2 + rhoL*eL))

      !print*,"out flux hll", flux_hllc
    end if 
    if ( SM <= 0 .AND. SR >= 0 ) then
      flux_hllc(1) = flux_R(1) + SR*(UREtoile(1)-rhoR)
      flux_hllc(2) = flux_R(2) + SR*(UREtoile(2)-rhoR*uR)
      flux_hllc(3) = flux_R(3) + SR*(UREtoile(3)-(0.5*rhoR*uR**2+rhoR*eR))
    end if 
  
    end function flux_hllc





end module mod_flux
