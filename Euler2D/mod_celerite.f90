module mod_celerite
  use mod_constante
    implicit none
    real :: SL = 0, SR = 0
contains 

  ! Calcule la vitesse du son (pour un gaz parfait)
  function a(rho,p,gamma)
    implicit None
    real, intent(in) :: rho,p,gamma
    real :: a

    a = (gamma*p/rho)**(0.5)

  end function a

  subroutine celerite_iso(rhoL,uL,pL,rhoR,uR,pR,SL,SR) 
    
    implicit none
    real, intent(in) :: rhoL,uL,pL,rhoR,uR,pR
    real :: SL, SR, uEtoile, aEtoile, aL, aR

    aL = a(rhoL,pL,gamma)
    aR = a(rhoR,pR,gamma)
    uEtoile = 0.5*(uL-uR)+(aL-aR)/(gamma-1)
    aEtoile = 0.5*(aL+aR)+0.25*(gamma-1)*(uL-uR)

    SL = min(uL-aL,uEtoile-aEtoile)
    SR = max(uR+aR,uEtoile+aEtoile)
    !print* , " Call", aL, aR, uEtoile, aEtoile , rhoL,uL,pL,rhoR,uR,pR,SL,SR
  end subroutine celerite_iso

  subroutine celerite_lin(rhoL,uL,pL,rhoR,uR,pR,SL,SR)

    implicit None 
    real, intent(in) :: rhoL,uL,pL,rhoR,uR,pR
    real :: SL, SR, uEtoile, aLEtoile, aREtoile, pEtoile, rhoLEtoile, rhoREtoile, aBarre, rhoBarre,aL, aR
    
    aL = a(rhoL,pL,gamma)
    aR = a(rhoR,pR,gamma)
    aBarre = 0.5*(aL+aR)
    rhoBarre = 0.5*(rhoL+rhoR)
    uEtoile = 0.5*(uL+uR)-0.5*(pR-pL)/(rhoBarre*aBarre)
    pEtoile = 0.5*(pL+pR)-0.5*(uR-uL)*(rhoBarre*aBarre)
    rhoLEtoile = rhoL + (uL-uEtoile)*rhoBarre/aBarre
    rhoREtoile = rhoR + (uEtoile-uR)*rhoBarre/aBarre

    aLEtoile = (gamma*pEtoile/rhoLEtoile)**(0.5)
    aREtoile = (gamma*pEtoile/rhoREtoile)**(0.5)

    SL = min(uL-aL,uEtoile-aLEtoile)
    SR = max(uR+aR,uEtoile+aREtoile)


    end subroutine celerite_lin

    subroutine celerite_hyb(rhoL,uL,pL,rhoR,uR,pR,SL,SR)

      implicit None 
      real, intent(in) :: rhoL,uL,pL,rhoR,uR,pR
      real :: pEtoile, rhoBarre, aBarre, SL, SR, ppvrs, aL, aR, qL, qR

      aL = a(rhoL,pL,gamma)
      aR = a(rhoR,pR,gamma)

      rhoBarre = 0.5*(rhoL+rhoR)
      aBarre = 0.5*(aL+aR)
      ppvrs = 0.5*(pL+pR)-0.5*(uR-uL)*rhoBarre*aBarre


      pEtoile = max(0., ppvrs)
      !pEtoile = 0.5*(pL+pR)-0.5*(uR-uL)*rhoBarre*aBarre
      !pEtoile = pL+rhoL*(uL-SL)*(uL-SM)
      !pEtoile = ((aL+0.5*(uL-uR)*(gamma-1))+(aR+0.5*(uL-uR)*(gamma-1)))/()

      if (pEtoile<=pL) then
        qL = 1
      else 
        qL = (1+((gamma+1)/(2*gamma))*((pEtoile/pL)-1))**0.5
      endif

      if (pEtoile<=pR) then
        qR = 1
      else 
        qR = (1+((gamma+1)/(2*gamma))*((pEtoile/pR)-1))**0.5
      endif
      !print*, "qr", pEtoile
      SL = uL - aL*qL
      SR = uR + aR*qR
    end subroutine celerite_hyb

    subroutine estimation_p(rhoL,uL,pL,rhoR,uR,pR, pEtoile)
          
      implicit None
      real, intent(in) :: rhoL,uL,pL,rhoR,uR,pR
      real :: pEtoile, rhoBarre, aBarre, ppvrs, aL, aR, z, gamma1

      aL = a(rhoL,pL,gamma)
      aR = a(rhoR,pR,gamma)

      rhoBarre = 0.5*(rhoL+rhoR)
      aBarre = 0.5*(aL+aR)

      gamma1=0.5*(gamma-1)
      z = gamma1/gamma
      ppvrs = 0.5*(pL+pR)-0.5*(uR-uL)*rhoBarre*aBarre ! simple acoustic
      !ppvrs = ((aL+aR-0.5*(gamma-1)*(uR-uL))/)
      pEtoile = max(0., ppvrs)

    end subroutine estimation_p
    

end module mod_celerite