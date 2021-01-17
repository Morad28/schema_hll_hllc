Module mod_flux
  use mod_energie
  Use mod_conservatif_primitif
  use mod_celerite
  use mod_constante
    implicit none
    real, Dimension(1:N*N)::  u1i, u2i, u3i, u4i
    real, dimension(1:N*N-N):: flux1, flux2, flux3, flux4, fluy1, fluy2, fluy3, fluy4
    real, Dimension(1:N*N):: U_1, U_2, U_3, U_4
contains 
! Fonction flux par rapport a x
function flux_x(rho,u,v,e,p) 
    
    implicit none

    real, intent(in) :: rho,u,v,e,p
    real, dimension(1:4) :: flux_x
    
    flux_x(1)=rho*u
    flux_x(2)=rho*u**2+p
    flux_x(3)=rho*u*v
    flux_x(4)=(0.5*rho*(u**2+v**2)+rho*e+p)*u
    

  end function flux_x

! Fonction flux par rapport a y
function flux_y(rho,u,v,e,p) 
    
    implicit none

    real, intent(in) :: rho,u,v,e,p
    real, dimension(1:4) :: flux_y
    
    flux_y(1)=rho*v
    flux_y(2)=rho*u*v
    flux_y(3)=rho*v**2+p 
    flux_y(4)=(0.5*rho*(u**2+v**2)+rho*e+p)*v
    

  end function flux_y

! Fonction intermediaire pour calculer le flux si SL<0 et SR>0
function flux_hll_x(rhoL,uL,vL,eL,pL,rhoR,uR,vR,eR,pR,SL,SR)
   implicit none 

   real, intent(in) :: rhoL,uL,vL,eL,pL,rhoR,uR,vR,eR,pR,SL,SR
   real, dimension(1:4) :: flux_L, flux_R, flux_hll_x

   flux_L = flux_x(rhoL,uL,vL,eL,pL)
   flux_R = flux_x(rhoR,uR,vL,eR,pR)

   flux_hll_x(1) = (SR*flux_L(1)-SL*flux_R(1)+SL*SR*(rhoR-rhoL))/(SR-SL)
   flux_hll_x(2) = (SR*flux_L(2)-SL*flux_R(2)+SL*SR*(rhoR*uR-rhoL*uL))/(SR-SL)
   flux_hll_x(3) = (SR*flux_L(3)-SL*flux_R(3)+SL*SR*(rhoR*vR-rhoL*vL))/(SR-SL)
   flux_hll_x(4) = (SR*flux_L(4)-SL*flux_R(4)+SL*SR*(0.5*rhoR*(uR**2+vR**2)+rhoR*eR-(0.5*rhoL*(uL**2+vL**2)+rhoL*eL)))/(SR-SL)
   end function flux_hll_x

! Fonction intermediaire pour calculer le flux si SL<0 et SR>0
function flux_hll_y(rhoL,uL,vL,eL,pL,rhoR,uR,vR,eR,pR,SL,SR)
    implicit none 
 
    real, intent(in) :: rhoL,uL,vL,eL,pL,rhoR,uR,vR,eR,pR,SL,SR
    real, dimension(1:4) :: flux_L, flux_R, flux_hll_y
 
    flux_L = flux_y(rhoL,uL,vL,eL,pL)
    flux_R = flux_y(rhoR,uR,vL,eR,pR)
 
    flux_hll_y(1) = (SR*flux_L(1)-SL*flux_R(1)+SL*SR*(rhoR-rhoL))/(SR-SL)
    flux_hll_y(2) = (SR*flux_L(2)-SL*flux_R(2)+SL*SR*(rhoR*uR-rhoL*uL))/(SR-SL)
    flux_hll_y(3) = (SR*flux_L(3)-SL*flux_R(3)+SL*SR*(rhoR*vR-rhoL*vL))/(SR-SL)
    flux_hll_y(4) = (SR*flux_L(4)-SL*flux_R(4)+SL*SR*(0.5*rhoR*(uR**2+vR**2)+rhoR*eR-(0.5*rhoL*(uL**2+vL**2)+rhoL*eL)))/(SR-SL)
    end function flux_hll_y

! FLux HLL sur l'axe x 
function flux_HLLx(rhoL,uL,vL,eL,pL,rhoR,uR,vR,eR,pR,SL,SR)
  implicit none
  
  real, intent(in) :: rhoL,uL,vL,eL,pL,rhoR,uR,vR,eR,pR,SL,SR
  real, dimension(1:4) :: flux_HLLx
  
  if ( SL >= 0 ) then
    flux_HLLx = flux_x(rhoL,uL,vL,eL,pL)
  end if
  if ( SR <= 0 ) then
    flux_HLLx = flux_x(rhoR,uR,vR,eR,pR)
  end if
  if ( SL <= 0 .AND. SR >= 0 ) then
    flux_HLLx = flux_hll_x(rhoL,uL,vL,eL,pL,rhoR,uR,vR,eR,pR,SL,SR)
  end if 

  end function flux_HLLx

  ! FLux HLL sur l'axe y
  function flux_HLLy(rhoL,uL,vL,eL,pL,rhoR,uR,vR,eR,pR,SL,SR)
    implicit none
    
    real, intent(in) :: rhoL,uL,vL,eL,pL,rhoR,uR,vR,eR,pR,SL,SR
    real, dimension(1:4) :: flux_HLLy
    
    if ( SL >= 0 ) then
      flux_HLLy = flux_y(rhoL,uL,vL,eL,pL)
    end if
    if ( SR <= 0 ) then
      flux_HLLy = flux_y(rhoR,uR,vR,eR,pR)
    end if
    if ( SL <= 0 .AND. SR >= 0 ) then
      flux_HLLy = flux_hll_y(rhoL,uL,vL,eL,pL,rhoR,uR,vR,eR,pR,SL,SR)
    end if 
  
    end function flux_HLLy


  function flux_hllc_x(rhoL,uL,vL,eL,pL,rhoR,uR,vR,eR,pR,SL,SR)
    implicit none
    
    real, intent(in) :: rhoL,uL,vL,eL,pL,rhoR,uR,vR,eR,pR,SL,SR
    real :: SM,rhoBarre, aBarre, aL, aR, CL, CR
    real, dimension(1:4) :: flux_hllc_x, ULEtoile, UREtoile, flux_L, flux_R, Q, R
    
   
    aL = a(rhoL,pL,gamma)
    aR = a(rhoR,pR,gamma)
    aBarre = 0.5*(aL+aR)
    rhoBarre = 0.5*(rhoL+rhoR)
    !SM = (pR-pL+rhoL*uL*(SL-uL)-rhoR*uR*(SR-uR))/(rhoL*(SL-uL)-rhoR*(SR-uR))
    
    SM = 0.5*(uL+uR)-0.5*(pR-pL)/(rhoBarre*aBarre)
    flux_L = flux_x(rhoL,uL,vL,eL,pL)
    flux_R = flux_x(rhoR,uR,vR,eR,pR)

    ! Q(1) = SL*rhoL - flux_L(1)
    ! Q(2) = SL*rhoL*uL - flux_L(2)
    ! Q(3) = SL*rhoL*vL - flux_L(3)
    ! Q(4) = SL*(0.5*rhoL*(uL**2+vL**2)+rhoL*eL) - flux_L(4)

    ! R(1) = SR*rhoR - flux_R(1)
    ! R(2) = SR*rhoR*uR - flux_R(2)
    ! R(3) = SR*rhoR*vR - flux_R(3)
    ! R(4) = SR*(0.5*rhoR*(uR**2+vR**2)+rhoR*eR) - flux_R(4)

    CL = rhoL*(SL-uL)/(SL-SM)
    ULEtoile(1) = CL
    ULEtoile(2) = CL*SM
    ULEtoile(3) = CL*vL
    ULEtoile(4) = CL*(0.5*(uL**2+vL**2)+eL + (SM-uL)*(SM + pL/(rhoL*(SL-uL))))

    CR = rhoR*(SR-uR)/(SR-SM)
    UREtoile(1) = CR
    UREtoile(2) = CR*SM
    UREtoile(3) = CR*vR
    UREtoile(4) = CR*(0.5*(uR**2+vR**2)+eR + (SM-uR)*(SM + pR/(rhoR*(SR-uR))))
 


    if ( SL >= 0 ) then
      flux_hllc_x = flux_L
    end if
    if ( SR <= 0 ) then
      flux_hllc_x = flux_R
    end if
    if ( SL <= 0 .AND. SM >= 0 ) then
      flux_hllc_x(1) = flux_L(1) + SL*(ULEtoile(1)-rhoL)
      flux_hllc_x(2) = flux_L(2) + SL*(ULEtoile(2)-rhoL*uL)
      flux_hllc_x(3) = flux_L(3) + SL*(ULEtoile(3)-rhoL*vL)
      flux_hllc_x(4) = flux_L(4) + SL*(ULEtoile(4)-(0.5*rhoL*(uL**2+vL**2)+rhoL*eL))

      !print*,"out flux hll", flux_hllc
    end if 
    if ( SM <= 0 .AND. SR >= 0 ) then
      flux_hllc_x(1) = flux_R(1) + SR*(UREtoile(1)-rhoR)
      flux_hllc_x(2) = flux_R(2) + SR*(UREtoile(2)-rhoR*uR)
      flux_hllc_x(3) = flux_R(3) + SR*(UREtoile(3)-rhoR*vR)
      flux_hllc_x(4) = flux_R(4) + SR*(UREtoile(4)-(0.5*rhoR*(uR**2+vR**2) + rhoR*eR))
    end if 
  
    end function flux_hllc_x

    function flux_hllc_y(rhoL,uL,vL,eL,pL,rhoR,uR,vR,eR,pR,SL,SR)
      implicit none
      
      real, intent(in) :: rhoL,uL,vL,eL,pL,rhoR,uR,vR,eR,pR,SL,SR
      real :: SM,rhoBarre, aBarre, aL, aR, CL, CR
      real, dimension(1:4) :: flux_hllc_y, ULEtoile, UREtoile, flux_L, flux_R, Q, R
      
     
      aL = a(rhoL,pL,gamma)
      aR = a(rhoR,pR,gamma)
      aBarre = 0.5*(aL+aR)
      rhoBarre = 0.5*(rhoL+rhoR)
      !SM = (pR-pL+rhoL*uL*(SL-uL)-rhoR*uR*(SR-uR))/(rhoL*(SL-uL)-rhoR*(SR-uR))
      
      SM = 0.5*(vL+vR)-0.5*(pR-pL)/(rhoBarre*aBarre)
      flux_L = flux_y(rhoL,uL,vL,eL,pL)
      flux_R = flux_y(rhoR,uR,vR,eR,pR)
  
      ! Q(1) = SL*rhoL - flux_L(1)
      ! Q(2) = SL*rhoL*uL - flux_L(2)
      ! Q(3) = SL*rhoL*vL - flux_L(3)
      ! Q(4) = SL*(0.5*rhoL*(uL**2+vL**2)+rhoL*eL) - flux_L(4)
  
      ! R(1) = SR*rhoR - flux_R(1)
      ! R(2) = SR*rhoR*uR - flux_R(2)
      ! R(3) = SR*rhoR*vR - flux_R(3)
      ! R(4) = SR*(0.5*rhoR*(uR**2+vR**2)+rhoR*eR) - flux_R(4)

      CL = rhoL*(SL-vL)/(SL-SM)
      ULEtoile(1) = CL
      ULEtoile(2) = CL*uL
      ULEtoile(3) = CL*SM
      ULEtoile(4) = CL*(0.5*(uL**2+vL**2)+eL + (SM-vL)*(SM + pL/(rhoL*(SL-vL))))
  
      CR = rhoR*(SR-vR)/(SR-SM)
      UREtoile(1) = CR
      UREtoile(2) = CR*uR
      UREtoile(3) = CR*SM
      UREtoile(4) = CR*(0.5*(uR**2+vR**2)+eR + (SM-vR)*(SM + pR/(rhoR*(SR-vR))))
   
  
  
      if ( SL >= 0 ) then
        flux_hllc_y = flux_L
      end if
      if ( SR <= 0 ) then
        flux_hllc_y = flux_R
      end if
      if ( SL <= 0 .AND. SM >= 0 ) then
        flux_hllc_y(1) = flux_L(1) + SL*(ULEtoile(1)-rhoL)
        flux_hllc_y(2) = flux_L(2) + SL*(ULEtoile(2)-rhoL*uL)
        flux_hllc_y(3) = flux_L(3) + SL*(ULEtoile(3)-rhoL*vL)
        flux_hllc_y(4) = flux_L(4) + SL*(ULEtoile(4)-(0.5*rhoL*(uL**2+vL**2)+rhoL*eL))
  
        !print*,"out flux hll", flux_hllc
      end if 
      if ( SM <= 0 .AND. SR >= 0 ) then
        flux_hllc_y(1) = flux_R(1) + SR*(UREtoile(1)-rhoR)
        flux_hllc_y(2) = flux_R(2) + SR*(UREtoile(2)-rhoR*uR)
        flux_hllc_y(3) = flux_R(3) + SR*(UREtoile(3)-rhoR*vR)
        flux_hllc_y(4) = flux_R(4) + SR*(UREtoile(4)-(0.5*rhoR*(uR**2+vR**2) + rhoR*eR))
      end if 
    
      end function flux_hllc_y 

    ! function flux_axe_X(k,n)
    !   implicit none 
    !   integer, intent(in) :: k,n
    !   real :: eL, eR, rhoL, uL, vL, pL, rhoR, uR, vR, pR, aL, aR, rhoBarre, aBarre, SM, CL, CR
    !   real, dimension(1:4) :: flux_axe_X, prim, flux_L, flux_R, ULEtoile, UREtoile

    !   prim = primitive(u1i(k),u2i(k),u3i(k), u4i(k))
    !   rhoL = prim(1)
    !   uL = prim(2)
    !   vL = prim(3)
    !   pL = prim(4)
    !   eL = energie(prim(1),prim(4))
    !   prim = primitive(u1i(k+1),u2i(k+1),u3i(k+1), u4i(k+1))
    !   rhoR = prim(1)
    !   uR = prim(2)
    !   vR = prim(3)
    !   pR = prim(4)
    !   eR = energie(prim(1),prim(4))



    !   ! Flux HLL
    !   if (n==1) then 
    !     flux_L = flux_x(rhoL,uL,vL,eL,pL)
    !     flux_R = flux_x(rhoR,uR,vL,eR,pR)
     
    !     flux_axe_X(1) = (SR*flux_L(1)-SL*flux_R(1)+SL*SR*(u1i(k+1)-u1i(k)))/(SR-SL)
    !     flux_axe_X(2) = (SR*flux_L(2)-SL*flux_R(2)+SL*SR*(u2i(k+1)-u2i(k)))/(SR-SL)
    !     flux_axe_X(3) = (SR*flux_L(3)-SL*flux_R(3)+SL*SR*(u3i(k+1)-u3i(k)))/(SR-SL)
    !     flux_axe_X(4) = (SR*flux_L(4)-SL*flux_R(4)+SL*SR*(u4i(k+1)-u4i(k)))/(SR-SL)
    !   end if 

    !   ! Flux HLLC
    !   if (n==2) then
    !     aL = a(rhoL,pL,gamma)
    !     aR = a(rhoR,pR,gamma)
    !     aBarre = 0.5*(aL+aR)
    !     rhoBarre = 0.5*(rhoL+rhoR)
        
    !     SM = 0.5*(uL+uR)-0.5*(pR-pL)/(rhoBarre*aBarre)
    !     flux_L = flux_x(rhoL,uL,vL,eL,pL)
    !     flux_R = flux_x(rhoR,uR,vR,eR,pR)
  
    
    !     CL = rhoL*(SL-uL)/(SL-SM)
    !     ULEtoile(1) = CL
    !     ULEtoile(2) = CL*SM
    !     ULEtoile(3) = CL*vL
    !     ULEtoile(4) = CL*(0.5*(uL**2+vL**2)+eL + (SM-uL)*(SM + pL/(rhoL*(SL-uL))))
    
    !     CR = rhoR*(SR-uR)/(SR-SM)
    !     UREtoile(1) = CR
    !     UREtoile(2) = CR*SM
    !     UREtoile(3) = CR*vR
    !     UREtoile(4) = CR*(0.5*(uR**2+vR**2)+eR + (SM-uR)*(SM + pR/(rhoR*(SR-uR))))
     
    
    
    !     if ( SL >= 0 ) then
    !       flux_axe_X = flux_L
    !     end if
    !     if ( SR <= 0 ) then
    !       flux_axe_X = flux_R
    !     end if
    !     if ( SL <= 0 .AND. SM >= 0 ) then
    !       flux_axe_X(1) = flux_L(1) + SL*(ULEtoile(1)-u1i(k))
    !       flux_axe_X(2) = flux_L(2) + SL*(ULEtoile(2)-u2i(k))
    !       flux_axe_X(3) = flux_L(3) + SL*(ULEtoile(3)-u3i(k))
    !       flux_axe_X(4) = flux_L(4) + SL*(ULEtoile(4)-u4i(k))
    
    !       !print*,"out flux hll", flux_hllc
    !     end if 
    !     if ( SM <= 0 .AND. SR >= 0 ) then
    !       flux_axe_X(1) = flux_R(1) + SR*(UREtoile(1)-u1i(k+1))
    !       flux_axe_X(2) = flux_R(2) + SR*(UREtoile(2)-u2i(k+1))
    !       flux_axe_X(3) = flux_R(3) + SR*(UREtoile(3)-u3i(k+1))
    !       flux_axe_X(4) = flux_R(4) + SR*(UREtoile(4)-u4i(k+1))
    !     end if 

    !   end if 
    !   end function flux_axe_X

    !   function flux_axe_Y(k,n)
    !     implicit none 
    !     integer, intent(in) :: k,n
    !     real :: eL, eR, rhoL, uL, vL, pL, rhoR, uR, vR, pR, aL, aR, rhoBarre, aBarre, SM, CL, CR
    !     real, dimension(1:4) :: flux_axe_Y, prim, flux_L, flux_R, ULEtoile, UREtoile
  
    !     prim = primitive(u1i(k),u2i(k),u3i(k), u4i(k))
    !     rhoL = prim(1)
    !     uL = prim(2)
    !     vL = prim(3)
    !     pL = prim(4)
    !     eL = energie(prim(1),prim(4))

    !     prim = primitive(u1i(k+N),u2i(k+N),u3i(k+N), u4i(k+N))
    !     rhoR = prim(1)
    !     uR = prim(2)
    !     vR = prim(3)
    !     pR = prim(4)
    !     eR = energie(prim(1),prim(4))
  
    !     ! Flux HLL
    !     if (n==1) then 
    !       flux_L = flux_y(rhoL,uL,vL,eL,pL)
    !       flux_R = flux_y(rhoR,uR,vL,eR,pR)
       
    !       flux_axe_Y(1) = (SR*flux_L(1)-SL*flux_R(1)+SL*SR*(u1i(k+N)-u1i(k)))/(SR-SL)
    !       flux_axe_Y(2) = (SR*flux_L(2)-SL*flux_R(2)+SL*SR*(u2i(k+N)-u2i(k)))/(SR-SL)
    !       flux_axe_Y(3) = (SR*flux_L(3)-SL*flux_R(3)+SL*SR*(u3i(k+N)-u3i(k)))/(SR-SL)
    !       flux_axe_Y(4) = (SR*flux_L(4)-SL*flux_R(4)+SL*SR*(u4i(k+N)-u4i(k)))/(SR-SL)
    !     end if 
  
    !     ! Flux HLLC
    !     if (n==2) then
    !       aL = a(rhoL,pL,gamma)
    !       aR = a(rhoR,pR,gamma)
    !       aBarre = 0.5*(aL+aR)
    !       rhoBarre = 0.5*(rhoL+rhoR)
          
    !       SM = 0.5*(vL+vR)-0.5*(pR-pL)/(rhoBarre*aBarre)
    !       flux_L = flux_y(rhoL,uL,vL,eL,pL)
    !       flux_R = flux_y(rhoR,uR,vR,eR,pR)
      
    
    !       CL = rhoL*(SL-vL)/(SL-SM)
    !       ULEtoile(1) = CL
    !       ULEtoile(2) = CL*uL
    !       ULEtoile(3) = CL*SM
    !       ULEtoile(4) = CL*(0.5*(uL**2+vL**2)+eL + (SM-vL)*(SM + pL/(rhoL*(SL-vL))))
      
    !       CR = rhoR*(SR-vR)/(SR-SM)
    !       UREtoile(1) = CR
    !       UREtoile(2) = CR*uR
    !       UREtoile(3) = CR*SM
    !       UREtoile(4) = CR*(0.5*(uR**2+vR**2)+eR + (SM-vR)*(SM + pR/(rhoR*(SR-vR))))
       
      
      
    !       if ( SL >= 0 ) then
    !         flux_axe_Y = flux_L
    !       end if
    !       if ( SR <= 0 ) then
    !         flux_axe_Y = flux_R
    !       end if
    !       if ( SL <= 0 .AND. SM >= 0 ) then
    !         flux_axe_Y(1) = flux_L(1) + SL*(ULEtoile(1)-rhoL)
    !         flux_axe_Y(2) = flux_L(2) + SL*(ULEtoile(2)-rhoL*uL)
    !         flux_axe_Y(3) = flux_L(3) + SL*(ULEtoile(3)-rhoL*vL)
    !         flux_axe_Y(4) = flux_L(4) + SL*(ULEtoile(4)-(0.5*rhoL*(uL**2+vL**2)+rhoL*eL))
      
    !         !print*,"out flux hll", flux_hllc
    !       end if 
    !       if ( SM <= 0 .AND. SR >= 0 ) then
    !         flux_axe_Y(1) = flux_R(1) + SR*(UREtoile(1)-rhoR)
    !         flux_axe_Y(2) = flux_R(2) + SR*(UREtoile(2)-rhoR*uR)
    !         flux_axe_Y(3) = flux_R(3) + SR*(UREtoile(3)-rhoR*vR)
    !         flux_axe_Y(4) = flux_R(4) + SR*(UREtoile(4)-(0.5*rhoR*(uR**2+vR**2) + rhoR*eR))
    !       end if 
    !     end if 
    !     end function flux_axe_Y







end module mod_flux
