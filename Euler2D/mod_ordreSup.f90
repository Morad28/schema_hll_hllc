module mod_ordreSup
    use mod_constante
    use mod_CI
    implicit none
contains

    function minmod1D(a,b)
        real, intent(in) :: a, b 
        real :: minmod1D
        minmod1D = 0

        if (a>0 .AND. b>0) then
            minmod1D = min(a,b)
        end if
        
        if (a<0 .AND. b<0 ) then
            minmod1D = max(a,b)
        endif

        end function minmod1D


    function limiteur(UL,UC,UR)
        real, intent(in) :: UL,UC,UR
        real :: r
        real :: limiteur 
        
        if (UR == UC) then
            if (UL == UC) then
                r = 1
                limiteur = 1
            else
                limiteur = 1
            end if
        else

            r = (UC-UL)/(UR-UC)
            !limiteur = (r+abs(r))/(1+abs(r))
            !limiteur = max(0.,min(1.,r))
            !limiteur = max(0.,min(1.,2*r),min(2.,r))
        end if 

    end function limiteur 

    function lim(r)
        real, intent(in) :: r 
        real :: lim 
    
        lim = max(0.,min(1.,r))
        !lim = max(0.,min(1.,2*r),min(2.,r))
        !lim = min(2.,(r+abs(r))/(1+abs(r)))
    end function lim 


    function reconstructionL(UL,UC,UR)
        real, intent(in) :: UL,UC,UR ! i-1, i, i+1
        real :: reconstructionL, r, i_r 
        ! if (UR == UC) then 
        !     if (UC == UL) then 
        !         ir =1
        !     else
        !         ir = (UC-UL)/(UR-UC)
        !     end if
        ! else
            r = (UC-UL)/(UR-UC)
            
            i_r = (UR-UC)/(UC-UL)
        !end if
        
            
        reconstructionL = UC + 0.5 * lim(r) * (UR-UC)

        !reconstructionL = UC + 0.5 * lim(i_r) * (UC-UL)
      


    end function reconstructionL

    function reconstructionR(UL,UC,UR)
        real, intent(in) :: UL,UC,UR
        real :: reconstructionR, r, i_r

        ! if (UR == UC) then 
        !     if (UC == UL) then 
        !         ir = 1
        !     else
        !         ir = (UC-UL)/(UR-UC)
        !     end if
        ! else
            r = (UC-UL)/(UR-UC)
            i_r = (UR-UC)/(UC-UL)
        ! end if


        reconstructionR = UC - 0.5 * lim(r) *(UR-UC)

        !reconstructionR = UC - 0.5 * lim(i_r) *(UR-UC)


    end function reconstructionR




end module mod_ordreSup