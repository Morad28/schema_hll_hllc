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
        r = (UC-UL)/(UR-UC)

        ! Minmod
        limiteur = minmod1D(1.,r)


    end function limiteur 


    function reconstructionL(UL,UC,UR)
        real, intent(in) :: UL,UC,UR
        real :: reconstructionL 

        reconstructionL = UC + 0.5 * limiteur(UL,UC,UR) * (UR-UC)

    end function reconstructionL

    function reconstructionR(UL,UC,UR)
        real, intent(in) :: UL,UC,UR
        real :: reconstructionR 

        reconstructionR = UC - 0.5 * limiteur(UL,UC,UR) * (UR-UC)

    end function reconstructionR

    function minmod(u1,u2,u3)
        implicit None

        real, dimension(1:4),intent(in) :: u1,u2,u3
        real, dimension(1:4) :: minmod
        integer :: i 

        do i = 1, 4 
            minmod(i) = 0

            if (u1(i)>0 .AND. u2(i)>0 .AND. u3(i)>0) then
                minmod(i) = min(u1(i),u2(i),u3(i))
            end if
            
            if (u1(i)<0 .AND. u2(i)<0 .AND. u3(i)<0) then
               minmod(i) = max(u1(i),u2(i),u3(i))
            endif
        end do 
    end function minmod

    function polij(UCentre,Ugauche,Udroite,Ubas,Uhaut,x,y,xi,yj)
        implicit none
        real, intent(in) :: x,y,xi,yj
        real, dimension(1:4), intent(in) :: UCentre,Ugauche,Udroite,Ubas,Uhaut
        real, dimension(1:4) :: polij, Ux, Uy
        real :: C
        !print*, Ucentre(1)
        C=theta/dx

        Ux = minmod(C*(Udroite-Ucentre),(Udroite-Ugauche)/(2*dx),C*(Ucentre-Ugauche))
        Uy = minmod(C*(Uhaut-Ucentre),(Uhaut-Ubas)/(2*dx),C*(Ucentre-Ubas))
        
        polij = Ucentre + (Ux*(x-xi)+Uy*(y-yj))
        !print*, (Ux*(x-xi)+Uy*(y-yj)), Ucentre
    end function polij

    function Uk(u1,u2,u3,u4)
        implicit none 
        real, intent(in) :: u1,u2,u3,u4
        real, dimension(1:4) :: Uk

        Uk = (/u1,u2,u3,u4/)

    end function Uk

    function get_U(x,y,xi,yj,UCentre,Ugauche,Udroite,Ubas,Uhaut)
        implicit none
        real, intent(in) :: x,y,xi,yj
        real, intent(in), dimension(1:4) :: UCentre,Ugauche,Udroite,Ubas,Uhaut
        real, dimension(1:4) :: get_U
        
        get_U = polij( UCentre , Ugauche ,Udroite , Ubas , Uhaut , x , y , xi , yj )
        
    end function get_U
end module mod_ordreSup