module mod_ordreSup
    use mod_constante
    !real :: UN,US,UE,UW
    implicit none
contains
    function minmod(u1,u2,u3)
        implicit None

        real, intent(in) :: u1,u2,u3
        real :: minmod

        minmod = 0
        if (u1>0 .AND. u2>0 .AND. u3>0) then
            minmod = min(u1,u2,u3)
        endif
        if (u1<0 .AND. u2<0 .AND. u3<0) then
            minmod = max(u1,u2,u3)
        endif
    end function minmod

    function polij(UCentre,Ugauche,Udroite,Ubas,Uhaut,x,y,xi,yj)
        implicit none
        real, intent(in) :: UCentre,Ugauche,Udroite,Ubas,Uhaut,x,y,xi,yj
        real :: polij, Ux, Uy, C
        C=theta/dx
        Ux = minmod(C*(Udroite-Ucentre),(Udroite-Ugauche)/(2*dx),C*(Ucentre-Ugauche))
        Uy = minmod(C*(Uhaut-Ucentre),(Uhaut-Ubas)/(2*dx),C*(Ucentre-Ubas))

        polij = Ucentre + Ux*(x-xi)+Uy*(y-yj)
    end function polij

    subroutine Ordre(n)
        implicit None 
        integer, intent(in) ::  n
        real :: UCentre,Ugauche,Udroite,Ubas,Uhaut
        if ( n == 1 ) then
        
        end if    
        if ( n == 2 ) then 
        
        
        end if
    end subroutine Ordre

        


end module mod_ordreSup