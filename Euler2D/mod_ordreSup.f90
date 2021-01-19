module mod_ordreSup
    use mod_constante
    use mod_CI
    implicit none
contains
    function minmod(u1,u2,u3)
        implicit None

        real, dimension(1:4),intent(in) :: u1,u2,u3
        real, dimension(1:4) :: minmod
        integer :: i 

        do i = 1, 4 
            if (u1(i)>0 .AND. u2(i)>0 .AND. u3(i)>0) then
                minmod(i) = min(u1(i),u2(i),u3(i))
            
            else if (u1(i)<0 .AND. u2(i)<0 .AND. u3(i)<0) then
               minmod(i) = max(u1(i),u2(i),u3(i))
            else 
                minmod(i) = 0
            endif
        end do 
    end function minmod

    function polij(UCentre,Ugauche,Udroite,Ubas,Uhaut,x,y,xi,yj)
        implicit none
        real, intent(in) :: x,y,xi,yj
        real, dimension(1:4), intent(in) :: UCentre,Ugauche,Udroite,Ubas,Uhaut
        real, dimension(1:4) :: polij, Ux, Uy
        real :: C
        !print*, 'pol', dx
        C=theta/dx
        Ux = minmod(C*(Udroite-Ucentre),(Udroite-Ugauche)/(2*dx),C*(Ucentre-Ugauche))
        Uy = minmod(C*(Uhaut-Ucentre),(Uhaut-Ubas)/(2*dx),C*(Ucentre-Ubas))

        polij = Ucentre + (Ux*(x-xi)+Uy*(y-yj))

    end function polij

    function Uk(k)
        implicit none 
        integer, intent(in) :: k
        real, dimension(1:4) :: Uk

        Uk(1) = u1i(k)
        Uk(2) = u2i(k)
        Uk(3) = u3i(k)
        Uk(4) = u4i(k)

    end function Uk

    function get_U(id,jd,xi,yj)
        implicit none
        real, intent(in) :: xi,yj 
        integer, intent(in) :: id,jd
        real, dimension(1:4) :: get_U
        integer :: k
        k = id+(jd-1)*N
        
        get_U = polij(Uk(k),Uk(k-1),Uk(k+1),Uk(k-N),Uk(k+N),xi,yj,id*dx,jd*dx)

    end function get_U
end module mod_ordreSup