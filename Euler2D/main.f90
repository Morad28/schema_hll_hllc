program prog
    !============================
      Use mod_flux
      use mod_constante
      use mod_conservatif_primitif
      use mod_energie
      use mod_celerite
      use mod_CI
      use mod_ordreSup
      use mod_update
    !===========================
      Implicit None

      Integer :: j, k, i ,m ,l=10, O 
      Character*4 :: flux
      Character*21 :: name

    !=====

      O = 1 ! 1 pour ordre 1 ou 2 pour ordre 2
      flux = 'HLLC'

    !===== Pas du maillage 
      dx = 1./(N)

    !==== Choix du cas test 
      call Cas_test( 7 )

    !==== Conditions initiales t = 0
      call Initialisation( u1i , u2i , u3i , u4i )

      call file_rename( l , name )
      call write


      m=0
    !====== BOUCLE PRINCIPALE ========
      
    if (O == 1) then
      !====== Ordre 1 ========
      Call Update(flux)
    end if 
    !====== Ordre 2 ========
      !Call Update_2

     
    !====== Ecriture =======
      ! if (m>14) then
      !   m=0
      !   l=l+1
      !   call file_rename(l,name)
      !   !print*, name 
      !   call write
      ! end if
      ! m=m+1

    l=l+1  
    call file_rename(l,name)
    call write
    call write1Dx
    call write1Dy
      
    contains

    ! ======================== RENAME ========================
    Subroutine file_rename(Me,name)
      Implicit None
      Integer :: Me
      Character*21 ::name
      Character*2 :: tn
      Integer :: i1,i2,i3
      i2 =( Me - 100*i1)/10
      i3 = Me - 100*i1 -10*i2
      tn = char(i2+48)//char(i3+48)
      name='output/sol'//tn//'.dat'
    End Subroutine file_rename
    ! ======================== WRITE ========================
    subroutine write
      implicit None 
    open(unit=4,file=name)
    Do j=1,N
      Do i=1,N
        k = i + (j-1)*N
         write(4,*) i*dx, j*dx , primitive(u1i(k), u2i(k), u3i(k), u4i(k))
         if (i==N) write(4,*)
      EndDo   
    Enddo 
    close(4)
    end subroutine write

    subroutine write1Dx
      implicit None 
      open(unit=5,file='output/out1Dx.dat')
      Do j=1,N
        Do i=1,N
          k = i + (j-1)*N
          if (j == int(N/2)) then 
            write(5,*) i*dx, j*dx , primitive(u1i(k), u2i(k), u3i(k), u4i(k))
          end if
        EndDo   
      Enddo 
      close(5)
    end subroutine write1Dx

    subroutine write1Dy
      implicit None 
      open(unit=6,file='output/out1Dy.dat')
      Do j=1,N
        Do i=1,N
          k = i + (j-1)*N
          if (i == int(N/2)) then 
            write(6,*) i*dx, j*dx , primitive(u1i(k), u2i(k), u3i(k), u4i(k))
          end if
        EndDo   
      Enddo 
      close(6)
    end subroutine write1Dy
 
    end program prog
