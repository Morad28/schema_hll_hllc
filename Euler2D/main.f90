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

      Integer :: j, k, i  ,l=10, Ordre, Cas 
      Character*4 :: flux
      Character*21 :: name

    !===== Parametres ======

      Ordre = 1 ! 1 pour ordre 1 ou 2 pour ordre 2
      flux = 'HLLC' ! Choix du flux 
      Cas = 6 ! Cas test

    !======================
 
      dx = 1./N
      call Cas_test( Cas )

    !======== CI ==========
      call Initialisation( u1i , u2i , u3i , u4i )

      call file_rename( l , name )
      call write(name)

    !====== Resolution ========
    
    !====== Ordre 1 ========
    if (Ordre == 1) then
      Call Update(flux, name, l)
    end if 
    !====== Ordre 2 ========
    if (Ordre == 2) then
      Call Update_2(flux)
    end if

    !===== Ecriture =======
    !call file_rename(l,name)
    call write('output/sortie_sim.dat')
    call write1Dx
    call write1Dy
     

    end program prog