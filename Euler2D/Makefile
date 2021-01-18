FC=mpif90
FCFLAGS=-O2 -Wall 

SRCS= mod_CI.f90 mod_ordreSup.f90 mod_flux.f90 mod_constante.f90 mod_celerite.f90 mod_energie.f90 mod_conservatif_primitif.f90 main.f90
PROGRAM= run
OBJ=$(SRCS: .F90= .o)

$(PROGRAM):$(OBJ)
	$(FC) $(FCFLAGS) -o $@ $^ 

%.o: %.f90
	$(FC) $(FCFLAGS) -c $<
clean:
	rm -f *.o *.mod fort.* *.exe run
