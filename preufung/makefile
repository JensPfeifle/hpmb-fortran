#Programmname
PROG =  main
#Liste der Quelldateien
SRCS =  main.f90 globals.f90 initial.f90 forces.f90
#Liste der Objektdateien
OBJS =  $(SRCS:.f90=.o)   
#Name des Compilers
F90 = gfortran

$(PROG): $(OBJS)
	$(F90) -o $@ $(OBJS) $(LIBS) 
clean:
	rm -f $(PROG) *.mod *.o

.SUFFIXES: $(SUFFIXES) .f90

%.o:%.f90
	$(F90) $(FLAGS) -c $<

globals.o: globals.f90
forces.o: forces.f90 globals.o
initial.o: initial.f90 globals.o
main.o: main.f90 globals.o initial.o forces.o
