#Programmname
PROG =  main
#Liste der Quelldateien
SRCS =  main.f90 globals.f90 initial.f90 forces.f90 integration.f90
#Liste der Objektdateien
OBJS =  $(SRCS:.f90=.o)   
#Name des Compilers
F90 = gfortran
#FLAGS = -O0  -g -fimplicit-none  -Wall  -Wline-truncation  -Wcharacter-truncation  -Wsurprising  -Waliasing  -Wimplicit-interface  -Wunused-parameter  -fwhole-file  -fcheck=all  -std=f2008  -pedantic  -fbacktrace
#FLAGS = -O0  -g -fopenmp -fimplicit-none  -Wall  -Wline-truncation  -Wcharacter-truncation  -Wsurprising  -Waliasing  -Wimplicit-interface  -Wunused-parameter  -fwhole-file  -fcheck=all  -std=f2008  -pedantic  -fbacktrace
FLAGS = -O3 -fopenmp -fimplicit-none

$(PROG): $(OBJS)
	$(F90) $(FLAGS) -o $@ $(OBJS) $(LIBS) 
clean:
	rm -f $(PROG) *.mod *.o

.SUFFIXES: $(SUFFIXES) .f90

%.o:%.f90
	$(F90) $(FLAGS) -c $<

globals.o: globals.f90
forces.o: forces.f90 globals.o
initial.o: initial.f90 forces.o globals.o
integration.o: integration.f90 globals.o
main.o: main.f90 globals.o initial.o forces.o integration.o
