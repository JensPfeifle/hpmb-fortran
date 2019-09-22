module globals
    implicit none

    integer, parameter :: dp=kind(1.d0)
    
    real, protected     :: g = 980.665 ! [cm/s2]
    real, protected     :: m = 0.17 ! mass of balls [kg]
    real, protected     :: mu = 0.008 ! rolling resistance

end module globals