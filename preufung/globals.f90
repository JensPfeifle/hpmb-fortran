module globals
    implicit none

    integer, parameter :: dp=kind(1.d0)
    
    real(dp), protected     :: G = 980.665 ! [cm/s2]
    real(dp), protected     :: RADIUS = 2.85 ! radius of balls
    real(dp), protected     :: RCUT = 2*2.85 ! cutoff for interactions
    real(dp), protected     :: M = 0.17 ! mass of balls [kg]
    real(dp), protected     :: MU = 0.01 ! rolling resistance

end module globals