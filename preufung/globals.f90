module globals
    implicit none
    
    real, parameter     :: g = 980.665 ! [cm/s2]
    integer, parameter  :: numballs = 16 ! including cue ball
    real, parameter     :: m = 0.17 ! mass of balls [kg]
    real, parameter      :: mu = 0.005 ! rolling resistance
    real,parameter      :: dt = 0.00001 ! time increment



end module globals