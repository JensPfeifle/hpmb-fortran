module initial
    use globals
    implicit none

contains

subroutine setupinitial(x,y,vx,vy)
    real, dimension(numballs),intent(out)  :: x,y
    real, dimension(numballs),intent(out)  :: vx,vy

    integer :: i
    integer :: j,k
    real    :: xi,yi
    real,parameter    :: sx = 5.7 ! spacing in xi
    real,parameter    :: sy = 4.9363448 ! spacing in yi ,sy

    ! cue ball
    i=1
    x(i) = 50.
    y(i) = 25.
    vx(i) = 0.
    vy(i) = 100.

    ! setup triangle with others
    i = 2
    do j=1,5
        yi = 150.+(j-1)*sy
        xi = 50.-(j-1)*sx/2.-sx
        do k=1,j
            xi = xi + sx
            x(i) = xi
            y(i) = yi
            vx(i) = 0.
            vy(i) = 0.
            i=i+1
        end do
    end do

end subroutine setupinitial

end module
