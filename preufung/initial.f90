module initial
    use globals
    implicit none

    private :: calc_numrows
    public :: set_nballs, initial_setup
    integer, private :: num_balls
    integer, private :: num_rows
    real(dp), private :: queue_ball_vel = 100.

contains

subroutine set_nballs(nballs)
    integer, intent(in) :: nballs
    num_balls = nballs 
    num_rows = calc_numrows(nballs)
end subroutine set_nballs

subroutine set_queue_vel(qvel)
    real(dp), intent(in) :: qvel
    queue_ball_vel = qvel 
end subroutine set_queue_vel

function calc_numrows(nballs) result(nrows)
    ! aus anzahl kugeln die anzahl reihen bestimmen
    integer, intent(in) :: nballs
    integer             :: nrows
    integer             :: sum = 0

    nrows = 1
    do while (sum < nballs)
        sum = sum + nrows ! nballs in row = row number
        nrows = nrows + 1
    end do
end function calc_numrows

subroutine initial_setup(x,y,vx,vy)
    real(dp), dimension(num_balls),intent(out)  :: x,y
    real(dp), dimension(num_balls),intent(out)  :: vx,vy

    integer :: i
    integer :: j,k
    real(dp)    :: xi,yi
    real(dp),parameter    :: sx = 5.7 ! spacing in xi
    real(dp),parameter    :: sy = 4.9363448 ! spacing in yi

    ! cue ball
    i=1
    x(i) = 50.
    y(i) = 25.
    vx(i) = 0.
    vy(i) = queue_ball_vel

    ! setup triangle with others
    i = 2
    do j=1,num_rows
        yi = 150.+(j-1)*sy
        xi = 50.-(j-1)*sx*0.5_dp-sx
        do k=1,j
            if (i > num_balls) then
                exit
            endif
            xi = xi + sx
            x(i) = xi
            y(i) = yi
            vx(i) = 0.
            vy(i) = 0.
            i=i+1
        end do
    end do

end subroutine initial_setup

end module
