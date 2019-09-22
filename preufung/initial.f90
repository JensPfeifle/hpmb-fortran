module initial
    use globals
    use forces
    implicit none

    private :: calc_numrows
    public :: initial_setup

contains

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

subroutine initial_setup(nballs, qvel, offcenter, width, height, x,y,vx,vy)
    implicit none
    integer, intent(in)                 :: nballs
    real(dp), intent(in)                :: qvel,offcenter,width,height
    real(dp), dimension(:),intent(out)  :: x,y
    real(dp), dimension(:),intent(out)  :: vx,vy

    integer            :: i,j,k
    integer            :: num_rows
    real(dp)           :: xi,yi
    real(dp)           :: center
    real(dp)           :: sx ! 5.7 spacing in xi
    real(dp)           :: sy  ! spacing in yi
    real(dp),parameter :: PI = 4.D0*DATAN(1.D0)

    ! set boundaries in for forces module
    call set_boundaries(width, height)

    ! spacing and center
    sx = 2*RADIUS
    sy = 2*RADIUS*sin(1._dp/3._dp*PI)
    center = width*0.5_dp

    ! cue ball
    i=1
    x(i) = center + offcenter
    y(i) = 25.
    vx(i) = 0.
    vy(i) = qvel

    ! setup triangle with others
    i = 2
    num_rows = calc_numrows(nballs)
    do j=1,num_rows
        yi = 150.+(j-1)*sy
        xi = center-(j-1)*sx*0.5_dp-sx
        do k=1,j
            if (i > nballs) then
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
