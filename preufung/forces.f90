module forces
    use globals
    implicit none

private :: interactions,boundaries,friction
public :: force

contains

subroutine force(x,y,vx,vy,fx,fy, numballs)
    implicit none
    integer                  ,intent(in)  :: numballs
    real, dimension(numballs),intent(in)  :: x,y
    real, dimension(numballs),intent(in)  :: vx,vy
    real, dimension(numballs),intent(out) :: fx,fy

    integer :: i

    do i=1,numballs
        fx(i) = 0.
        fy(i) = 0.
    end do

    call interactions(x,y,fx,fy,numballs)
    call boundaries(x,y,fx,fy,numballs)
    call friction(vx,vy,fx,fy,numballs)

end subroutine force

subroutine friction(vx,vy,fx,fy,numballs)
    implicit none
    integer                  ,intent(in)  :: numballs
    real, dimension(numballs),intent(in)  :: vx,vy
    real, dimension(numballs),intent(out) :: fx,fy

    integer :: i
    real,parameter :: v0 = 0.001 ! velocity to consider zero for friction
    real :: fxi, fyi


    do i=1,numballs
        if (vx(i) > v0) then
            fxi = -mu*m*g        
        else if (vx(i) < -v0) then
            fxi = mu*m*g
        else
            fxi = 0     
        end if

        if (vy(i) > v0) then
            fyi = -mu*m*g        
        else if (vy(i) < -v0) then
            fyi = mu*m*g
        else
            fyi = 0
        end if

        ! gesamtkraefte
        fx(i) = fx(i) + fxi
        fy(i) = fy(i) + fyi
    end do

end subroutine friction


subroutine interactions(x,y,fx,fy,numballs)
    implicit none
    integer                  ,intent(in)  :: numballs
    real, dimension(numballs),intent(in)  :: x,y
    real, dimension(numballs),intent(out) :: fx,fy

    integer                               :: i,j
    real :: dx, dy, r2, fr, fxi, fyi

    ! fixme check r2 > r2cut
    
    ! ball-to-ball forces
    do i=1,numballs-1
        do j=i+1,numballs
            dx = x(i) - x(j)
            dy = y(i) - y(j)
            r2 = dx*dx + dy*dy
            ! anteilige kraft
            fr = dudr_ball2ball(r2)
            fxi = fr * dx
            fyi = fr * dy
            ! gesamtkraefte
            fx(i) = fx(i) + fxi ! actio
            fx(j) = fx(j) - fxi ! reactio
            fy(i) = fy(i) + fyi ! actio
            fy(j) = fy(j) - fyi ! reactio
        end do
    end do

end subroutine interactions

function dudr_ball2ball(r2) result(du)
    real, intent(in) :: r2
    real :: du

    ! 32.49 = kugeldurchmesser**2
    if (r2 > 32.49) then 
        du = 0.
    else
        du =  (r2-32.49)**2
    end if
end function dudr_ball2ball

subroutine boundaries(x,y,fx,fy, numballs)
    implicit none
    integer                  ,intent(in)  :: numballs
    real, dimension(numballs),intent(in)  :: x,y
    real, dimension(numballs),intent(out) :: fx,fy

    integer                               :: i
    real :: dx, dy, r2, fr, fxi, fyi

    ! fixme hardcoded table size

    do i=1,numballs
        ! left
        if (x(i) < 15.) then
            dx = x(i)
            r2 = dx*dx
            fr = dudr_table_edge(r2)
            fxi = fr * dx
        ! right
        else if (x(i) > 90.) then
            dx = x(i) - 111.76
            r2 = dx*dx
            fr = dudr_table_edge(r2)
            fxi = fr * dx
        else 
            fxi = 0.
        end if

        ! top
        if (y(i) < 15.) then
            dy = y(i)
            r2 = dy*dy
            fr = dudr_table_edge(r2)
            fyi = fr * dy
        ! bottom
        else if (y(i) > 200.) then
            dy = y(i) - 223.52
            r2 = dy*dy
            fr = dudr_table_edge(r2)
            fyi = fr * dy
        else 
            fyi = 0.
        end if

        ! gesamtkraefte
        fx(i) = fx(i) + fxi ! actio
        fy(i) = fy(i) + fyi ! actio
    end do

end subroutine boundaries

function dudr_table_edge(r2) result(du)
    real, intent(in) :: r2
    real :: du

    ! 8.1225 = kugelradius**2
    if (r2 > 32.49) then 
        du = 0.
    else
        du =  (r2-32.49)**2
    end if
end function dudr_table_edge

end module forces