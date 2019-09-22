module forces
    use globals
    implicit none

    private :: interactions,boundaries,friction
    public :: force

    real(dp), private :: boundary_width
    real(dp), private :: boundary_height

contains

subroutine set_boundaries(width, height)
    real(dp), intent(in) :: width, height
    boundary_width = width
    boundary_height = height
end subroutine set_boundaries

subroutine force(x,y,vx,vy,fx,fy, numballs)
    implicit none
    integer                  ,intent(in)  :: numballs
    real(dp), dimension(numballs),intent(in)  :: x,y
    real(dp), dimension(numballs),intent(in)  :: vx,vy
    real(dp), dimension(numballs),intent(out) :: fx,fy

    integer :: i

    do i=1,numballs
        fx(i) = 0_dp
        fy(i) = 0_dp
    end do

    call interactions(x,y,fx,fy,numballs)
    call boundaries(x,y,fx,fy,numballs)
    call friction(vx,vy,fx,fy,numballs)

end subroutine force

function dudr_ball2ball(r2) result(du)
    ! ableitung des Potentials zw. Kugeln
    real(dp), intent(in) :: r2
    real(dp) :: du

    ! 32.49 = kugeldurchmesser**2
    if (r2 > 32.49) then 
        du = 0_dp
    else
        du =  (r2-32.49)**2
    end if
end function dudr_ball2ball

function dudr_table_edge(r2) result(du)
    ! ableitung des Potentials zw. Kugel und Bande
    real(dp), intent(in) :: r2
    real(dp) :: du

    if (r2 > 32.49) then 
        du = 0_dp
    else
        du =  (r2-32.49)**2
    end if
end function dudr_table_edge

subroutine interactions(x,y,fx,fy,numballs)
    implicit none
    integer                  ,intent(in)  :: numballs
    real(dp), dimension(numballs),intent(in)  :: x,y
    real(dp), dimension(numballs),intent(out) :: fx,fy

    integer                               :: i,j
    real(dp) :: dx, dy, r2, fr, fxi, fyi
  
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

subroutine boundaries(x,y,fx,fy, numballs)
    implicit none
    integer                  ,intent(in)  :: numballs
    real(dp), dimension(numballs),intent(in)  :: x,y
    real(dp), dimension(numballs),intent(out) :: fx,fy

    integer                               :: i
    real(dp)                                  :: dx, dy, r2, fr, fxi, fyi

    do i=1,numballs
        ! left
        if (x(i) < 0.1_dp*boundary_width) then
            dx = x(i)
            r2 = dx*dx
            fr = dudr_table_edge(r2)
            fxi = fr * dx
        ! right
        else if (x(i) > 0.9_dp*boundary_width) then
            dx = x(i) - boundary_width
            r2 = dx*dx
            fr = dudr_table_edge(r2)
            fxi = fr * dx
        else 
            fxi = 0_dp
        end if

        ! bottom
        if (y(i) < 0.1_dp*boundary_height) then
            dy = y(i)
            r2 = dy*dy
            fr = dudr_table_edge(r2)
            fyi = fr * dy
        ! top
        else if (y(i) > 0.9_dp*boundary_height) then
            dy = y(i) - boundary_height
            r2 = dy*dy
            fr = dudr_table_edge(r2)
            fyi = fr * dy
        else 
            fyi = 0_dp
        end if

        ! gesamtkraefte
        fx(i) = fx(i) + fxi ! actio
        fy(i) = fy(i) + fyi ! actio
    end do

end subroutine boundaries

subroutine friction(vx,vy,fx,fy,numballs)
    implicit none
    integer                  ,intent(in)  :: numballs
    real(dp), dimension(numballs),intent(in)  :: vx,vy
    real(dp), dimension(numballs),intent(out) :: fx,fy

    integer :: i
    real(dp),parameter :: v0 = 0.001 ! velocity to consider zero for friction
    real(dp) :: fxi, fyi


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

end module forces