module forces
    use globals
    use omp_lib
    implicit none

    public  :: set_boundaries, force
    private :: boundaries,friction
    private :: interactions_naive,interactions_cll

    real(dp), private :: boundary_width
    real(dp), private :: boundary_height


contains


subroutine force(x,y,vx,vy,fx,fy)
    implicit none
    real(dp), dimension(:),intent(in)  :: x,y
    real(dp), dimension(:),intent(in)  :: vx,vy
    real(dp), dimension(:),intent(out) :: fx,fy

    fx = 0_dp
    fy = 0_dp

    call interactions_cll(x,y,fx,fy)
    !call interactions_naive(x,y,fx,fy)
    call boundaries(x,y,fx,fy)
    call friction(vx,vy,fx,fy)

end subroutine force


function dudr_ball2ball(r2) result(du)
    ! ableitung des Potentials zw. Kugeln
    real(dp), intent(in) :: r2
    real(dp) :: du,diam2

    diam2 = (RADIUS*2)**2 ! kugeldurchmesser**2
    if (r2 > diam2) then
        du = 0_dp
    else
        du =  (r2-diam2)**2
    end if
end function dudr_ball2ball


function dudr_table_edge(r2) result(du)
    ! ableitung des Potentials zw. Kugel und Bande
    real(dp), intent(in) :: r2
    real(dp) :: du,diam2

    diam2 = (RADIUS*2)**2 ! kugeldurchmesser**2
    if (r2 > diam2) then
        du = 0_dp
    else
        du =  (r2-diam2)**2
    end if
end function dudr_table_edge


subroutine interactions_naive(x,y,fx,fy)
    ! adds interactions (ball-ball) components to force arrays
    ! naive (each-with-rest) approach
    implicit none
    real(dp), dimension(:),intent(in)       :: x,y
    real(dp), dimension(:),intent(inout)    :: fx,fy

    integer        :: i,j,numballs
    real(dp)       :: dx, dy, r2, fr, fxi, fyi

    numballs = size(x)
    ! ball-to-ball forces
    do i=1,numballs-1
        do j=i+1,numballs
            ! ball-to-ball forces
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

end subroutine interactions_naive

subroutine interactions_cll(x,y, fx,fy)
    ! adds interactions (ball-ball) components to force arrays
    ! linked-cell approach
    implicit none
    real(dp), dimension(:),intent(in)    :: x,y
    real(dp), dimension(:),intent(inout) :: fx,fy

    integer                              :: ncellsx, ncellsy, ncells
    real(dp)                             :: dx,dy
    integer                              :: n,m,numballs
    integer                              :: i,j,l,k,lmin,lmax,kmin,kmax
    integer, allocatable, dimension(:,:) :: first
    integer, allocatable, dimension(:)   :: next
    integer, allocatable, dimension(:,:) :: last
    real(dp)                             :: rx, ry, r2, fr, fxi, fyi

    numballs = size(x)
    ! determine number and size of cells
    ncellsx = max(floor(boundary_width/RCUT), 1)
    ncellsy = max(floor(boundary_height/RCUT), 1)
    ncells = ncellsx*ncellsy
    allocate(first(ncellsx,ncellsy), &
             next(numballs),         &
             last(ncellsx,ncellsy))
    first = -1
    next = -1
    dx = boundary_width / real(ncellsx, dp)
    dy = boundary_height/ real(ncellsy, dp)

    ! fill lists
    do n=1,numballs
        ! determine cell number
        i = floor(x(n)/dx)+1
        j = floor(y(n)/dy)+1
        if (first(i,j) == -1) then
            first(i,j) = n
        else
            next(last(i,j)) = n
        end if
        last(i,j) = n
    end do
    
    ! set to true for verbose cell list output
    if (.false.) then
        write(*,*) "first"
        100 format(39(i3))
        do j=ncellsy,1,-1
                write(*,100) first(:,j)
        end do
        write(*,*) "next"
        write(*,100)  next
        write(*,*) "last"
        do j=ncellsy,1,-1
                write(*,100) last(:,j)
        end do
    end if

    deallocate(last) ! dont need this anymore

    call omp_set_nested(.false.)
    !$omp parallel do default(private) shared(x,y,fx,fy,first,next,ncellsx,ncellsy)
    do j=1,ncellsy
        !write(*,'(a,3(i4))') "thread", omp_get_thread_num(), j
        do i=1,ncellsx
            kmin = max(i-1,1)
            kmax = min(i+1,ncellsx)
            lmin = max(j-1,1)
            lmax = min(j+1,ncellsy)
            do l=lmin,lmax
                do k=kmin,kmax
                    ! ball n in this cell
                    n = first(i,j)
                    do while (n .NE. -1)
                     ! ball m in other cell
                        m = first(k,l)
                        do while (m .NE. -1)
                            if (n < m) then ! no double counting of pair (n, m)
                                ! ball-to-ball forces
                                rx = x(n) - x(m)
                                ry = y(n) - y(m)
                                r2 = rx*rx + ry*ry
                                ! anteilige kraft
                                fr = dudr_ball2ball(r2)
                                fxi = fr * rx
                                fyi = fr * ry
                                ! gesamtkraefte
                                fx(n) = fx(n) + fxi ! actio
                                fx(m) = fx(m) - fxi ! reactio
                                fy(n) = fy(n) + fyi ! actio
                                fy(m) = fy(m) - fyi ! reactio
                            end if
                        m = next(m)
                        end do
                    n = next(n)
                    end do
                end do
            end do
        end do
    end do
    !$omp end parallel do

end subroutine interactions_cll

subroutine set_boundaries(width, height)
    ! sets up boundary size
    implicit none
    real(dp), intent(in) :: width, height
    boundary_width = width
    boundary_height = height
end subroutine set_boundaries

subroutine boundaries(x,y,fx,fy)
    ! adds boundary components to force arrays
    implicit none
    real(dp), dimension(:),intent(in)         :: x,y
    real(dp), dimension(:),intent(inout)      :: fx,fy

    integer                                   :: i,numballs
    real(dp)                                  :: dx, dy, r2, fr, fxi, fyi

    numballs = size(x)
    do i=1,numballs
        ! left
        if (x(i) < RCUT) then
            dx = x(i)
            r2 = dx*dx
            fr = dudr_table_edge(r2)
            fxi = fr * dx
        ! right
        else if (x(i) > boundary_width-RCUT) then
            dx = x(i) - boundary_width
            r2 = dx*dx
            fr = dudr_table_edge(r2)
            fxi = fr * dx
        else
            fxi = 0_dp
        end if

        ! bottom
        if (y(i) < RCUT) then
            dy = y(i)
            r2 = dy*dy
            fr = dudr_table_edge(r2)
            fyi = fr * dy
        ! top
        else if (y(i) > boundary_height-RCUT) then
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

subroutine friction(vx,vy,fx,fy)
    ! adds frictional components to force arrays
    implicit none
    real(dp), dimension(:),intent(in)  :: vx,vy
    real(dp), dimension(:),intent(inout) :: fx,fy

    integer :: i,numballs
    real(dp),parameter :: v0 = 0.001 ! velocity to consider zero for friction
    real(dp) :: fxi, fyi

    numballs = size(vx)
    do i=1,numballs
        if (vx(i) > v0) then
            fxi = -MU*M*G
        else if (vx(i) < -v0) then
            fxi = MU*M*G
        else
            fxi = 0
        end if

        if (vy(i) > v0) then
            fyi = -MU*M*G
        else if (vy(i) < -v0) then
            fyi = MU*M*G
        else
            fyi = 0
        end if

        ! gesamtkraefte
        fx(i) = fx(i) + fxi
        fy(i) = fy(i) + fyi
    end do

end subroutine friction

end module forces