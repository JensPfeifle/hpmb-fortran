module integration
    use globals
    implicit none

    public init_ver
    public positions_ver
    public velocities_ver
    public positions_eul
    public velocities_eul

    real(dp), private, allocatable, dimension(:) :: fxold, fyold
    
    
contains


subroutine init_ver(nballs)
    implicit none
    integer, intent(in) :: nballs

    allocate(fxold(nballs),fyold(nballs))
    fxold = 0_dp
    fyold = 0_dp

end subroutine init_ver

subroutine positions_eul(x,y,vx,vy,dt)
    implicit none
    real(dp), intent(in)                :: dt
    real(dp), dimension(:), intent(in)  :: vx,vy
    real(dp), dimension(:), intent(out)  :: x,y

    integer                             :: i,numballs

    numballs = size(vx)
    ! bewege atome
    do i=1,numballs
        x(i) = x(i) + vx(i)*dt
        y(i) = y(i) + vy(i)*dt
    end do

end subroutine positions_eul

subroutine velocities_eul(vx,vy,fx,fy,dt)
    implicit none
    real(dp), intent(in)                :: dt
    real(dp), dimension(:), intent(in) :: fx,fy
    real(dp), dimension(:), intent(out)  :: vx,vy

    integer                             :: i,numballs

    numballs = size(fx)
    ! aktualisiere geschwindigkeiten
    do i=1,numballs
        vx(i) = vx(i) + fx(i)/m*dt*0.5_dp
        vy(i) = vy(i) + fy(i)/m*dt*0.5_dp
    end do

end subroutine velocities_eul

subroutine positions_ver(x,y,vx,vy,fx,fy,dt)
    implicit none
    real(dp), intent(in)                :: dt
    real(dp), dimension(:), intent(in)  :: vx,vy
    real(dp), dimension(:), intent(in)  :: fx,fy
    real(dp), dimension(:), intent(out)  :: x,y

    integer                             :: i,numballs

    numballs = size(fx)
    ! bewege atome
    do i=1,numballs
        x(i) = x(i) + vx(i)*dt + fx(i)/m*dt*dt*0.5_dp
        y(i) = y(i) + vy(i)*dt + fy(i)/m*dt*dt*0.5_dp
    end do

end subroutine positions_ver

subroutine velocities_ver(vx,vy,fx,fy,dt)
    implicit none
    real(dp), intent(in)                :: dt
    real(dp), dimension(:), intent(in) :: fx,fy
    real(dp), dimension(:), intent(out)  :: vx,vy

    integer                             :: i,numballs

    numballs = size(fx)
    ! aktualisiere geschwindigkeiten
    do i=1,numballs
        vx(i) = vx(i) + dt*0.5_dp/m*(fx(i) + fxold(i))
        vy(i) = vy(i) + dt*0.5_dp/m*(fy(i) + fyold(i))
        ! kraefte speichern fuer naechstes mal
        fxold(i) = fx(i)
        fyold(i) = fy(i)
    end do

end subroutine velocities_ver


end module integration

