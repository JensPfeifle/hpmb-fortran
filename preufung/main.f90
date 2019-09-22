program main
    use globals
    use initial
    use forces
    use integration
    implicit none

    real(dp), allocatable, dimension(:) :: x,y,vx,vy,fx,fy
    real(dp)                            :: t, dt, t_total
    real(dp)                            :: width_mm, height_mm, qball_vel
    integer                             :: i, frame
    real(dp)                            :: vtotal2, ekin
    integer                             :: numballs, integration_scheme

    namelist /INPUT/ numballs,width_mm,height_mm,   &
                     t_total,dt,integration_scheme, &
                     qball_vel
                     
    open( UNIT=10, file='input.dat' )
    read( UNIT=10, NML=INPUT )
    close(10)

    allocate(x(numballs), &
             y(numballs), &
             vx(numballs), &
             vy(numballs), &
             fx(numballs), &
             fy(numballs)  )

    ! initial conditions
    frame = 0
    t = 0
    call set_boundaries(width_mm, height_mm)
    call set_queue_vel(qball_vel)
    call set_nballs(numballs)
    call initial_setup(x,y,vx,vy)

    select case(integration_scheme)
    case(1) ! euler
        do while (t < t_total)
            call write_(frame,t,ekin,x,y,vx,vy)          ! ausgabe
            call force(x,y,vx,vy,fx,fy,numballs)         ! bestimme kraefte
            call positions_eul(x,y,vx,vy,dt)             ! bewege atome
            call velocities_eul(vx,vy,fx,fy,dt)          ! aktualisiere geschwindigkeiten
            ekin = ekin_(vx,vy)                          ! berechne kinetische energie
            ! zeitschritt
            frame = frame + 1
            t = t + dt
        end do
    case(2) ! stoermer-verlet
        call init_ver(numballs)                          ! f_old initialisieren
        do while (t < t_total)
            call write_(frame,t,ekin,x,y,vx,vy)          ! ausgabe
            call force(x,y,vx,vy,fx,fy,numballs)         ! bestimme kraefte
            call positions_ver(x,y,vx,vy,fx,fy,dt)       ! bewege atome
            call velocities_ver(vx,vy,fx,fy,dt)          ! aktualisiere geschwindigkeiten
            ekin = ekin_(vx,vy)                          ! berechne kinetische energie
            ! zeitschritt
            frame = frame + 1
            t = t + dt
        end do
    end select

contains

function ekin_(vx,vy) result(e)
    real(dp), dimension(:), intent(in)  :: vx,vy
    real(dp)                            :: e
    e = 0.
    do i=1,size(vx)
        e = e + 0.5_dp*m*( vx(i)**2 + vy(i)**2 )
    end do
end function ekin_

subroutine write_(f,t,ekin,x,y,vx,vy)
    integer, intent(in)                 :: f
    real(dp), intent(in)                :: t,ekin
    real(dp), dimension(:), intent(in)  :: x,y
    real(dp), dimension(:), intent(in)  :: vx,vy
    integer                             :: i, numballs

    numballs = size(x)
    write(*,*) "FRAME", f
    write(*,*) t, ekin
    do i=1,numballs
        write(*,*) i, x(i), y(i), vx(i), vy(i)
    end do
    write(*,*) "ENDF"


    end subroutine write_
        
end program main

