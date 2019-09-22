program main
    use globals
    use initial
    use forces
    implicit none

    real(dp), allocatable, dimension(:) :: x,y,vx,vy,ax,ay,fx,fy
    real(dp)                            :: t, dt, t_total
    real(dp)                            :: width_mm, height_mm, qball_vel
    integer                             :: i, frame
    real(dp)                            :: vtotal2, ekin
    integer                             :: numballs

    namelist /INPUT/ numballs,width_mm,height_mm,t_total,dt,qball_vel
    
    open( UNIT=10, file='input.dat' )
    read( UNIT=10, NML=INPUT )
    close(10)

    allocate(x(numballs), &
             y(numballs), &
             vx(numballs), &
             vy(numballs), &
             ax(numballs), &
             ay(numballs), &
             fx(numballs), &
             fy(numballs)  )

    ! initial conditions
    frame = 0
    t = 0
    call set_boundaries(width_mm, height_mm)
    call set_queue_vel(qball_vel)
    call set_nballs(numballs)
    call initial_setup(x,y,vx,vy)

    do while (t < t_total)
        ! ausgabe
        write(*,*) "FRAME", frame
        write(*,*) t, ekin
        do i=1,numballs
            write(*,*) i, x(i), y(i), vx(i), vy(i)
        end do
        write(*,*) "ENDF"
        ! bestimme kraefte
        call force(x,y,vx,vy,fx,fy,numballs)
        ! bestimme beschleunigungen
        do i=1,numballs
            ax(i) = fx(i)/m
            ay(i) = fy(i)/m
        end do
        ! bewege atome
        do i=1,numballs
            x(i) = x(i) + vx(i)*dt + ax(i)*dt*dt/2_dp
            y(i) = y(i) + vy(i)*dt + ay(i)*dt*dt/2_dp
        end do
        ! aktualisiere geschwindigkeiten
        do i=1,numballs
            vx(i) = vx(i) + ax(i)*dt/2_dp
            vy(i) = vy(i) + ay(i)*dt/2_dp
        end do
        ! berechne kinetische energie
        ekin = 0.
        do i=1,numballs
            vtotal2 = vx(i)**2 + vy(i)**2
            ekin = ekin + 0.5_dp*m*vtotal2
        end do
        ! zeitschritt
        frame = frame + 1
        t = t + dt
        end do
        
    end program main