program main
    use initial
    use forces
    use globals
    implicit none

    real, dimension(numballs) :: x,y,vx,vy,ax,ay,fx,fy
    real                :: t
    integer             :: i, frame
    real                :: vtotal2, ekin

    !fixme interfaces!

    ! initial conditions
    frame = 0
    t = 0
    call setupinitial(x,y,vx,vy)

    do while (t < 10.)
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
            x(i) = x(i) + vx(i)*dt + ax(i)*dt*dt/2.
            y(i) = y(i) + vy(i)*dt + ay(i)*dt*dt/2.
        end do
        ! aktualisiere geschwindigkeiten
        do i=1,numballs
            vx(i) = vx(i) + ax(i)*dt/2.
            vy(i) = vy(i) + ay(i)*dt/2.
        end do
        ! berechne kinetische energie
        ekin = 0.
        do i=1,numballs
            vtotal2 = vx(i)**2 + vy(i)**2
            ekin = ekin + 0.5*vtotal2
        end do
        
        ! zeitschritt
        frame = frame + 1
        t = t + dt
        end do
        
    end program main