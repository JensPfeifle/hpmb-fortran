program name
    implicit none

    integer, parameter :: numballs = 3 ! including cue

    real, dimension(numballs+1) :: x,y,vx,vy,ax,ay,fx,fy
    real                :: t, dt
    integer             :: i, frame
    real, parameter     :: m = 0.17 ! [kg]
    real                :: vtotal2, kinen

    interface
        subroutine forces(x,y,fx,fy,numballs)
            integer, intent(in) :: numballs
            real, dimension(numballs),intent(in)  :: x,y
            real, dimension(numballs),intent(out) :: fx,fy
        end subroutine forces
    end interface

    ! fixme, use module to create globals
    dt = 0.0001
    frame = 0
    !table size

    ! Ausgangslage
    t = 0
    x(1) = 55.
    y(1) = 25.
    x(2) = 50.
    y(2) = 50.
    x(3) = 50.
    y(3) = 75.

    vx(1) = 0.
    vy(1) = 50.
    vx(2) = 0.
    vy(2) = 0.
    vx(3) = 0.
    vy(3) = 0.

    do while (t < 60)

        ! bestimme F,a
        call forces(x,y,fx,fy,numballs)

        do i=1,numballs
            ax(i) = fx(i)/m
            ay(i) = fy(i)/m
        end do
   
        ! bewege Atome
        do i=1,numballs
            x(i) = x(i) + vx(i)*dt + ax(i)*dt*dt/2.
            y(i) = y(i) + vy(i)*dt + ay(i)*dt*dt/2.
        end do
        ! aktualisiere Geschwindigkeiten
        do i=1,numballs
            vx(i) = vx(i) + ax(i)*dt/2.
            vy(i) = vy(i) + ay(i)*dt/2.
        end do

        kinen = 0.
        do i=1,numballs
            vtotal2 = vx(i)**2 + vy(i)**2
            kinen = kinen + 0.5*vtotal2
        end do
        ! ausgabe
        write(*,*) "FRAME", frame
        write(*,*) t, kinen
        do i=1,numballs
            write(*,*) i, x(i), y(i), vx(i), vy(i)
        end do
        write(*,*) "ENDF"

        
        ! Zeitschritt
        frame = frame + 1
        t = t + dt
        
        end do
        
    end program name

! fixme: use interface
subroutine forces(x,y,fx,fy, numballs)
    implicit none
    integer                  ,intent(in)  :: numballs
    real, dimension(numballs),intent(in)  :: x,y
    real, dimension(numballs),intent(out) :: fx,fy
    integer                               :: i,j
    real :: dx, dy, r2, fr, fxi, fyi

    do i=1,numballs
        fx(i) = 0.
        fy(i) = 0.
    end do

    ! fixme check r2 > r2cut
    
    ! ball-to-ball forces
    do i=1,numballs-1
        do j=i+1,numballs
            dx = x(i) - x(j)
            dy = y(i) - y(j)
            ! 32.49 = kugeldurchmesser**2
            r2 = max(0., dx*dx + dy*dy - 32.49)
            ! anteilige kraft
            fr = 50./(r2)**3
            fxi = fr * dx
            fyi = fr * dy
            ! gesamtkraefte
            fx(i) = fx(i) + fxi ! actio
            fx(j) = fx(j) - fxi ! reactio
            fy(i) = fy(i) + fyi ! actio
            fy(j) = fy(j) - fyi ! reactio
        end do
    end do

    ! boundaries
    do i=1,numballs
        ! fixme, hardcoded edges

        ! left
        if (x(i) < 15.) then
            dx = x(i)
            r2 = max(0., dx*dx - 32.49)
            fr = 50./(r2)**3
            fxi = fr * dx
        ! right
        else if (x(i) > 90.) then
            dx = x(i) - 111.76
            r2 = max(0., dx*dx - 32.49)
            fr = 50./(r2)**3
            fxi = fr * dx
        else 
            fxi = 0.
        end if

        ! top
        if (y(i) < 15.) then
            dy = y(i)
            r2 = max(0., dy*dy - 32.49)
            fr = 50./(r2)**3
            fyi = fr * dy
        ! bottom
        else if (y(i) > 200.) then
            dy = y(i) - 223.52
            r2 = max(0., dy*dy - 32.49)
            fr = 50./(r2)**3
            fyi = fr * dy
        else 
            fyi = 0.
        end if

        ! gesamtkraefte
        fx(i) = fx(i) + fxi ! actio
        fx(j) = fx(j) - fxi ! reactio
        fy(i) = fy(i) + fyi ! actio
        fy(j) = fy(j) - fyi ! reactio
    end do


end subroutine forces
