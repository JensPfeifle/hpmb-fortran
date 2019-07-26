program name
    implicit none

    integer, parameter :: numballs = 2 ! including cue

    real, dimension(numballs+1) :: x,y,vx,vy,ax,ay,fx,fy
    real                :: t, dt
    integer             :: i
    real, parameter     :: m = 0.17 ! [kg]
    real                :: vtotal2, kinen

    interface
        subroutine forces(x,y,fx,fy,numballs)
            integer, intent(in) :: numballs
            real, dimension(numballs),intent(in)  :: x,y
            real, dimension(numballs),intent(out) :: fx,fy
        end subroutine forces
    end interface

    dt = 0.001

    ! Ausgangslage
    t = 0
    x(1) = 50.5
    y(1) = 25.
    x(2) = 50.
    y(2) = 50.

    vx(1) = 0.
    vy(1) = 30.
    vx(2) = 0.
    vy(2) = 0.

    do while (t < 1)

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

        ! ausgabe
        do i=1,numballs
            write(*,*) t, x(i), y(i), vx(i), vy(i)
        end do

        kinen = 0.
        do i=1,numballs
            vtotal2 = vx(i)**2 + vy(i)**2
            kinen = kinen + 0.5*vtotal2
        end do
        write(*,*) "KE:", kinen
        
        ! Zeitschritt
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
    real :: dx, dy, r2, dudr, fxi, fyi

    do i=1,numballs
        fx(i) = 0.
        fy(i) = 0.
    end do
    
    do i=1,numballs-1
        do j=i+1,numballs
            dx = x(i) - x(j)
            dy = y(i) - y(j)
            r2 = dx*dx + dy*dy
            ! fixme check r2 > r2cut
            ! anteilige kraft
            dudr = -9.*r2**(-5)
            fxi = dudr * dx / (dx + dy) ! F=-du/dr * normalized distance vector
            fyi = dudr * dy / (dx + dy) ! F=-du/dr * normalized distance vector
            ! normalisieren koennte man schon bei dudr, aber das ist eh erfunden
            ! gesamtkraefte
            fx(i) = fx(i) + fxi ! actio
            fx(j) = fx(j) - fxi ! reactio
            fy(i) = fy(i) + fyi ! actio
            fy(j) = fy(j) - fyi ! reactio
        end do
    end do

end subroutine forces
