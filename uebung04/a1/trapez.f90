program aufgabe1
    implicit none
    integer, parameter :: sp = 4

    real :: q

    q = trapz(f, -10.0, 8.0, 1000)
    write(*,*) q

    !subroutine trapz(func,a,b,n):
        
    !end subroutine trapz

contains

function f(x) result(y)
    implicit none
    real(kind=sp), intent(in) :: x
    real(kind=sp)             :: y
    y = x**3 + 7
end function f

function trapz(f,a,b,n) result(I)
    implicit none
    real(kind=sp), intent(in) :: a
    real(kind=sp), intent(in) :: b
    integer,       intent(in) :: n
    real(kind=sp)             :: I

    ! f als interface, damit sie aufgerufen werden kann
    interface
        function f(x) result(y)
            real, intent(in) :: x
            real             :: y
        end function
    end interface

    real(kind=sp)             :: h,t
    
    h = (b-a)/n
    t = a
    I = 0.0

    do while (t<b)
        I = I + h*(f(t)+f(t+h))/2.0
        t = t + h
    end do
    
end function

end program aufgabe1

