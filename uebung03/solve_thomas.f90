program thomas
implicit none

! Goal: solve Ax=d where
!       A is tridiagonal
!       x, d are vectors
!   through LU decomposition

! A
double precision, dimension(2) :: a
double precision, dimension(3) :: b
double precision, dimension(2) :: c
! d
double precision, dimension(3) :: d

! L
double precision, dimension(2) :: l
! U
double precision, dimension(3) :: m
double precision, dimension(2) :: r

! x
double precision, dimension(3) :: x
double precision, dimension(3) :: y

! Given LGS
!   [6 -2  0]   |14|
!   [9 -1  1] = |21|
!   [0 -7  5]   |17|

a = [9, -7]
b = [6, -1, 5]
c = [-2, 1]
d = [14,21,17]

write(*,*) 'original'
write(*,*) "a", a
write(*,*) "b", b
write(*,*) "c", c

call ludecomp(a,b,c,l,m,r)

write(*,*) 'decomp'
write(*,*) "l", l
write(*,*) "m", m
write(*,*) "r", r

call forwardssubs(l,d,y)
call backwardssubs(m,r,x)

write(*,*) 'solved'
write(*,*) 'y', y
write(*,*) 'x', x

contains

subroutine ludecomp(a,b,c, l, m, r)
    implicit none

    double precision, intent(in) :: a(:)
    double precision, intent(in) :: b(:)
    double precision, intent(in) :: c(:)
    double precision, intent(out) :: l(:)
    double precision, intent(out) :: m(:)
    double precision, intent(out) :: r(:)
    integer :: n, i

    n = size(b)
    m(1) = b(1)
    do i=1,n-1
        r(i) = c(i)
        l(i) = a(i)/m(i)
        m(i+1) = b(i+1) - l(i)*r(i)
    end do

end subroutine ludecomp

subroutine forwardssubs(l,d,y)
    ! solve Ly=d for y
    implicit none
    double precision, intent(in) :: l(:)
    double precision, intent(in) :: d(:)
    double precision, intent(out) :: y(:)
    integer :: n, i

    n=size(d)

    y(1)=d(1)

    do i=2,n
        y(i)=d(i)-l(i-1)*y(i-1)
    end do

end subroutine forwardssubs

subroutine backwardssubs(m,r,x)
    ! solve Ux=y for x
    implicit none
    double precision, intent(in) :: m(:)
    double precision, intent(in) :: r(:)
    double precision, intent(out) :: x(:)
    integer :: n, i

    n=size(x)

    x(n)=y(n)/m(n)

    do i=n-1,1, -1
        write(*,*) 'i',i
        x(i)=(y(i)-r(i)*x(i+1))/m(i)
    end do

end subroutine backwardssubs

end program thomas


