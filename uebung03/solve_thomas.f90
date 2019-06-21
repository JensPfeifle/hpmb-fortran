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

! Given LGS
!   [6 -1  0]   |14|
!   [9 -1  1] = |21|
!   [0 -7  5]   |17|

a = [9, -7]
b = [6, -1, 5]
c = [-1, 1]
d = [14,21,17]

call ludecomp(a,b,c,l,m,r)

contains

subroutine ludecomp(a,b,c, l, m, r)
    implicit none

    double precision, intent(in) :: a(:)
    double precision, intent(in) :: b(:)
    double precision, intent(in) :: c(:)

    double precision, intent(out) :: l(:)
    double precision, intent(out) :: m(:)
    double precision, intent(out) :: r(:)

    write(*,*) "a", a
    write(*,*) "b", b
    write(*,*) "c", c


    write(*,*) "l", l
    write(*,*) "m", m
    write(*,*) "r", r


end subroutine ludecomp

end program thomas


