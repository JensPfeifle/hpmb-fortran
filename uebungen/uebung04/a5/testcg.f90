program testcg
    use cg
    implicit none

    real, dimension(3,3) :: A
    real, dimension(3) :: b,x
    integer :: i, j
     
    !do i = 1, 3
    !   do j = 1, 3
    !    !if (i == j) then
    !      A(i, j) = 1.0*(i+j)
    !    !end if
    !   end do
    !end do

    A = reshape( (/1.,-5.,8., -5.,-2.,1., 8.,1.,-5./) , (/3,3/))

    b = (/15., -6., -5./)

    call show_consts()

    write(*,*) "A", A
    write(*,*) "b", b

    call solvecg(A,b,x)

    write(*,*) "x", x

end program testcg


