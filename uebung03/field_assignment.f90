program test
implicit none

real*8, dimension(1:3,1:3) :: A
integer :: i,j

A(:,:) = 0.0
A = reshape( (/ ((i*(j+2),i=1,3), j=1,3) /), (/3,3/))

write(*,*) 'A', A


A(:,:) = 0.0
do i=1,3
    do j=1,3
        A(i,j) = i*(j+2)
    end do
end do

write(*,*) 'A', A

end program test

