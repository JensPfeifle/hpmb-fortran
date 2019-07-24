module cg
    implicit none
    save

    
   real, parameter :: pi = 3.1415926536  
   real, parameter :: e = 2.7182818285 
   
contains      
   subroutine show_consts()          
      print*, "Pi = ", pi          
      print*,  "e = ", e     
   end subroutine show_consts 
   

    subroutine solvecg(A,b,x)
        implicit none
        real, dimension(3),intent(in) :: b
        real, dimension(3,3),intent(in) :: A
        real, dimension(3),intent(out) :: x

        real, dimension(3) :: r,rk,d,z
        real               :: alph, beta, eps
        real               :: eps_max = 0.001
        integer            :: iters, maxiters

        ! TODO: test for symmetry, make use of it
        
        write(*,*) "solvegc"
        
        eps = 1000.0
        maxiters = 100
        iters = 0
    
        ! reference: https://de.wikipedia.org/wiki/CG-Verfahren#CG-Verfahren_ohne_Vorkonditionierung
        x = (/0.,0.,0./)
        r = b - matmul(A,x)
        d = r

        do while (eps > eps_max .AND. iters < maxiters)
            z = matmul(A,d)
            alph = dot_product(r,r)/dot_product(d,z)
            x = x + alph*d
            rk = r - alph*z
            beta = dot_product(rk, rk)/dot_product(r,r)
            d = rk + beta*d
            eps = dot_product(r,r)
            iters = iters + 1
            r = rk
        end do

    write(*,*) "solvegc finished after", iters, "iterations"
    write(*,*) "residual error:", eps

    end subroutine solvecg

end module cg