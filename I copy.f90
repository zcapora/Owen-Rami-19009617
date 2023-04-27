program field
    implicit none

    real, parameter :: n = 100000 ! electrons per cubic meter
    real, parameter :: I_0 = 10 ! Initial intensity
    real, parameter :: beta = 0.9 ! beta = v/c
    real, parameter :: sigma_t = 6.65245875E-29 ! constant
    integer :: c ! counter
    real :: gamma, a, b
    real, dimension(20) :: x, I
    gamma = 1/sqrt(1-beta**2)
    a = (1-beta**2)/((1+beta)**2)
    b = 1/((1+beta)**2*gamma**2)
    I = x(c)-a*x(c)**2

    do c = 1,20
        x(c) = c
    end do
    write(*,*) 'value of b = ',b
    write(*,*) 'value of a = ',a
    write(*,*) 'value of I(1) ', x(1)-a*x(1)**2
    open(1, file = 'D6.csv', status = 'new')
    do c = 1,20
        write (1,*)  x(c), sigma_t*x(c)-a*x(c)**2*n/(4*gamma**2*beta**2)
    end do
    close(1)

    



   

    
    
    

    
    
end program