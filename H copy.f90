program field
    implicit none
    real, parameter :: A = 1.4E-10
    real, parameter :: p = 3
    real, parameter :: e_max = 5000
    real, dimension(5000) :: x, phi
    integer :: i

    do i = 1,5000
        x(i) = i
    end do

    phi = (1.4E-10)*x(i)**(-p)*exp((-x)/e_max)

    open(1, file = 'D7.csv', status = 'new')
    do i = 1,5000
        write (1,*)  x(i), (1.4E-10)*x(i)**(-p)*exp((-x(i))/e_max)
    end do


end program