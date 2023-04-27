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

    phi = -(x(i)/e_max)*LOG(A*x(i)**(-p))

    open(1, file = 'D8.csv', status = 'new')
    do i = 1,5000
        write (1,*)  x(i), -(x(i)/e_max)*LOG(A*x(i)**(-p))
    end do


end program