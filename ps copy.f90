program field
    implicit none
    real, parameter :: e = 16.E-19
    real, parameter :: m = 9.1E-21
    real, parameter :: c = 2.99E-8
    integer, dimension(360) :: theta_d
    real, dimension(360) :: theta, y, x, Bx, By, Bz
    integer :: i, j
    real ::  B, nu_s, nu_c, gamma, p_angle
    real, dimension(1000) :: p_s, x_nu, nu
    real, parameter :: beta = 0.999
    real, parameter :: B0 = 10
    real, parameter :: r = 10000 !radius of magnetic field 
    real, parameter :: pi = 3.14159
    

    theta = theta_d * 2 * pi / 360
    x = r*cos(theta)
    y = r*sin(theta)
    
    do i = 1,360
        theta_d(i) = i 
    end do

    Bx = B0 * cos(atan(y/x))
    By = -B0 * sin(atan(y/x))
    do i = 1,360
        Bz(i) = 0
    end do

    B = sqrt(dot_product(Bx,Bx)**2 + dot_product(By,By)**2 + dot_product(Bz,Bz)**2 )

    p_angle = pi/2
    gamma = 1/(sqrt(1-beta**2))
    nu_s = (gamma**2*e*B)/(2*pi*m*c)
    nu_c = 1.5*nu_s*sin(p_angle)

    p_s = ((1.73205*e**3*B*sin(p_angle))/(m*c**2))*x_nu*BESSEL_YN(2, x_nu(j))
    x_nu = nu/nu_c

    open(1, file = 'ps.csv', status = 'new')
    do j = 1,1000
        write (1,*)  x_nu(j),BESSEL_YN(2, x_nu(j))
    end do


    
end program