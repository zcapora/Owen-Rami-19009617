program field
    implicit none

    real, parameter :: u_0 = 1.2566370621219E-6
    real, parameter :: pi = 3.14159
    real, parameter :: q = 1.0
    real, parameter :: m = 1.67262192E-27
    real, parameter :: c = 2.99792458E8
    real, parameter :: gamma_third= 2.6789385347077476337

    real, parameter :: r = 100000 !radius of magnetic field 
    real, parameter :: h = 10 ! harmonic number 


    integer, dimension(360) :: theta_d
    real, dimension(360) :: theta, y, x, Bx, By, Bz

    
    integer :: i
    real ::  B, P,  num, den
    real,dimension(180) :: alpha, P_s
    real, parameter :: beta = 0.999

    real, parameter :: B0 = 10

    !write(*,*) 'Please work' 
    !write(*,*) 'Input the radius of the cylindrical magetic field'
    !read(*,*)  r
    !write(*,*) 'Input the pitch angle'
    r!ead(*,*)  alpha
    !write(*,*) 'Input the harmonic number'
    !read(*,*)  h

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
    num = sqrt(3.0)*q**3*B*sin(radians(alpha))*F(h)
    den = 2*pi*m*c**2
    P = (sqrt(3.0)*q**3*B*sin(radians(alpha))*F(h))/(2*pi*m*c**2)

    P_s = 2*6.65245875E-29*c*(B**2/8*pi)*(sin(alpha))**2*beta**2/(1-beta**2)

    do i = 1,180
        alpha(i) = i
    end do

    write(*,*) 'The power emitted by one proton:',P_s
    write(*,*) 'The magnetic field strength B = ',B
    contains
        function F(h)
            real :: F,h
                if (h > 1) then
                    F = (pi/2)**0.5*exp(-h)*h**0.5
                else
                    F = (4*pi*(h/2)**(1/3))/(sqrt(3.0)*gamma_third)
                end if
        end function

        function radians(alpha)
            real,dimension(180):: radians, alpha
                radians = (alpha/180)*pi
        end function

    
    
end program