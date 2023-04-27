program field
    implicit none

    real, parameter :: u_0 = 1.2566370621219E-6
    real, parameter :: pi = 3.14159
    real, parameter :: q = 1.0
    real, parameter :: m = 1.67262192E-27
    real, parameter :: c = 2.99792458E8
    

    real, parameter :: r = 10000 !radius of magnetic field 
    


    integer, dimension(360) :: theta_d
    real, dimension(360) :: theta, y, x, Bx, By, Bz

    
    integer :: i
    real ::  B

    real, dimension(180) :: alpha, p_s

    real, parameter :: beta = 0.999

    real, parameter :: B0 = 10

   

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

    P_s = 2*6.65245875E-29*c*(B**2/8*pi)*(sin(alpha))**2*beta**2/(1-beta**2)

    do i = 1,180
        alpha(i) = i
    end do


    !write(*,*) 'The power emitted by one proton:',P_s
    write(*,*) 'The magnetic field strength B = ',B

    open(1, file = 'data1.csv', status = 'new')
    do i = 1,180
        write (1,*) alpha(i),2*6.65245875E-29*c*(B**2/8*pi)*(sin(alpha(i)))**2*beta**2/(1-beta**2)
    end do
    close(1)
    
    contains
        
        function radians(alpha)
            real,dimension(180 ):: radians, alpha
                radians = (alpha/180)*pi
        end function

    
    
end program