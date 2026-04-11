program gausseidal
    implicit none
    real,dimension(1000)::x,y,z
    integer::i
    x(1)=1.5
    y(1)=1.5
    z(1)=-0.5
    write(*,*)'  n    ','       xn     ','    yn      ','      zn    '
    do i=2,100
        x(i)=(14.0-5*y(i-1)-3*z(i-1))/6.0
        y(i)=(11-8*x(i)-2*z(i-1))/(-3.0)
        z(i)=(21*x(i)+7*y(i))/(-8.0)
        write(*,*) i-1,x(i),y(i),z(i)
        if(abs(x(i)-x(i-1))<0.00001) then
            write(*,*)" roots are " ,x(i),y(i),z(i)
            exit
            end if
            end do
            end program
