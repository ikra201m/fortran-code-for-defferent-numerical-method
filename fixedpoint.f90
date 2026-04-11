program fixd
    implicit none
    real::a,b,c
    integer::i
    a=1.5
    b=1.5
    c=1.5
    write(*,*)' n ', 'f1 ','f2 ','f3 '
    do i=1,20
        write(*,10)i-1,a,b,c
        10 format(i2,2x,f12.8,2x,f12.8,2x,f12.8)
        a=f1(a)
        b=f2(b)
        c=f3(c)
        end do
        contains
        function f1(x)
            real::f1,x
            f1=(x**3-5.0)/2.0
            end function
            function f2(x)
            real::f2,x
            f2=(2*x+5.0)**(1/3.0)
            end function
            function f3(x)
                real::f3,x
            f3=(2.0*x+5.0)/x**2
            end function
            end program


