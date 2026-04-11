


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
         if(abs(g1(a))>1) then
            write(*,*) 'f1 converge'
            else
                write(*,*) 'f1 diverge'
                end if
                if(abs(g2(b))>1) then
            write(*,*) 'f2 converge'
            else
                write(*,*) 'f2 diverge'
                end if
                if(abs(g3(c))>1) then
            write(*,*) 'f3 converge'
            else
                write(*,*) 'f3 diverge'
                end if

        contains
        function f1(x)
            real::f1,x
            f1=(x**3-5.0)/2.0
            end function
             function g1(x)
            real::g1,x
            g1=3.0/2.0*x**2
            end function
            function f2(x)
            real::f2,x
            f2=(2*x+5.0)**(1/3.0)
            end function
             function g2(x)
            real::g2,x
            g2=2.0/3.0*(2*x+5)**(-2.00/3)
            end function
            function f3(x)
                real::f3,x
            f3=(2.0*x+5.0)/x**2
            end function
             function g3(x)
            real::g3,x
            g3=(2*x**-10*x**2)/x**5
            end function
            end program
