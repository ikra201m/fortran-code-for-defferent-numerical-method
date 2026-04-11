program secent
    implicit none
    real::a,b,c,f,x
    integer::d
    a=2.0
    b=3.0
    d=0
    write(*,*)' n ',' c ',' f(x)'
    do
        c=b-((b-a)*f(b))/(f(b)-f(a))
        d=d+1
        if(abs(f(c))<0.000002) exit
        a=b
        b=c
        write(*,10)d,c,f(c)
        10 format(i3,2x,f7.4,2x,f7.4)
        end do
        end program
        function f(x)
            f=x**3-3*x-5
            end function
