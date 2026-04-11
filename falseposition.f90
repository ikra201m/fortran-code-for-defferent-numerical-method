program falsepos
    implicit none
    real::a,b,c,f,x
    integer::d
    a=2.0
    b=3.0
    d=0
    write(*,*)' n ',' root ','  f(x) '
    do
        c=(a*f(b)-b*f(a))/(f(b)-f(a))
        d=d+1
        if(abs(f(c))<0.00002) exit
        if(f(c)*f(a)>0) then
            a=c
            else
                b=c
                end if
                write(*,10)d,c,f(c)
                10 format(i3,2x,f7.4,2x,f7.4)
                end do
                end program
                function f(x)
                    real::x,f
                    f=x**3-2*x-5
                    end function

