program cheby
    implicit none
    real::a,b,x,f,df,ddf
    integer::i
    a=1
    i=0
    do
        b=a-(f(a)/df(a))-((f(a))**2*ddf(a))/(2*(df(a))**3)
        i=i+1
        if(abs(b-a)<0.0002) exit
        a=b
        write(*,10)i-1,a
        10 format(i2,2x,f7.4)
        end do
        end program
        function f(x)
            real::x,f
            f=x**3-2*x-5
            end function
            function df(x)
            real::dx,f
            df=3*x**2
            end function
            function ddf(x)
            real::ddf,x
            ddf=6*x
            end function



