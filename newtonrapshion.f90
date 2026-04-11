program newtonschems
    implicit none
    real::a,xsqrt,xcubrt,xinv
    integer::i
    write(*,*)" eneter a number a:"
    read(*,*) a
    xsqrt=a/2.0 !
    do i=1,10
        xsqrt=(1/2.0)*(xsqrt+a/xsqrt)
        end do
        xcubrt=a/3.0
        do i=1,10
            xcubrt=(1/3.0)*(2*xcubrt+a/xcubrt**2)
            end do
            xinv=1/a
            do i=1,10
                xinv=xinv*(2-a*xinv)
                end do
                write(*,*)"the value of a:",a
                write(*,*)" squre root of a:",xsqrt
                write(*,*)" cube root of a:",xcubrt
                write(*,*) " inverse of a :",xinv
                end program


