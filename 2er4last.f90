program conv
    implicit none
    real::x,y,xn,yn
    integer::i
    x=4.4
    y=1.0
    do i=1,100

        xn=sqrt(7.0-y)
        yn=-sqrt(abs(x-4.0))
        if(abs(yn-y)<0.0000001 .and. abs(xn-x) <0.000001) then
            write(*,*) "conv"
            x=xn
            y=yn
            exit
            end if
            x=xn
            y=yn
            write(*,*) i,x,y
            end do
            write(*,*) x,y
            end program

