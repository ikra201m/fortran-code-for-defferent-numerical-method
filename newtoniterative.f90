program newtonitterative
    implicit none
    real::x,y,f,g,dfx,dgx,dx,dy,dfy,dgy,det
    integer::i
    x=5
    y=2
    do i=1,3
        f=log(x-y**2)-sin(x*y)-sin(3.1416)
        g=exp(x*y)+cos(x-y)-2
        dfx=1/(x-y**2)-y*cos(x*y)
        dfy=-2*y*(1/(x-y**2))-x*cos(x*y)
        dgx=y*exp(x*y)-sin(x-y)
        dgy=x*exp(x*y)+sin(x-2)
        det=dfx*dgy-dfy*dgx
        dx=((-f)*dgy-(-g)*dfy)/det
        dy=((-g)*dfx-(-f)*dgx)/det
        x=x+dx
        y=y+dy
        write(*,*)i-1,x,y
        end do
        end program



