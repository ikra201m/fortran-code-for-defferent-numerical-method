program dominat
    implicit none
    real,dimension(1000)::a(3,3),b(3),NA(3,3),NB(3),x,y,z
    integer::i,j,p1,p2,p3
    x(1)=0.5
    y(1)=0.05
    z(1)=0.5
    write(*,*)"Enter the cof mAtrric"
    do i=1,3
        read(*,*) (a(i,j) ,j=1,3)
        end do
        write(*,*)"enetr the column B"
        read(*,*) (b(i),i=1,3)
        do p1=1,3
            do p2=1,3
                do p3=1,3
                    if (p1/=p2 .and. p1/=p3 .and. p2/=p3) then
                       NA(1,:)=a(p1,:)
                        NA(2,:)=a(p2,:)
                        NA(3,:)=a(p3,:)
                        NB(1)=b(p1)
                        NB(2)=b(p2)
                        NB(3)=b(p3)
                        if(abs(NA(1,1))>(abs(NA(1,2))+abs(NA(1,3))) .and. &
                        abs(NA(2,2))>(abs(NA(2,1))+abs(NA(2,3))) .and. &
                         abs(NA(3,3))>(abs(NA(3,1))+abs(NA(3,2)))) then
                         write(*,*) " diagonal matrix found"
                         do i=1,3
                            write(*,*) NA(i,1),NA(i,2),NA(i,3) ,' | ',NB(i)
                            end do
                            goto 10
                            end if
                            end if
                            end do
                            end do
                            end do
                            write(*,*)" no dominant found"
                            10 continue
                            write(*,*) " solve by gass"
                            write(*,*) 'xn ','yn' ,'zn'
                            do i=2,100
                                x(i)=(1-y(i-1)+z(i-1))/3.0
                                y(i)=(2-2*x(i)-z(i-1))/4.0
                                z(i)=(3+x(i)-2*y(i))/5.0
                                write(*,*) i-1,x(i),y(i),z(i)
                                if(abs(x(i)-x(i-1))<0.000001 .and. abs(y(i)-y(i-1))<0.000001 .and.  abs(z(i)-z(i-1))<0.00001) then
                                    write(*,*) i-1,x(i),y(i),z(i)
                                    exit
                                    end if
                                    end do
                            end program


