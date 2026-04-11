program gaussian
    implicit none
    real::A(3,3),B(3),X(3)
    integer::i,j,k
    real::factor
    write(*,*)"Enter the cof matrix A "
    do i=1,3
        read(*,*)(A(i,j),j=1,3)
        end do
        write(*,*)" enter the column vector B"
        read(*,*) (B(i),i=1,3)
        do k=1,2
            do i=k+1,3
                factor=A(i,k)/A(k,k)

                do j=k,3
                    A(i,j)=A(i,j)-factor*A(k,j)
                    end do
                    B(i)=B(i)-factor*B(k)
                    end do
                    end do
                    X(3)=B(3)/A(3,3)

                    do i=2,1,-1
                        X(i)=B(i)

                        do j=i+1,3
                            X(i)=X(i)-A(i,j)*X(i)
                            end do
                            X(i)=X(i)/A(i,j)
                            end do
                            write(*,*) X
                            end program







