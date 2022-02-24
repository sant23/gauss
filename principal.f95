program sistema_lineal

    use lineal
    ! Variables
    real(8), allocatable    :: A(:,:)
    real(8), allocatable    :: B(:)
    real(8), allocatable    :: X(:), C(:)
    
    integer :: n
    
    ! Damos valores
    n = 4
    allocate(A(n,n), B(n),C(n), X(n))
    
    ! **** Problema 1 ****************
    A(1,:)= (/2.d0, 3.d0, 4.d0, 5.d0/)
    A(2,:)= (/6.d0, 15.d0, 19.d0, 23.d0/)
    A(3,:)= (/8.d0, 42.d0, 60.d0, 70.d0/)
    A(4,:)= (/12.d0, 60.d0, 1.d0, 17.d0/)
    
    B=(/5.d0, 30.d0, 98.d0, 144.d0/)
    X = 0.d0
    
    call Gauss(A,B,X)
    
    write(*,*) 'Solucion Gauss'
    write(*,fmt='(6(f7.2,1x))') X(:)
    
    ! Comprobación resultados
    C = matmul(A,X)
    write(*,fmt='(a,6(f6.1,1x))') ' A*X= ', C(:)
    write(*,fmt='(a,6(f6.1,1x))') '   B= ', B(:)
    
    !**********************************************************
    !******************* Problema 2 ***************************
    
    A(1,:)= (/1.d0, -1.d0, 1.d0, 0.d0/)
    A(2,:)= (/-1d0, 2.d0, 0.d0, 1.d0/)
    A(3,:)= (/-1.d0, 0.d0, -2.d0, 1.d0/)
    A(4,:)= (/0.d0, 1.d0, -1.d0, 2.d0/)
    
    B=(/2.d0, 7.d0, -3.d0, 7.d0/)
    X = 0.d0
    
    call Gauss(A,B,X)
    
    write(*,*) 'Solucion Gauss'
    write(*,fmt='(6(f7.2,1x))') X(:)
    
    ! Comprobación resultados
    C = matmul(A,X)
    write(*,fmt='(a,6(f6.1,1x))') ' A*X= ', C(:)
    write(*,fmt='(a,6(f6.1,1x))') '   B= ', B(:)
    
    !**********************************************************
    !******************* Problema 3 ***************************
    
    A(1,:)= (/2.d0, 3.d0, 4.d0, 5.d0/)
    A(2,:)= (/6.d0, 15.d0, 19.d0, 23.d0/)
    A(3,:)= (/8.d0, 42.d0, 60.d0, 70.d0/)
    A(4,:)= (/12.d0, 60.d0, 1.d0, 6.d0/)
    
    B=(/5.d0, 30.d0, 98.d0, 144.d0/)
    X = 0.d0
    
    call Gauss(A,B,X)
    
    write(*,*) 'Solucion Gauss'
    write(*,fmt='(6(f7.2,1x))') X(:)
    
    ! Comprobación resultados
    C = matmul(A,X)
    write(*,fmt='(a,6(f6.1,1x))') ' A*X= ', C(:)
    write(*,fmt='(a,6(f6.1,1x))') '   B= ', B(:)
    
    
    
    end program
