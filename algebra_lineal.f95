module lineal

contains

subroutine Gauss(A,B,X)

    real(8), intent(in)     :: A(:,:)
    real(8), intent(in)     :: B(:)
    real(8), intent(inout)  :: X(:)

    ! Locales
    ! Es posible que haya que añadir variables locales para hacer pivote
    real(8), allocatable    :: AB(:,:)
    real(8)                 :: m
    integer                 :: i,k,n

    n = size(A,1)
    allocate(AB(n,n+1))

    AB(:,1:n) = A
    AB(:,n+1) = B
    X = 0.d0

    ! Cuerpo del Programa

    ! I.- Triangulación
    ! Bucle por elementos en la diagonal
    do k = 1, n-1
        ! Pivote parcial
        ! Completa el código para implementar el pivote parcial
        



        ! ¿Qué ocurre si el elemento A(n,n)=0?
        do i = k+1, n !Bucle por las filas debajo de la diagonal 
            m = -AB(i,k)/AB(k,k) 
            AB(i,:) = AB(i,:) + m*AB(k,:)
        enddo
    enddo
    
    ! Matriz Triangular
    write(*,*) 'Comprobación Matriz Triangular'
    do i = 1,n
        write(*,fmt='(6(f7.2,1x))') AB(i,:)
    enddo

    ! II.- Sustitución
    ! Bucle por elementos en la diagonal de abajo a arriba
    do k = n, 1, -1
        m = 0.d0 ! Sumatorio de terminos ya calculados 
        ! Bucle desde la diagonal+1 a n
        do j = k+1, n
            m = m + AB(k,j)*X(j)
        enddo
        X(k) = (AB(k,n+1)-m)/AB(k,k)
    enddo 

    deallocate(AB)

end subroutine

subroutine LU_solver(A,b,x)

    real(8),intent(in)  :: A(:,:)
    real(8),intent(in)  :: b(:)
    real(8),intent(out) :: x(:)


    !Locales
    integer :: n
    real(8),allocatable     :: L(:,:),U(:,:),P(:,:)
    real(8),allocatable     :: y(:),baux(:)

    n = size(A,1)
    allocate(L(n,n),U(n,n),P(n,n),y(n),baux(n))

    call LU_factor(A,L,U,P)
    baux = matmul(P,b)
    call lower_solver(L,baux,y)
    call upper_solver(U,y,x)

    deallocate(L,U,P,y,baux)

end subroutine

subroutine LU_factor(A,L,U,P)

    ! ****** Descripción  *********
    ! Esta subrutina devuelve la factorización LU de una matriz A de la forma  PA = LU
    ! Siendo P una matriz de permutación, L una matriz triangular inferior y U una matriz 
    ! triangular superior todas de tamaño nxn.

    real(8), intent(in)     :: A(:,:)
    real(8), intent(inout)  :: L(:,:), U(:,:), P(:,:)

    integer             :: n,i,k,pos(1)
    real(8),allocatable :: V(:)
    real(8)             :: V_aux,m

    n = size(A,1)
    allocate(V(n))

    ! Inicializo las variables
    U = A
    L = 0.d0
    P = 0.d0
    do i =1,n
        L(i,i) = 1.d0
        P(i,i) = 1.d0
    enddo
        
    ! Bucle por elementos de la diagonal
    do k = 1, n
        ! Pivote parcial
        if (abs(U(k,k))<epsilon(1.0)) then
            pos = maxloc(abs(U(k+1:n,k)))
            if (abs(U(k+pos(1),k))<epsilon(1.d0)) then
                write(*,*)'Sistema incompatible'
                stop
            else
                V = U(k,:)
                U(k,:) = U(k+pos(1),:)
                U(k+pos(1),:) = V

                V = P(k,:)
                P(k,:) = P(k+pos(1),:)
                P(k+pos(1),:) = V

                do i = 1, k-1 ! Cambio las filas de L. Solo las columnas ya calculadas
                    v_aux = L(k,i)
                    L(k,i)= L(k+pos(1),i)
                    L(k+pos(1),i) = v_aux
                enddo
            endif
        endif

        do i = k+1, n !Bucle por las filas debajo de la diagonal 
            m = U(i,k)/U(k,k) 
            U(i,:) = U(i,:) - m*U(k,:)
            L(i,k) = m
        enddo
    enddo

end subroutine

subroutine upper_solver(U,b,x)
    
    ! ****** Descripción  *********
    ! Esta subrutina resuelve un sistema de la forma Ux = b 
    ! Siendo U una matriz triangular superior de nxn y b un vector de tamaño n
     
    real(8),intent(in)  :: U(:,:)
    real(8),intent(in)  :: b(:)
    real(8),intent(out) :: x(:)

    ! Variables locales
    ! Añade las variables locales que necesites


    ! Implementa el código para resolver el sistema



end subroutine


subroutine lower_solver(L,b,x)

    ! ****** Descripción  *********
    ! Esta subrutina resuelve un sistema de la forma Lx = b 
    ! Siendo L una matriz triangular inferior de nxn y b un vector de tamaño n
     
    real(8),intent(in)  :: L(:,:)
    real(8),intent(in)  :: b(:)
    real(8),intent(out) :: x(:)

    ! Variables locales
    ! Añade las variables locales que necesites


    ! Implementa el código para resolver el sistema



end subroutine

end module