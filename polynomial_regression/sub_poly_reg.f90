! This program calculate polynomial regression coefficients based on  
! OLS method (Made by MG 18.01.31)
! ---------------------------------------------------------------------
! a=((X^(T))X)^(-1)X^(T)Y
! a: coefficient matrix
! ^T: transpose of a matrix
! ^(-1): inverse matrix
!      
! See 'http://mathworld.wolfram.com/LeastSquaresFittingPolynomial.html' 
! for details
! ---------------------------------------------------------------------

      SUBROUTINE poly_reg(n,ndim,x,y,coef)
      IMPLICIT NONE
      INTEGER,INTENT(IN)              :: n                              ! order
      INTEGER,INTENT(IN)              :: ndim                           ! dimension of x and y
      REAL,DIMENSION(ndim),INTENT(IN) :: x,y                            ! input data
      REAL,DIMENSION(n+1),INTENT(OUT) :: coef                           ! regression coefficients

      REAL,DIMENSION(ndim,n+1)        :: mat_x
      REAL,DIMENSION(ndim,1)          :: mat_y
      REAL,DIMENSION(n+1,n+1)         :: dummy1, dummy2
      REAL,DIMENSION(n+1,1)           :: dummy3

      INTEGER                         :: i

      ! .. Matrix of Y ..
      mat_y(:,1) = y

      ! .. Matrix of X ..
      mat_x(:,1) = 1.
      DO i = 2,(n+1)
        mat_x(:,i) = x(:)**(i-1)
      ENDDO ! i

      ! .. Caculating regression coefficients ..
      dummy1 = MATMUL(TRANSPOSE(mat_x),mat_x)
      CALL s_inverse(dummy1,(n+1),(n+1),dummy2)
      dummy3 = MATMUL(MATMUL(dummy2,TRANSPOSE(mat_x)),mat_y)

      coef = dummy3(:,1)

      RETURN
      ENDSUBROUTINE

      INCLUDE './sub_invmat.f90'
