! This program calculate polynomial regression coefficients based on OLS mehtod(Made by MG 18.01.31)
!
! a=((X^(T))X)^(-1)X^(T)Y
! a: coefficient matrix
! ^T: transpose of a matrix
! ^(-1): inverse matrix
!      
! See 'http://mathworld.wolfram.com/LeastSquaresFittingPolynomial.html' for details


      PROGRAM poly_reg_test
      IMPLICIT NONE
      INTEGER             :: n
      REAL,DIMENSION(8)   :: x, y
      REAL,DIMENSION(8,3) :: mat_x
      REAL,DIMENSION(3,3) :: mat_dummy1,mat_dummy2
      REAL,DIMENSION(3,8) :: mat_dummy3
      REAL,DIMENSION(3)   :: coef
      REAL,DIMENSION(8,1) :: dummy_y

      integer :: i

      data x/32,64,96,118,126,144,152.5,158/
      data y/99.5,104.8,108.5,100,86,64,35.3,15/


      CALL poly_reg(2,8,x,y,coef)

      print *, coef

      ENDPROGRAM

      INCLUDE './sub_poly_reg.f90'
