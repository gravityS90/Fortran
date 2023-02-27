! Calculate inverse matrix
! Apply to symmetric matrix
! Compare output with other results calculated by R or Wolfram|Alpha, If you apply this program to nonsymmertic matrix

       SUBROUTINE s_inverse(matrix,zeilen,spalten,inverse)
       IMPLICIT NONE
       INTEGER                          :: i,j,k
       INTEGER                          :: zeilen,spalten     ! zeilen by spalten
       REAL,DIMENSION(zeilen,spalten)   :: matrix             ! Matrix (input)
       REAL,DIMENSION(zeilen,spalten)   :: inverse            ! inverse matrix (output)
       REAL,DIMENSION(288,288)          :: ursprung, einheit  ! einheit: unit matrix
       REAL,DIMENSION(30)               :: indx
       REAL                             :: sum, d

       DO j = 1,zeilen
         DO k = 1,spalten
           inverse(j,k) = 0
           ursprung(j,k) = matrix(j,k)
         ENDDO ! k
         inverse(j,j) = 1
       ENDDO ! j

       CALL ludcmp(matrix,zeilen,spalten,indx,d)

       DO j = 1,zeilen
         CALL lubksb(matrix,zeilen,spalten,indx,inverse(1,j))
       ENDDO ! j

!       WRITE(6,*) 'check inverse matrix'
       DO j = 1,zeilen
         DO k = 1,spalten
           sum = 0
           DO i = 1,zeilen
             sum = sum+ursprung(i,j)*inverse(k,i)
           ENDDO ! i
           einheit(j,k) = sum
         ENDDO ! k
!          WRITE(6,'(38f10.6)') (einheit(j,k),k=1,spalten)
       ENDDO ! j
       RETURN

       ENDSUBROUTINE

       INCLUDE './lubksb.f90'
       INCLUDE './ludcmp.f90'
