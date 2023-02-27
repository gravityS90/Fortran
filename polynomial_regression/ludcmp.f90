      SUBROUTINE ludcmp(a,n,np,indx,d)

      INTEGER               :: n,np
      INTEGER,PARAMETER     :: nmax=100,tiny=1.0E-20
      REAL,DIMENSION(np,np) :: a
      REAL,DIMENSION(nmax)  :: vv
      REAL,DIMENSION(n)     :: indx
      
      d = 1.
      DO i = 1,n
        aamax = 0.

        DO j = 1,n
          IF(ABS(a(i,j)) .GT. aamax) aamax = ABS(a(i,j))
        ENDDO ! j

        IF(aamax .EQ. 0.) PAUSE 'Singular matrix.'
        vv(i)=1./aamax
      ENDDO ! i

      DO j = 1,n
        IF(j .GT. 1) THEN
          DO i = 1,j-1
            SUM = a(i,j)

            IF(i .GT. 1) THEN
              DO k = 1,i-1
                SUM = SUM-a(i,k)*a(k,j)
              ENDDO ! k
              a(i,j) = SUM
            ENDIF ! if 

          ENDDO ! i
        ENDIF ! if 

        aamax=0.
        DO i = j,n
          SUM = a(i,j)

          IF(j .GT. 1) THEN
            DO K = 1,J-1
              SUM = SUM-a(i,k)*a(k,j)
            ENDDO ! k
            a(i,j)=SUM
          ENDIF ! if 

          dum=vv(i)*ABS(SUM)
          IF(dum .GE. aamax) THEN
            imax = i
            aamax = dum
          ENDIF ! if

        ENDDO ! i

        IF(j .NE. imax) THEN
          DO k = 1,N
            dum = a(imax,k)
            a(imax,k) = a(j,k)
            a(j,k) = dum
          ENDDO ! k

          d = -d
          vv(imax) = vv(j)
        ENDIF ! if

        indx(j) = FLOAT(imax)
        IF(j .NE. n) THEN
          IF(a(j,j) .EQ. 0.) a(j,j)=tiny
          dum = 1./a(j,j)

          DO i = j+1,n
            a(i,j) = a(i,j)*dum
          ENDDO ! i

        ENDIF ! if

      ENDDO ! j

      IF(a(n,n) .EQ. 0.) a(n,n)=tiny

      RETURN
      ENDSUBROUTINE
