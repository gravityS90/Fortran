! lubksb.for
      SUBROUTINE lubksb(a,n,np,indx,b)
      INTEGER               :: n,np
      REAL,DIMENSION(n)     :: indx,b
      REAL,DIMENSION(np,np) :: a

      ii = 0

      DO i = 1,N
        ll = INT(indx(i))
        SUM = b(ll)
        b(ll) = b(i)

        IF(ii .NE. 0) THEN
          DO j = ii,i-1
            SUM = SUM-a(i,j)*b(j)
          ENDDO ! j
        ELSEIF (SUM .NE. 0.) THEN
          ii = i
        ENDIF ! if ii

        b(i) = SUM
      ENDDO ! i

      DO i = n,1,-1
        SUM = b(i)
        IF(i .LT. n)THEN
          DO j = i+1,n
            SUM = SUM-a(i,j)*b(j)
          ENDDO ! j
        ENDIF
        b(i) = SUM/a(i,i)
      ENDDO ! i

      RETURN
      ENDSUBROUTINE
