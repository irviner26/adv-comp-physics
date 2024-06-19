PROGRAM secant_method

    IMPLICIT NONE
    INTEGER, PARAMETER :: dpr = KIND(1.0D0)
    REAL(dpr) :: prev_root, prev_prev_root, next_root, upper_bound, relative_error, i

    i = 0
    prev_root = 0.5
    upper_bound = 1.5

    DO 
        i=i+1

        IF(i < 3) THEN
            next_root = (upper_bound*funct(prev_root) - prev_root*funct(upper_bound)) &
            /(funct(prev_root) - funct(upper_bound))
        ELSE
            next_root = (prev_prev_root*funct(prev_root) - prev_root*funct(prev_prev_root)) &
            /(funct(prev_root) - funct(prev_prev_root))
        END IF
            
        relative_error = ABS((prev_root - next_root)/(next_root))
        IF (relative_error <= 0.001) EXIT

        prev_prev_root = prev_root
        prev_root = next_root
    END DO

    WRITE(*,*) 'f(x) = 0, when x = ',next_root
    WRITE(*,*) 'with iterations of ',i

    STOP 
    CONTAINS

    FUNCTION funct(x) RESULT(y)

        IMPLICIT NONE
        REAL(dpr), INTENT(IN) :: x
        REAL(dpr) :: y

        y = EXP(x) - 2

        RETURN
        
    END FUNCTION funct
END PROGRAM