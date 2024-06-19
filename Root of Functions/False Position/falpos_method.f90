PROGRAM falpos_method
    
    IMPLICIT NONE
    INTEGER, PARAMETER :: dpr = KIND(1.0D0)
    REAL(dpr) :: lower_bound, upper_bound, center_bound, previous_center, relative_error

    lower_bound = 0.5
    upper_bound = 1

    DO 
        previous_center = upper_bound

        center_bound = (lower_bound * funct(previous_center) - previous_center * funct(lower_bound)) & 
        /(funct(previous_center) - funct(lower_bound))

        relative_error = ABS((previous_center - center_bound)/(center_bound))

        IF (relative_error <= 0.001) EXIT

        upper_bound = center_bound
    END DO

    WRITE(*,*) 'f(x) = 0, when x = ',center_bound

    STOP 
    CONTAINS

    FUNCTION funct(x) RESULT(y)

        IMPLICIT NONE
        REAL(dpr), INTENT(IN) :: x
        REAL(dpr) :: y

        y = cos(x) - x

        RETURN
        
    END FUNCTION funct

END PROGRAM