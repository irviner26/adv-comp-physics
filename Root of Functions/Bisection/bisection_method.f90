PROGRAM bisection_method

    IMPLICIT NONE
    INTEGER, PARAMETER :: dpr = KIND(1.0D0)
    REAL(dpr) :: lower_bound, upper_bound, center_bound, previous_center, relative_error

    lower_bound = 0.5
    upper_bound = 1

    DO 
        center_bound = (lower_bound + upper_bound)/2

        if (funct(lower_bound) * funct(center_bound) < 0) THEN
            upper_bound = center_bound ; previous_center = center_bound
            center_bound = (lower_bound + upper_bound)/2
        ELSE IF (funct(upper_bound) * funct(center_bound) < 0) THEN
            lower_bound = center_bound ; previous_center = center_bound
            center_bound = (lower_bound + upper_bound)/2
        END IF

        relative_error = ABS((previous_center - center_bound)/(center_bound))

        IF (relative_error <= 0.001) EXIT

    END DO

    WRITE(*,*) 'f(x) = 0, when x = ',center_bound

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