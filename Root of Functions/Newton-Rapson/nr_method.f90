PROGRAM nr_method

    IMPLICIT NONE
    INTEGER, PARAMETER :: dpr = KIND(1.0D0)
    REAL(dpr) :: prediction_point, crossing_zero_point, relative_error

    DO
        crossing_zero_point = prediction_point - (funct(prediction_point) / dfunct(prediction_point))
        relative_error = ABS((prediction_point - crossing_zero_point)/(crossing_zero_point))
        IF (relative_error <= 0.001) EXIT
        prediction_point = crossing_zero_point
    END DO

    WRITE(*,*) 'f(x) = 0, when x = ',crossing_zero_point

    STOP 
    CONTAINS

    FUNCTION funct(x) RESULT(y)

        IMPLICIT NONE
        REAL(dpr), INTENT(IN) :: x
        REAL(dpr) :: y

        y = cos(x) - x

        RETURN
        
    END FUNCTION funct

    FUNCTION dfunct(x) RESULT(y)

        IMPLICIT NONE
        REAL(dpr), INTENT(IN) :: x
        REAL(dpr) :: y

        y = -sin(x) - 1

        RETURN
        
    END FUNCTION dfunct
END PROGRAM