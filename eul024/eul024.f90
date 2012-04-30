!************************************************************
! Project Euler - Problem # 24
!
! Find the millionth lexicographic permutation of the digits
! 0-9.
!
! Solved by Eric Jones
!
!************************************************************
RECURSIVE FUNCTION factorial(n) RESULT(res)
  INTEGER res, n
  IF(n.EQ.0) THEN
    res = 1
  ELSE
    res = n*factorial(n-1)
  END IF
END

program lexicoperm
  implicit none
  integer*4 :: lexicoArray(0:9), permSum, i, fact, numIterations, j, k, factorial, origPermSum
  logical :: digitUsed(0:9)
  print *, "Enter the permutation to calculate to: "
  read *, origPermSum
  permSum=origPermSum
  !permSum=1000000
  lexicoArray(:)=0
  do i=10, 1, -1
    fact=factorial(i-1)
    !print *, "Factorial calculated as ", fact
    numIterations=permSum/fact
    !print *, "Number of iterations of this digit ", numIterations
    permSum=permSum-numIterations*fact
    !print *, "New number of permutations ", permSum
    if ( permSum == 0 ) then
      numIterations=numIterations - 1
    end if
    j=0
    do k=0, 9, 1
      if ( digitUsed(k) == .FALSE. ) then
        j=j+1
        if ( j == numIterations+1 ) exit
      end if
    end do
    digitUsed(k) = .TRUE.
    lexicoArray(i-1) = k
    !print *, "New digit selected ", k
    if ( permSum == 0 ) then
      exit
    end if
  end do
  if ( i > 1 ) then
    !print *, "beginning assignment of remaining digits starting with digit ", i
    do i=i-1, 1, -1
      do k=9, 0, -1
        if ( digitUsed(k) == .FALSE. ) then
          lexicoArray(i-1)=k
          digitUsed(k) = .TRUE.
          exit
        end if
      end do
    end do
  end if
  write (*,'(A,I0,A,10I0)') "The ", origPermSum ,"th lexicographic of digits 0-9 is ", lexicoArray(9:0:-1)
end program

