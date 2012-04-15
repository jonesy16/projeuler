!************************************************************
! Project Euler - Problem # 6
!
! Find the difference between the sum of the squares of the
! first hundred natural numbers and the square of the sum.
!
! Solved by Eric Jones
!
!************************************************************
program squarediff
  implicit none
  integer*2 :: limit=100, i
  integer*4 :: sumsquare, squaresum
  sumsquare=0
  squaresum=0
  do i=1, limit
    sumsquare=sumsquare+i**2
    squaresum=squaresum+i
  end do
  squaresum=squaresum**2
  write (*,'(A,I0,A,I0)') "Difference of sum of squares and square of sum is ", squaresum - sumsquare
end program
