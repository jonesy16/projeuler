!************************************************************
! Project Euler - Problem # 48
!
! Find the last ten digis of the series 1^1 + 2^2 + ... 1000^1000
!
! Solved by Eric Jones
!
!************************************************************
program powersum
  implicit none
  Integer*8 :: solution, summation
  Integer*4 :: i, j, k
  summation=0
  do i=1, 1000
    solution=i
    do j=2, i
      solution=mod(solution*i,10000000000)
    end do
    summation=mod(summation+solution,10000000000)
  end do
  print '(A,I10.10)', "Digits are ", summation
end Program
