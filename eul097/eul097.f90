!************************************************************
! Project Euler - Problem # 97
!
! Find the last ten digis of the non-Mersenne prime 28433*2^7830457+1
!
! Solved by Eric Jones
!
!************************************************************
program digitfind
  implicit none
  Integer*8 :: solution, solutionPart1, i
  solution=2
  do i=2, 7830457
    solution=solution*2
    solution=mod(solution,10000000000)
  end do
  solutionPart1=solution
  do i=2, 28433
    solution=solution+solutionPart1
    solution=mod(solution,10000000000)
  end do
  solution=solution+1
  print '(A,I10.10)', "Digits are ", solution
end Program
