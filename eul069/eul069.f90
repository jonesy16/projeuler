!************************************************************
! Project Euler - Problem # 69
!
! Find the value of n<=1E6 for which n/phi(n) is a maximum.
! phi(n) = number of numbers less than n which are relatively
! prime to n.
!
! Solved by Eric Jones
!
!************************************************************
program noverphi
  implicit none
  integer*8 :: n, maxN=0, numRelPrimes=0, i, j
  real :: maxNoverPhi=0.0
  logical :: factorList(1:500000)=.TRUE.
  do n=2, 1E6, 2
    if ( mod(n,100) == 0 ) print *, " Checking ", n
    factorList(1:n/2)=.TRUE.
    do i=2, n/2
      if ( factorList(i) == .TRUE. .and. mod(n,2*i-1) == 0 ) then
        j=i
        do
          if ( 2*j-1 > n ) exit
          factorList(j)=.FALSE.
          j=j+2*i-1
        end do
      end if
    end do
    numRelPrimes=count(factorList(1:n/2))
    !print *, " Number of relative primes = ", numRelPrimes
    !print *, " N/Phi(n) = ", real(n)/real(numRelPrimes)
    if ( real(n)/real(numRelPrimes) > maxNoverPhi ) then
      maxNoverPhi = real(n)/real(numRelPrimes)
      maxN=n
    end if
  end do
  write (*,'(A,I0,A,F12.4)') "The max N is ", maxN, " with ", maxNoverPhi
end program

