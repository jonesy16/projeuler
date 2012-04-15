!************************************************************
! Project Euler - Problem # 10
!
! Find the sum of all primes below 2,000,000
!
! Solved by Eric Jones
!
!************************************************************
program primesumfind
  implicit none
  integer*4 ::  i, testnum, targetnum=2E6, upperlim
  integer*8 :: primesum
  testnum=3
  primesum=5
  do 
    testnum=testnum+2
    if ( testnum >= targetnum ) then
      write (*,'(A,I0,A,I0)') "The sum of primes below ", targetnum, " is ", primesum
      exit
    end if
    i=3
    upperlim=sqrt(real(testnum))
    if ( mod(testnum,5) /= 0 ) then
      do i=3, upperlim, 2
        if ( mod(testnum,i) == 0 ) exit
      end do
    end if
    if ( i > upperlim ) then
      primesum=primesum+testnum
    end if
  end do
end program
