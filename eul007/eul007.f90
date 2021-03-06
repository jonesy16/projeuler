!************************************************************
! Project Euler - Problem # 7
!
! Find the 10001st prime number
!
! Solved by Eric Jones
!
!************************************************************
program primefind
  implicit none
  integer*4 ::  i, numprimes, lastprime, testnum, targetnum=10001, upperlim
  testnum=3
  numprimes=2
  do 
    testnum=testnum+2
    upperlim=sqrt(real(testnum))
    do i=3, upperlim, 2
      if ( mod(testnum,i) == 0 ) exit
    end do
    if ( i > upperlim ) then
      lastprime=testnum
      numprimes=numprimes+1
    end if
    if ( numprimes == targetnum ) then
      write (*,'(A,I0,A,I0)') "The ", targetnum, "st prime number is ", lastprime
      exit
    end if
  end do
end program
