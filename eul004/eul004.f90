!************************************************************
! Project Euler - Problem # 4
!
! Find the largest palindrome made from the product of two
! 3-digit numbers
!
! Solved by Eric Jones
!
!************************************************************
program threedigpal
  implicit none
  integer*2 :: limit=999, stringlength, k
  integer*4 :: prod, multA, multB, i, j, palindrome
  character*6 :: prodstringA, prodstringB
  palindrome=0
  do i=limit, 1, -1
    do j=limit, 1, -1
      prod=i*j
      !- palindrome check
      prodstringA=""
      prodstringB=""
      write (prodstringA, '(I0)') prod
      do k=1, 6
        prodstringB(7-k:7-k)=prodstringA(k:k)
      end do
      if ( trim(prodstringA) == trim(prodstringB) ) then
        palindrome=prod
      end if
      if ( palindrome /= 0 .and. prod < palindrome ) exit
    end do
  end do
  write (*,'(A,I0,A,I0)') "The largest palindrome is ", palindrome
end program
