!************************************************************
! Project Euler - Problem # 9
!
! Find the product of the Pythagorean triplet for which
! a + b + c = 1000. i.e., find abc
!
! Solved by Eric Jones
!
!************************************************************
program pythfind
  implicit none
  integer*2 ::  targetsum=1000
  integer*4 ::  abcproduct, a, b, c
  do c=targetsum, 1, -1
    do b=c-1, 1, -1
      do a=b-1, 1, -1
        if ( a + b + c == 1000 ) then
          if ( c**2 - b**2 - a**2 == 0 ) then
            abcproduct=a*b*c
            print *, "Product = ", abcproduct
            stop
          end if
        end if
      end do
    end do
  end do
end program
