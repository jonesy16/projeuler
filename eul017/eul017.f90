!************************************************************
! Project Euler - Problem # 17
!
! Find the number of letters used to write out the first
! one thousand numbers starting with 1.
!
! Solved by Eric Jones
!
!************************************************************
program numberwordsum
  implicit none
  integer :: wordList(0:1000)
  integer*4 :: summation, i
  wordList(0)=0
  wordList(1)=3; wordList(2)=3; wordList(3)=5; wordList(4)=4
  wordList(5)=4; wordList(6)=3; wordList(7)=5; wordList(8)=5
  wordList(9)=4; wordList(10)=3; wordList(11)=6; wordList(12)=6
  wordList(13)=8; wordList(14)=8; wordList(15)=7; wordList(16)=7
  wordList(17)=9; wordlist(18)=8; wordList(19)=8; wordList(20)=6
  wordList(30)=6; wordList(40)=5; wordList(50)=5; wordList(60)=5
  wordList(70)=7; wordList(80)=6; wordList(90)=6; wordList(100)=7
  wordList(1000)=8
  summation=0
  do i=1, 1000
    if ( i <= 20 ) then
      summation=summation+wordList(i)
    else if ( i < 100 ) then
      summation=summation+wordList(mod(i,10))+wordList(i-mod(i,10))
    else if ( i < 1000 ) then
      if ( i == 100 ) then
        summation=summation*10
      end if
      if ( mod(i,100) == 0 ) then
        summation=summation+wordList(i/100)+wordlist(100)
      else
        summation=summation+wordList(i/100)+wordlist(100)+3
      end if
    else
      summation=summation+wordList(i/1000)+wordList(1000)
    end if
  end do
  print *, "Sum is ", summation
end program
