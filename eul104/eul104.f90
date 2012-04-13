PROGRAM PANCHECK
  Integer, Parameter :: ARRAYSIZE=100000
  Integer*2, Dimension (1:ARRAYSIZE) :: NUMA,NUMB,NUMC
  Integer :: iCount, i, j, iTestCount, iMaxIndex
  NUMA(:)=0
  NUMB(:)=0
  NUMC(:)=0
  NUMA(1)=1
  NUMB(1)=1
  NUMC(1)=2
  iCount=2
  iMaxIndex=1
  Do
    iCount=iCount+1
    Do i=1, ARRAYSIZE, 1
      If ( NUMC(i) >= 10 ) Then
        NUMC(i)=NUMC(i)-10
        NUMC(i+1)=NUMC(i+1)+1
        If (i == iMaxIndex) Then
          iMaxIndex=iMaxIndex+1
          Exit
        End If
      End If
    End Do
    ! Check Last 9 Digits
    iTestCount=0
    Do i=1, 9
      Do j=1, 9
        If ( NUMC(j) == 0 ) Then
          Exit
        Else If ( NUMC(j) == i ) Then
          iTestCount=iTestCount+1
          Exit
        End If
      End Do
    End Do
    If ( iTestCount == 9 ) Then
      Print *, " Fib number ", iCount, " contains a pandigital number in the last 9 digits"
      ! Check First 9 Digits
      iTestCount=0
      ! Find array index where number begins
      !Do i=ARRAYSIZE, 1, -1
        !If ( NUMC(i) /= 0 ) Then
           !Exit
        !End If
      !End Do
      Do k=1, 9, 1
        Do j=iMaxIndex, iMaxIndex-8, -1
          If ( NUMC(j) == 0 ) Then
            Exit
          Else If ( NUMC(j) == k ) Then
            iTestCount=iTestCount+1
            Exit
          End If
        End Do
      End Do
      If ( iTestCount == 9 ) Then
        Print *, " Fib number ", iCount, " contains a pandigital number in the first and last 9 digits"
        Exit
      End If
    End If
  NUMA(1:iMaxIndex)=NUMB(1:iMaxIndex)
  NUMB(1:iMaxIndex)=NUMC(1:iMaxIndex)
  NUMC(1:iMaxIndex)=NUMA(1:iMaxIndex)+NUMB(1:iMaxIndex)
  End Do
End Program
