def eul001():
  limit = 1000
  multiplesSum = 0
  for x in xrange(0,limit):
    if (x % 3 == 0) or (x % 5 == 0):
      multiplesSum = multiplesSum + x
  print "The sum of all multiples of 3 or 5 below " + str(limit) + " is " + str(multiplesSum)

eul001()
