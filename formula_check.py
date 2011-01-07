import math, cmath, random

def check():
  a = 2.0 * random.random() - 1.0;
  b = 2.0 * random.random() - 1.0;
  z = complex(a,b)
  zh = cmath.sqrt(z)
  r = math.sqrt(a**2 + b**2)
  zhr = math.sqrt((r+a)/2.0)
  if b < 0: zhr = -zhr
  zhi = math.sqrt((r-a)/2.0)
  try:
    assert (abs(z - complex(zhr, zhi)**2) < 0.000001)
  except:
    print zh, complex(zhr, zhi)

for i in xrange(100000):
  check()

def check_nb():
  a = 2.0 * random.random() - 1.0;
  b = 2.0 * random.random() - 1.0;
  z = complex(a,b)
  zh = cmath.sqrt(z)
  r = math.sqrt(a**2 + b**2)
  zhr = math.sqrt((r+a)/2.0)
  zhi = b/math.sqrt(2*(r+a))
  try:
    assert (abs(z - complex(zhr, zhi)**2) < 0.0001)
  except:
    print zh, complex(zhr, zhi)

for i in xrange(1000):
  check()
  check_nb()
