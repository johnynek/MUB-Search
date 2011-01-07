#!/usr/bin/env python

import math, sys
from itertools import izip

mubs = {}
r = int(sys.argv[1]) 
#this is for d=6:
#
# d = 2: [1, 1], [1, -1]
#        [1, i], [1, -i]
mub2 = ( [ [0.0], [0.5] ],
         [ [0.25], [0.75] ] )
mubs[2] = mub2
# d = 3: w^3 = 1, w*w =v, v*v = w
# [1, 1, 1], [1, w, v], [1, v, w]
# [1, w, w], [1, v, 1], [1, 1, v]
# [1, v, v], [1, 1, w], [1, w, 1]
z = 0.0; w = 1.0/3.0; v = 2.0/3.0
mub3 = ( [ [z, z], [w, v], [v, w] ],
         [ [w, w], [v, z], [z, v] ],
         [ [v, v], [z, w], [w, z] ] )
mubs[3] = mub3

#Make product mubs:
def tensor_prod_v(vec0, vec1):
  this_res = [ int(r*v) for v in vec0 ]
  this_res.extend(int(r*v) for v in vec1)
  for comp0 in vec0:
    for comp1 in vec1:
      this_res.append(int(r * math.modf(comp0 + comp1)[0]))
  return this_res
   
def tensor_prod_b(basis0, basis1):
  return [ tensor_prod_v(vec0, vec1) for vec0 in basis0 \
                              for vec1 in basis1 ]

def make_prod_mub(mub0, mub1):
  return [ tensor_prod_b(basis0, basis1) \
           for basis0, basis1 in izip(mub0,mub1) ]

print make_prod_mub(mub2, mub3)
