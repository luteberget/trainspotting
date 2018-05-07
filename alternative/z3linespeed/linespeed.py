from z3 import *
import timeit

def toSMT2Benchmark(f, status="unknown", name="benchmark", logic=""):
  v = (Ast * 0)()
  return Z3_benchmark_to_smtlib_string(f.ctx_ref(), name, logic, status, "", 0, v, f.as_ast())


start_time = timeit.default_timer()
#print "Building constraints"

s = Solver()#Tactic('qfnra').solver()
constraints = []

# (length, maxvelocity)
segmentSpec = [
(1000.0,50.0), (1000.0, 60.0)
]
# (length, maxvelocity)
trainLength = 50.0
trainMaxVelocity = 60.0
trainMaxAcceleration = 2.0
trainMaxBraking = 1.0

maxSegmentLength = max([x[0] for x in segmentSpec])
totalLength = sum([x[0] for x in segmentSpec])
maxConsecutiveSegmentPairsLength = max([x[0] + y[0] for (x,y) in zip(segmentSpec, segmentSpec[1:])])

SegmentSort, segmentsvalues = EnumSort('Segment', 
  map(lambda x: "Segment"+str(x), list(range(len(segmentSpec)))))
#print "Segments: ", SegmentSort, segmentsvalues

# State is a tuple of variables (frontSegment, frontOffset, backSegment, backOffset, velocity)
def boundaryState(backSegment,backOffset,velocity):
  frontSegment = backSegment
  frontOffset = backOffset + trainLength
  while frontOffset > segmentSpec[frontSegment][0]:
    frontOffset -= segmentSpec[frontSegment][0]
    frontSegment += 1
    if frontSegment >= len(segmentSpec):
      raise Exception("no space for train in boundary state")
  return (segmentsvalues[frontSegment], frontOffset, segmentsvalues[backSegment], backOffset, velocity)

def interiorState(n):
  #print "Creating interior state"

  # State
  frontSegment = Const("frontSegment_" + str(n), SegmentSort)
  backSegment = Const("backSegment_" + str(n), SegmentSort)
  frontOffset = Real("frontOffset_" + str(n))
  constraints.append(frontOffset >= 0.0)
  constraints.append(frontOffset <= maxSegmentLength)
  backOffset = Real("backOffset_" + str(n))
  constraints.append(backOffset >= 0.0)
  constraints.append(backOffset <= maxSegmentLength)
  velocity = Real("velocity_" + str(n))
  constraints.append(velocity >= 0.0)
  constraints.append(velocity <= trainMaxVelocity)

  # Constraint: offsets fit in segments
  for (segment, (segmentLength, segmentVelocity)) in zip(segmentsvalues, segmentSpec):
    x1 = Implies(frontSegment == segment, frontOffset <= segmentLength)
    x2 = Implies(backSegment == segment, backOffset <= segmentLength)
    constraints.append(x1)
    constraints.append(x2)

    v1 = Implies(frontSegment == segment, velocity <= segmentVelocity)
    v2 = Implies(backSegment == segment, velocity <= segmentVelocity)
    constraints.append(v1)
    constraints.append(v2)

  return (frontSegment, frontOffset, backSegment, backOffset, velocity)

# Takes two states (5-tuples), and returns a tuple (delta_t, delta_x)
def transition(n,s1,s2):
  #print "Transition ", n, s1, s2

  delta_t = Real("delta_t_" + str(n))
  constraints.append(delta_t >= 0.0)
  constraints.append(delta_t <= totalLength)

  delta_x = Real("delta_x_" + str(n))
  constraints.append(delta_x >= 0.0)
  constraints.append(delta_x <= maxConsecutiveSegmentPairsLength)

  # Constraint: Trains move to consecutive segments
  for ((segment, segmentNext), (segmentLength, segmentVelocity)) in zip(zip(segmentsvalues, segmentsvalues[1:] + [None]), segmentSpec):
    s1FrontSegment = s1[0]
    s1BackSegment = s1[2]
    s2FrontSegment = s2[0]
    s2BackSegment = s2[2]
    s1FrontOffset = s1[1]
    s1BackOffset = s1[3]
    s2FrontOffset = s2[1]
    s2BackOffset = s2[3]

    s1Velocity = s1[4]
    s2Velocity = s2[4]


    # Front and back can move 0 or 1 segments, and move distance delta_x.
    if segmentNext != None:
    	x1 = Implies(s1FrontSegment == segment, Or(s2FrontSegment == segment, s2FrontSegment == segmentNext))
    	x2 = Implies(s1BackSegment == segment, Or(s2BackSegment == segment, s2BackSegment == segmentNext))
        x3 = Implies(And(s1FrontSegment == segment, s2FrontSegment == segment), 
                     (s1FrontOffset + delta_x) == s2FrontOffset)
        x4 = Implies(And(s1FrontSegment == segment, s2FrontSegment == segmentNext), 
                     (s1FrontOffset + delta_x) == (s2FrontOffset + segmentLength) )
        x5 = Implies(And(s1BackSegment == segment, s2BackSegment == segment), 
                     (s1BackOffset + delta_x) == s2BackOffset)
        x6 = Implies(And(s1BackSegment == segment, s2BackSegment == segmentNext), 
                     (s1BackOffset + delta_x) == (s2BackOffset + segmentLength) )
        constraints.append(x1)
        constraints.append(x2)
        constraints.append(x3)
        constraints.append(x4)
        constraints.append(x5)
        constraints.append(x6)
    else:
    	x1 = Implies(s1FrontSegment == segment, s2FrontSegment == segment)
    	x2 = Implies(s1BackSegment == segment, s2BackSegment == segment)
    	x3 = Implies(s1FrontSegment == segment, s2FrontOffset == s1FrontOffset + delta_x)
    	x4 = Implies(s1BackSegment == segment, s2BackOffset == s1BackOffset + delta_x)
        constraints.append(x1)
        constraints.append(x2)
        constraints.append(x3)
        constraints.append(x4)

  # Velocities are attainable given maximum acceleration and braking
  vdiff  = s1Velocity - s2Velocity
  vdiff2 = vdiff*vdiff
  tv1    = delta_t*s1Velocity
  tv2    = delta_t*s2Velocity

  isAccelerating = s1Velocity <= s2Velocity
  
  acc = trainMaxAcceleration
  brk = trainMaxBraking
  # Braking: 2*b*dt*v1 + (v1-v0)^2 <= 2*b*dx <= 2*b*dt*v0 - (v1-v0)^2
  c1 = (2*brk)*delta_x <= (2*brk)*tv1 - vdiff2
  c2 = (2*brk)*tv2 + vdiff2 <= (2*brk)*delta_x

  c3 = (2*acc)*delta_x <= (2*acc)*tv2 - vdiff2
  c4 = (2*acc)*tv1 + vdiff2 <= (2*acc)*delta_x

  constraints.append(Implies(Not(isAccelerating),c1))
  constraints.append(Implies(Not(isAccelerating),c2))
  constraints.append(Implies(isAccelerating,c3))
  constraints.append(Implies(isAccelerating,c4))

  return (delta_t,delta_x)

startState =  boundaryState(0,0,0)
endState =  boundaryState(len(segmentSpec)-1, segmentSpec[len(segmentSpec)-1][0] - trainLength, 0)
#print "boundary state start: ", startState
#print "boundary state end:   ", endState

nsteps = 2

states      = [startState] + [interiorState(n) for n in range(nsteps-1)] + [endState]
transitions = [transition(n,s1,s2) for (n,(s1,s2)) in zip(range(len(states)), zip(states,states[1:]))]

#totalTime = sum(map(lambda transition: transition[0], transitions))
#print "total time ", totalTime

# x = Real('x')
# y = Real('y')
# constraints.append(x + y > 5, x > 1, y > 1)

elapsed = timeit.default_timer() - start_time
print "Building constraints took ", elapsed
for x in constraints: s.add(x)
print s

smtlibstring = toSMT2Benchmark(And(constraints), status="unknown", name="benchmark", logic="")
with open('output.smt2', 'w') as the_file:
    the_file.write(smtlibstring)


start_time = timeit.default_timer()
issat = s.check()
elapsed = timeit.default_timer() - start_time
print "Solving took ", elapsed
print issat
model = s.unsat_core()
print model
print "Z3 version", z3.get_version()
