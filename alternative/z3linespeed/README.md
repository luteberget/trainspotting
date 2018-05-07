# Experiments with Z3 non-linear real arithmetic and booleans

The `linespeed.py` file in this folder demonstrates an SMT encoding which
is a small subset of constraints which are relevant in the 
[`rolling` and `railperfcheck`](https://luteberget.github.io/rollingdocs/)
tools.

The solver fails, returning `unknown`. There might be errors in the encoding of the problem,
but how can one investigate this?

Tested on :
* Z3 version 4.6 
  ```
  Solving took  0.0152609348297
  unknown
  []
  Z3 version (4L, 6L, 0L, 0L)
  ```
* Z3 version 4.5
  ```
  Solving took  0.0147528648376
  unknown
  []
  Z3 version (4L, 5L, 0L, 0L)
  ```

Some explanations of non-linear real arithmetic in Z3:

* https://stackoverflow.com/questions/21284266/can-z3-always-give-result-when-handling-nonlinear-real-arithmetic
* https://stackoverflow.com/questions/14821687/encoding-returns-unknown/

## Full output
```
Building constraints took  0.0132670402527
[frontOffset_0 >= 0,
 frontOffset_0 <= 1000,
 backOffset_0 >= 0,
 backOffset_0 <= 1000,
 velocity_0 >= 0,
 velocity_0 <= 60,
 Implies(frontSegment_0 == Segment0, frontOffset_0 <= 1000),
 Implies(backSegment_0 == Segment0, backOffset_0 <= 1000),
 Implies(frontSegment_0 == Segment0, velocity_0 <= 50),
 Implies(backSegment_0 == Segment0, velocity_0 <= 50),
 Implies(frontSegment_0 == Segment1, frontOffset_0 <= 1000),
 Implies(backSegment_0 == Segment1, backOffset_0 <= 1000),
 Implies(frontSegment_0 == Segment1, velocity_0 <= 60),
 Implies(backSegment_0 == Segment1, velocity_0 <= 60),
 delta_t_0 >= 0,
 delta_t_0 <= 2000,
 delta_x_0 >= 0,
 delta_x_0 <= 2000,
 Implies(Segment0 == Segment0,
         Or(frontSegment_0 == Segment0,
            frontSegment_0 == Segment1)),
 Implies(Segment0 == Segment0,
         Or(backSegment_0 == Segment0,
            backSegment_0 == Segment1)),
 Implies(And(Segment0 == Segment0,
             frontSegment_0 == Segment0),
         50 + delta_x_0 == frontOffset_0),
 Implies(And(Segment0 == Segment0,
             frontSegment_0 == Segment1),
         50 + delta_x_0 == frontOffset_0 + 1000),
 Implies(And(Segment0 == Segment0,
             backSegment_0 == Segment0),
         0 + delta_x_0 == backOffset_0),
 Implies(And(Segment0 == Segment0,
             backSegment_0 == Segment1),
         0 + delta_x_0 == backOffset_0 + 1000),
 Implies(Segment0 == Segment1, frontSegment_0 == Segment1),
 Implies(Segment0 == Segment1, backSegment_0 == Segment1),
 Implies(Segment0 == Segment1,
         frontOffset_0 == 50 + delta_x_0),
 Implies(Segment0 == Segment1,
         backOffset_0 == 0 + delta_x_0),
 Implies(Not(velocity_0 >= 0),
         2*delta_x_0 <=
         2*delta_t_0*0 - (0 - velocity_0)*(0 - velocity_0)),
 Implies(Not(velocity_0 >= 0),
         2*delta_t_0*velocity_0 +
         (0 - velocity_0)*(0 - velocity_0) <=
         2*delta_x_0),
 Implies(velocity_0 >= 0,
         4*delta_x_0 <=
         4*delta_t_0*velocity_0 -
         (0 - velocity_0)*(0 - velocity_0)),
 Implies(velocity_0 >= 0,
         4*delta_t_0*0 + (0 - velocity_0)*(0 - velocity_0) <=
         4*delta_x_0),
 delta_t_1 >= 0,
 delta_t_1 <= 2000,
 delta_x_1 >= 0,
 delta_x_1 <= 2000,
 Implies(frontSegment_0 == Segment0,
         Or(Segment1 == Segment0, Segment1 == Segment1)),
 Implies(backSegment_0 == Segment0,
         Or(Segment1 == Segment0, Segment1 == Segment1)),
 Implies(And(frontSegment_0 == Segment0,
             Segment1 == Segment0),
         frontOffset_0 + delta_x_1 == 1000),
 Implies(And(frontSegment_0 == Segment0,
             Segment1 == Segment1),
         frontOffset_0 + delta_x_1 == 2000),
 Implies(And(backSegment_0 == Segment0,
             Segment1 == Segment0),
         backOffset_0 + delta_x_1 == 950),
 Implies(And(backSegment_0 == Segment0,
             Segment1 == Segment1),
         backOffset_0 + delta_x_1 == 1950),
 Implies(frontSegment_0 == Segment1, Segment1 == Segment1),
 Implies(backSegment_0 == Segment1, Segment1 == Segment1),
 Implies(frontSegment_0 == Segment1,
         frontOffset_0 + delta_x_1 == 1000),
 Implies(backSegment_0 == Segment1,
         backOffset_0 + delta_x_1 == 950),
 Implies(Not(velocity_0 <= 0),
         2*delta_x_1 <=
         2*delta_t_1*velocity_0 -
         (velocity_0 - 0)*(velocity_0 - 0)),
 Implies(Not(velocity_0 <= 0),
         2*delta_t_1*0 + (velocity_0 - 0)*(velocity_0 - 0) <=
         2*delta_x_1),
 Implies(velocity_0 <= 0,
         4*delta_x_1 <=
         4*delta_t_1*0 - (velocity_0 - 0)*(velocity_0 - 0)),
 Implies(velocity_0 <= 0,
         4*delta_t_1*velocity_0 +
         (velocity_0 - 0)*(velocity_0 - 0) <=
         4*delta_x_1)]
Solving took  0.0152609348297
unknown
[]
Z3 version (4L, 6L, 0L, 0L)
```
