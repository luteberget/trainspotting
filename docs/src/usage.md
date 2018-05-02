# Usage 📆

The **usage** specification contains high-level description of usage and performance 
requirements for railway infrastructure.

## Vehicles

Declares vehicle types for use in movements.

```
vehicle <name> length <number> accel <number> brake <number> maxspeed <number>
```

Example:

```haskell
vehicle passengertrain length 220.0 accel 1.0 brake 0.9 maxspeed 55.0
```

## Movements

Declares a **movement**, which means that a train of specified type has to perform the
listed visits in the listed order.

Each visit may optionally have a waiting time, which causes the train to dwell
at the node for a specified amount of time.
The last visit in a movement may have an infinite wait time (`inf`), meaning
that the train should not move from its position after arriving at the node.
**Wait times are currently not supported.**

```
movement <vehicle-name> {
   visit [#<name>] [<node-name>, <node-name>, ...] [wait <number>]
   visit ...
   ...
}
```

Example:

```haskell
movement passengertrain {
  visit #start_p1 [inLeft, inRight]
  visit #stop_p1 [sig1] wait 60.0
  visit #end_p1 [outRight]
}
```

## Timing

A timing statement declares an ordering of named visits, as defined in the movements.
The visits may refer to named visits in two different trains. 
The timing statement has an optional time parameter which may be used to set an upper bound
on the time passing from the first to the second visit.

```
timing <visit-name> <visit-name> [<number>]
```
  
Example

``` haskell
timing start_p1 start_p2
timing end_p2 end_1
timing start_p1 end_p1 120.0
```

