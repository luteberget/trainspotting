# History 📈

The **history** is the main output from the `rolling` dispatch mode.

## Full history

The full history consists of separate histories for the infrastructure, and of each train. The format is:

* A list of *infrastructure / interlocking* events:
   * **Wait**: time passing
   * **Route**: status of a route, pending/active/released.
   * **Authority**: a signal's given movement authority: None or a length.
   * **Reserved**: the lock status (locked = true/false) for exclusive reservation of an infrastructure resource (switch or detection section)
   * **Occupied**: detection section's occupancy status (occupied = true/false)
   * **Position**: position of movable track element (switch left/right)
* A list of for each train containing the history of its dynamics:
   * **Wait**: time passing. This form is generally only used when the train is not in the *running* state, as time deltas when running are given in the *Move* item.
   * **Node**: front of train arrives at node
   * **Edge**: front of train begins traveling along edge from source node to target node, with a specified distance. The target node may be omitted when the train is exiting the model.
   * **Sight**: the train starts or stops receiving information from an infrastructure object 
   * **Move**: a time delta, a driver action (accelerate/brake/coast), a distance delta and the train's updated velocity.

An example of contents (the specifics of the formatting are likely to change):

```
# Infrastructure history:
> Route(0, Pending)
> Route(0, Active)
> Route(1, Pending)
> Reserved(2, true)
> Authority(0, Some(1750.0))
> Route(1, Active)
> Wait(1.4142135623730951)
> Wait(8.585786437626904)
> Wait(15.0)
> Wait(0.10000000000000142)
> Wait(4.899999999999999)
> Occupied(2, true)

## Train "t1" TrainParams { length: 200.0, max_acc: 1.0, max_brk: 0.9, max_vel: 10.0 }:
> Node(0)
> Node(1)
> Edge(1, Some(2))
> Move(1.4142135623730951, Accel, DistanceVelocity { dx: 1.0000000000000002, v: 1.4142135623730951 })
> Node(2)
> Sight(0, true)

```

## Simplified history

The simplified history is a list of trains' arrival times at nodes.
This output format is used for example by the verification mode to 
check whether timing constraints are satisfied.

Example:

```
t1 0 b1
t1 0 n1
t1 1.4142135623730951 n2
t1 1.4142135623730951 n3
t1 30 n4
t1 30 n5
```


