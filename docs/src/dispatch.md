# Dispatch

The dispatch plan file represents a timeline of the following types of events:

 * Trains entering the infrastructure from a given entry route.
 * A dispatcher requesting the interlocking to activate train routes.

The `train` and `route` statements can be seen as launching concurrent processes in the simulation.

## Statements

### Train

A train starting at the current time is described by the following syntax:

```
train <name> l=<number> a=<number> b=<number> v=<number> <route-name>
```

The parameters are:

 * **Name** is an identifier for the train.
 * **Length** `l=<number>` sets the length of the train in meters.
 * **Acceleration** `a=<number>` sets the maximum acceleration of the train in N/m².
 * **Braking deceleration** `b=<number>` sets the maximum deceleration of the train in N/m².
 * **Maximum velocity** `v=<number>` sets the maximum velocity of the train in m/s.
 * **Route name** `<route-name>` names an [entry route](./routes.md) which must be activated before the train enters from the entry routes' specified model boundary.

The train, seen as a process, executes in three stages:

1. **Activation**: the given entry route of the train must be activated. If its resources are occupied, or it is waiting for movable elements to come in to their positions, the train does not yet enter the model and waits for the route activation process of the entry route to finish.
2. **Running**: the train is partially or fully inside the infrastructure model. It runs by:
   * travelling according to its given movement authority
   * setting the occupied flag on sections it enters
   * receiving updated movement authority from signal which are in sight
3. **Finished**: when the train has exited through a model boundary, it is removed from the model and considered finished.

### Route

A route request dispatches at the current time is represented using the following syntax:

```
route <route-name>
```

This statement starts a route activation process. The process will wait until all resources are available before reserving the resources and starting to move any movable elements. This means that two conflicting routes may be requested, after which the first route's activation will finish, and the second route to be requested will remain waiting until all required resources are available.

### Wait

Passing time is represented by a separate statement:

```
wait <number>?
```

The `<number>` parameter gives the time to pass in seconds. Note that no time passes after starting processes unless explicitly described by a `wait` statement.

If there are no further statements in the dispatch plan, the simulation will run until no more processes are scheduled, or all processes are stuck, such as when a train is waiting at a red light, but no more routes will be dispatched.

The `wait` statement may be given without a time parameter. This will cause the simulation to wait until all route activations are finished (including route activations which are part of a train's activation process). This may be used to ensure that one set of route allocations happen before another. For example, the *verification mode* produces a plan without wait times, but which has a series of *steps* which need to happen in the given order. The un-timed `wait` statement acts as a synchronization barrier for the route activation processes which are currently running.


