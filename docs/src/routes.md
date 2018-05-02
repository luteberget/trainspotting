# Interlocking 🔒

An **interlocking** is the safety-critical low-level control system in railways, tasked with ensuring that all actions involving signals and movable elements are performed in a safe manner.

The main operating mode of the interlocking is to take requests for **setting a route**, and then moving track elements and giving signals if this route is safe to set.  Routes are the **atomic unit of allocation** from the dispatcher's point of view.
Note that the atomic unit of de-allocation is typically smaller than the route, 
a feature of interlockings which is called *partial release*.

The *routes file* contains set of whitespace-separated *route statements*,
each declared by the following syntax:

```
route <name> {
  entry <signal-name>
  exit <signal-name>
  entrysection <section-name>
  length <number>
  sections [<section-name>, ...]
  switches [<switch-name> <switch-position>, ...]
  contains [<node-name>, ...]
  <release>*
}
```

The lines in the above route statement contain the following:

* A route has a *name*, which is the identifier used 
from the dispatch plan to request this route for activation.
* The *entry* is an identifier for a signal which is used to 
signal the movement authority given by this route when it is activated.
* The *exit* is an identifier for a signal which is not directly
used for simulation, but which is need for planning for knowing
which routes can extend this route.
* The *entry section* is an identifier for a detection section.
When this detection section becomes occupied, the route is considered
to be in use, and the signal stops signalling movement authority to avoid
other trains from using the same route before it has been activated again.
* The *length* is a number indicating the driving length of the route path.
This length is used by the planner, and also by the simulator in the 
communication of movement authority from the control system to the train, 
in case of a signalling system which transfers movement authority lengths
(either on-line, through train protection systems or radio communication, or off-line, by
having the train driver familiarize with lengths though a railway line's handbook).
This length is not adjusted for stopping margins.
* The *section list* is a comma-separated list of section which 
must be exclusively allocated before the route can be activated.
Sections should be listed in the order of occupancy by a train running the route path.
* The *switch list* is a comma-separated list of switches and corresponding 
positions (e.g. `sw1 left`).
* The *contains list* is a comma-separated list of node which the route contains.
It is used for planning, to fulfil requirements of visiting specific nodes by
activating routes.
* Finally, a list of *release conditions* determine how the resources required by the
route are de-allocated. The syntax is as follows

  ```
  release {
    length <number>
    trigger <section-name>
    resources [<resource-name>, ...]
  }
  ```

  The lines in the above release statement contain the following:
    * The *length* is a number indicating the length of the partial route path.
    * The *trigger* is an identifier for a detection section. After this
    trigger has turned occupied and then subsequently vacant again, the resources are released.
    * The *release list* is a comma-separated list of resources, i.e. sections and switches,
    which cease to be allocated after the trigger event has occurred.

  Note that if no releases are specified, a default release condition is added which
  frees the whole length and all resources, using the last section in the path as the trigger.


## Entry / exit routes

Two special cases of route syntax exist for the entry and exit from the model.

```
modelentry <name> from <boundary-name> {
  exit <signal-name>
  (length, section, switches, contains as in route)
}
```

```
modelexit <name> to <boundary-name> {
  entry <signal-name>
  entrysection <detector-name>
  (length, section, switches, contains as in route)
}
```


## Overlap / safety zone

Many railway administrations require a given length of track after the 
exit signal to be vacant or exclusively allocated for a route to be activated.
This track section is called *overlap* (british) or *safety zone* (international).
Overlaps require special logic for allowing the extension of a route 

Overlaps are currently not supported.

## Flank protection

Flank protection can be implemented by adding requirements of exclusive allocation of resources such as detection sections and switches.

