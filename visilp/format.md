## Spec

We need:
 1. a list of nodes with associated mileage (x-axis position)
 2. edges with left/right/straight labels on connections


## Example: two track

```
node a start 0.0
node b outleftsw 250.0
node c inrightsw 500.0
node d end 750.0

edge a.out b.trunk
edge b.left c.right
edge b.right c.left
edge c.trunk d.in
```

## Example: balloon loop

```
node a start 0.0
node b outleftsw 250.0
node c vertical 500.0

edge a.out b.trunk
edge b.left c.top
edge b.right c.bottom
```
