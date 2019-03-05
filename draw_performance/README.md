# Schematic drawing performance test

## Programs:

1. Linear programming

> trainspotting/levelbasedsatplus/linprog file.in

2. Direct grid SAT (gridvis)

Has older format. Use rolling to output it from dgraph format?

> trainspotting/gridvis/gridvis file.in

3. Levels sat

> trainspotting/vis-rs/...

4. Grid SAT

> trainspotting/rail-layout-gridbased/FrontImport file.in hwb




## Settings:
1. optimization criteria ordering
2. time until first solution?

## Models:
1. eidsvoll
2. asker? 
3. arna
4. weert
5. regular axb (artificial scaling)



## Conversion pipelien

1. railML (except weert)
2. convert to dgraph
3. convert to .in
4. convert to old format (direct SAT / gridvis)

5. run programs -> tikz


