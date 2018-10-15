# milelang: structured railway topology description language

Some ideas for syntax for a description language suitable for railway signalling designs. 
It is *structured* in the sense that programming languages' if/then/else and while constructs
are structured: they cover common cases, such as loops and sidings, while still allowing
for named connections (like gotos in programming languages).


```
define block(x) {
  signal dir=up x=x
  signal dir=down x=x
  detector x=x
}

parallell n=2 {
  line d=1200.0 {
    @sym @par signal dir=up x=200.0;
    @sym @par signal dir=up x=800.0;
    loop left 300.0--900.0 swvel=80.0 {
      incoming right x=500.0 from=cin;
    };
    outgoing left x=500.0 to=cin l=50.0 {};
  };
};
```


## Concept: almost-DAG-shaped railway networks

```
line d=1200.0 {
  loop 

  siding left x=500.0 d=500.0 {
    loop left 200.0--300.0 --- {
    }
  }

  @par signal 
}
```

## Single track with 1 passing loop

```
define twotrack(ca,cb,l,dl,side) {
  line l=l {
    loop left x=(l/2-dl/2) l=dl {};
  }
}

insert(twotrack, a,b,1000,500,left);
line l=600.0 b d;
insert(twotrack, d,c,2000,500,right);
```

## Data structure

```rust
struct Property(String, String);
enum Modifier { Par, Sym };
enum Side { Left, Right };
enum Dir { Outgoing, Incoming };
enum Fly { Over, Under };
type Props = Vec<Property>;
type Mods = Vec<Modifier>;
type LineStmts = Vec<Box<LineLevel>>;
type TopStmts = Vec<Box<TopLevel>>;
enum LineEnd { Internal(String), External(String), Stop }

enum LineLevel {
  Object(String, Mods, Props),
  Loop(Side, Props, LineStmts),
  Siding(Side, LineEnd, Props, LineStmts),
  Connection(Dir, Side, Props, LineStmts),
  Cross(Dir,Side,Props,LineStmts),
  Fly(Fly, Side, Props, LineStmts),
}

enum TopLevel {
  // Maybe remove these two and use instead the function abstraction 
  // (arbitrary number of repeats might not be so important)
  //Parallell(TopStmts, Props),
  //Serial(TopStmts, Props),

  Line(LineStmts, LineEnd, LineEnd, Props),
}

enum ToppestLevel {
  Abstract(String,Vec<String>,TopStmts),
  Import(String,String), //  filename, variablename (namespace for further)
  Concrete(TopStmts),
  Module(String, TopStmts),
}
```

## Example: flying double junction

```
define flyingdoublejunction(c1,c2,c3,c4,c5,c6) {
  line l=1000.0 conn[c1] conn[c2] {
    siding right x=500.0 end=c6 {};
    under x=700.0 fly2;
  }
  line l=1000.0 c2 c4 {
    under x=700.0 fly1;
    siding left x=500.0 end=c5{
      over x=200.0 fly1;
      over x=300.0 fly2;
    };
  }
}
```


## Example: importing a railml model as partial model

```
mod arnacad { import("arna.railml") }
line c[arnacad.bergen] l=1000.0{
  siding left x=700.0 l=300.0 {
    siding left x=25.0 l=200.0 {};
  };
  siding right x=650.0 l=350.0 {
    siding right x=25.0 l=300.0 {};
  };
} stop;
```



## Syntax experiments: line start/end connections

```
line c[arnacad.bergen] length=1000.0 {
} stop[];


c[arna.bergen] 
l[500.0] {
  
}
stop;

```

## Syntax experiment: infrastructure, dispatch and capacity specifications in one file


```
line conn--conn2 {
};

vehicle passenger (length=500, accel=0.5);

scenario crossing {
  movement passenger {
    asdf
  }

  timing p1 p2
}

auto-dispatch *;
```


