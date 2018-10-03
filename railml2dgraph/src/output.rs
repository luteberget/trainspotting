use dgraph::*;
use routes::*;
use std;

pub fn repr_partnodeobject(obj: &PartNodeObject) -> String {
    use dgraph::PartNodeObject::*;
    match *obj {
        Signal(ref x) => format!("signal {}", x),
        TVDEnter(ref x) => format!("enter {}", x),
        TVDExit(ref x) => format!("exit {}", x),
        Sight(ref x, l) => format!("sight {} {}",x,l),
    }
}

pub fn repr_partnode(objs: &[PartNodeObject]) -> String {
    if objs.len() > 0 {
        format!("({})",
                objs.iter()
                    .map(|o| repr_partnodeobject(o))
                    .collect::<Vec<_>>()
                    .join(", "))
    } else {
        "".to_string()
    }
}

pub fn print_resources<W: std::io::Write>(buf: &mut W,
                                          _model: &DGraphModel,
                                          route: &Route)
                                          -> std::io::Result<()> {
    writeln!(buf, "  sections [{}]", route.sections.join(", "))?;
    writeln!(buf,
             "  switches [{}]",
             route.switches
                 .iter()
                 .map(|&(ref sw, pos)| format!("{} {}", sw, pos.as_str()))
                 .collect::<Vec<_>>()
                 .join(", "))?;
    writeln!(buf, "  contains []")?;
    Ok(())
}

pub fn print_releases<W: std::io::Write>(buf: &mut W,
                                         _model: &DGraphModel,
                                         route :&Route)
                                        -> std::io::Result<()> {


    // Don't use partial release on exit routes.
    // Just omit printing here, so that consumers will 
    // create default release behavior.
    let exit_route = if let RouteBoundary::ModelBoundary(_) = route.exit { true } else { false };

    if !exit_route {
        for &(ref trigger,length,ref resources) in &route.releases {
            let reslist = resources.clone().join(", ");
            writeln!(buf, "  release {{ length {} trigger {} resources [{}] }}",
                     length, trigger, reslist)?;
        }
    }
    Ok(())
}

pub fn print_routes<W: std::io::Write>(buf: &mut W,
                                       model: &DGraphModel,
                                       routes: &Vec<Route>)
                                       -> std::io::Result<()> {
    let mut i = 1;
    for r in routes {
        use routes::RouteBoundary::*;
        match (&r.entry, &r.exit) {
            (&ModelBoundary(ref b), &Signal(ref s)) => {
                writeln!(buf,
                         "modelentry r{} from {} {{",
                         i,
                         model.nodes[b.node_idx()].get_part(b.node_part()).name)?;
                writeln!(buf, "  exit {}", s)?;
                writeln!(buf, "  length {}", r.length)?;
                print_resources(buf, model, r)?;
                print_releases(buf, model, r)?;
                writeln!(buf, "}}")?;
            }
            (&Signal(ref s), &ModelBoundary(ref b)) => {
                writeln!(buf,
                         "modelexit r{} to {} {{",
                         i,
                         model.nodes[b.node_idx()].get_part(b.node_part()).name)?;
                writeln!(buf, "  entry {}", s)?;
                if let Some(s) = r.sections.get(0) {
                    writeln!(buf, "  entrysection {}", s)?;
                }
                writeln!(buf, "  length {}", r.length + 1000.0)?;
                print_resources(buf, model, r)?;
                print_releases(buf, model, r)?;
                writeln!(buf, "}}")?;
            }
            (&Signal(ref s1), &Signal(ref s2)) => {
                writeln!(buf, "route r{} {{", i)?;
                writeln!(buf, "  entry {}", s1)?;
                writeln!(buf, "  exit {}", s2)?;
                if let Some(s) = r.sections.get(0) {
                    writeln!(buf, "  entrysection {}", s)?;
                }
                writeln!(buf, "  length {}", r.length)?;
                print_resources(buf, model, r)?;
                print_releases(buf, model, r)?;
                writeln!(buf, "}}")?;
            }
            (&ModelBoundary(ref b1), &ModelBoundary(ref b2)) => {
                println!("Warning: boundaries {:?} to {:?} are reachable without passing a \
                          signal.",
                         b1,
                         b2);
            }
        }

        i += 1;
    }
    Ok(())
}


pub fn print_rolling<W: std::io::Write>(buf: &mut W, model: &DGraphModel) -> std::io::Result<()> {
    for n in &model.nodes {
        writeln!(buf,
                 "node {}{}-{}{}",
                 n.a.name,
                 repr_partnode(&n.a.objs),
                 n.b.name,
                 repr_partnode(&n.b.objs))?;
    }
    for e in &model.edges {
        match *e {
            Edge::Linear(a, (b, d)) => {
                writeln!(buf,
                         "linear {}-{} {}",
                         model.nodes[a.node_idx()].get_part(a.node_part()).name,
                         model.nodes[b.node_idx()].get_part(b.node_part()).name,
                         d)?
            }
            Edge::Switch(ref name, side, n1, (n2, d2), (n3, d3)) => {
                writeln!(buf,
                         "switch {} {} {}-({} {}, {} {})",
                         name,
                         match &side { &Some(ref s) => s.as_str(),
                             &None => "unknown" },
                         model.nodes[n1.node_idx()].get_part(n1.node_part()).name,
                         model.nodes[n2.node_idx()].get_part(n2.node_part()).name,
                         d2,
                         model.nodes[n3.node_idx()].get_part(n3.node_part()).name,
                         d3)?
            }
            Edge::Boundary(n) => {
                writeln!(buf,
                         "boundary {}",
                         model.nodes[n.node_idx()].get_part(n.node_part()).name)?
            }

        }
    }

    Ok(())
}


