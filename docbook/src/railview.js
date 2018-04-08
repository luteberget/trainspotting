function last(a) { return a[a.length -1]; }

var div = d3.select("div#railview");
var gridsvg = div.append("svg").attr("id","gridview")
    .attr("preserveAspectRatio", "xMinYMin meet")
    .attr("viewBox", "0 0 960 300");

var grid = gridsvg.append("g");


gridsvg.call(d3.zoom().on("zoom", function () {
             grid.attr("transform", d3.event.transform)
                  })) ;

var gridmargin = {right: 50, left: 50, top: 50, bottom: 50};
var gridwidth = 960;
var gridheight = 300;

//grid.attr("width",gridwidth);
//grid.attr("height",gridheight);

var gridx = d3.scaleLinear()
    .rangeRound([gridmargin.left, gridwidth-gridmargin.right]).clamp(true);

var gridy = d3.scaleLinear()
    .rangeRound([gridheight-gridmargin.bottom, gridmargin.top]);


var lines = [];
var points = [];
for (var edge in edges) {
    var node_names = edge.split("-");
    var node1_pt      = edges[edge]["lines"][0][0];
    var node1_tangent = edges[edge]["lines"][0];
    var node2_pt      = last(edges[edge]["lines"])[1];
    var node2_tangent = reverse_line(last(edges[edge]["lines"]));
    data.infrastructure.nodes[node_names[0]].pt = node1_pt;
    data.infrastructure.nodes[node_names[0]].tangent = normalize_vector(line_to_vector(node1_tangent));
    data.infrastructure.nodes[node_names[1]].pt = node2_pt;
    data.infrastructure.nodes[node_names[1]].tangent = normalize_vector(line_to_vector(node2_tangent));
    for (var l in edges[edge]["lines"]) {
        lines.push(edges[edge]["lines"][l]);
    }
}

for (var node in data.infrastructure.nodes) {
    var n = data.infrastructure.nodes;
    if (n[node].pt == null) {
        n[node].pt = n[n[node].other_node].pt;
        n[node].tangent = reverse_vector(n[n[node].other_node].tangent);
    }
}

for (var l in lines) {
    points.push(lines[l][0]);
    points.push(lines[l][1]);
}

gridx.domain(d3.extent(points, function(e) { return e[0]; }))
gridy.domain(d3.extent(points.concat([[0,5]]), function(e) { return -e[1]; }))

grid.selectAll("line.schematicline").data(lines).enter().append("line")
  .attr("class", "schematicline")
  .attr("x1", function(d) { return gridx(d[0][0]); })
  .attr("x2", function(d) { return gridx(d[1][0]); })
  .attr("y1", function(d) { return gridy(-d[0][1]); })
  .attr("y2", function(d) { return gridy(-d[1][1]); });

  grid.selectAll("circle.node").data(points).enter().append("circle")
  .attr("class","node")
  .attr("r", 3)
  .attr("cx", function(d) { return gridx(d[0]); })
  .attr("cy", function(d) { return gridy(-d[1]); });

var traingroup = grid.append("g").attr("id","traingroup");
var signalgroup = grid.append("g").attr("id","signalgroup");

var timelinesvg = div.append("svg").attr("id","timeline")
    .attr("preserveAspectRatio", "xMinYMin meet")
    .attr("viewBox", "0 0 960 500");

var timeline = timelinesvg.append("g");
timelinesvg.call(d3.zoom().on("zoom", function () {
             timeline.attr("transform", d3.event.transform)
                  })) ;

var margin = {right: 50, left: 50, top: 50, bottom: 50};
var width = 960; //+timeline.attr("width"),
var height = 500; // +timeline.attr("height");

//timeline.attr("width",width);
//timeline.attr("height",height);

var x = d3.scaleLinear()
    .rangeRound([margin.left, width-margin.right]).clamp(true);

var y = d3.scaleLinear()
    .rangeRound([height-margin.bottom, margin.top]);


    let train_snapshots = [];
    for (var k in data.trains) { train_snapshots.push(...(data.trains[k])); }

	x.domain(d3.extent(train_snapshots, function(d) { return d.time }));
	y.domain(d3.extent(train_snapshots, function(d) { return d.x}));


var train = timeline.append("g").attr("class","trains")
  .selectAll("g").data(Object.keys(data.trains).map(function (key) { return data.trains[key]; })).enter()
  .append("g").attr("class","trainline");

  var trainlength = 100.0;

  var trainpaths = train.selectAll("path").data(function(d) {return d.slice(1).map(function(b,i) { return [d[i],b]; }) }).enter();
//  .append("line")
//.attr("x1", function(d) { return x(d[0].time); })
//.attr("x2", function(d) { return x(d[1].time); })
//.attr("y1", function(d) { return y(d[0].x); })
//.attr("y2", function(d) { return y(d[1].x); })
  trainpaths.append("path")
.attr("d", function(d) { return "M" + x(d[0].time) + "," + y(d[0].x) + 
             "L" + x(d[1].time) + "," + y(d[1].x) ;})
.attr("stroke", function(d) { 
        if (d[1].dx < 1e-4) return "gray";
	if (d[1].action == "Accel" ) return "green";
	if (d[1].action == "Coast" ) return "orange";
	if (d[1].action == "Brake" ) return "red";
});

  trainpaths.append("path")
  .attr("class","trainfilled")
.attr("d", function(d) { return "M" + x(d[0].time) + "," + y(d[0].x) + 
             "L" + x(d[1].time) + "," + y(d[1].x) + 
             "L" + x(d[1].time) + "," + y(d[1].x - trainlength) + 
             "L" + x(d[0].time) + "," + y(d[0].x - trainlength) ;})
.attr("stroke", function(d) { 
	if (d[1].action == "Accel" ) return "green";
	if (d[1].action == "Coast" ) return "orange";
	if (d[1].action == "Brake" ) return "red";
})
.style("opacity",0.25);





// function train_t_x(train,dx) {
// 	var trainline = d3.line()
// 	    .x(function(d) { return x(d.time); })
// 	    .y(function(d) { return y(d.x + dx); });
// 
// 
// 	  timeline.append("path")
// 	    .attr("class","trainline")
// 	    .attr("d", trainline(train));
// 
// 
// 	timeline.selectAll(".dot")
// 	 .data(train)
// 	 .enter().append("circle")
// 	 .attr("class","dot")
// 	    .attr("cx", function(d) { return x(d.time) })
// 	    .attr("cy", function(d) { return y(d.x +dx) })
// 	    .attr("r", 2.5);
// 
// }

//train_t_x(data["trains"]["t1"],0.0);
//train_t_x(data["trains"]["t1"],-200.0);

var slider = timelinesvg.append("g")
.attr("class", "slider")
.attr("transform", "translate(" + 0 + "," + margin.top / 2 + ")");

slider.append("line")
    .attr("class", "track")
    .attr("x1", x.range()[0])
    .attr("x2", x.range()[1])
  .select(function() { return this.parentNode.appendChild(this.cloneNode(true)); })
    .attr("class", "track-inset")
  .select(function() { return this.parentNode.appendChild(this.cloneNode(true)); })
    .attr("class", "track-overlay")
    .call(d3.drag()
        .on("start.interrupt", function() { slider.interrupt(); })
        .on("start drag", function() { set_t(x.invert(d3.event.x)); }));

slider.insert("g", ".track-overlay")
    .attr("class", "ticks")
    .attr("transform", "translate(0," + 18 + ")")
  .selectAll("text")
  .data(x.ticks(10))
  .enter().append("text")
    .attr("x", x)
    .attr("text-anchor", "middle")
    .text(function(d) { return d + " s"; });

var handle = slider.insert("circle", ".track-overlay")
    .attr("class", "handle")
    .attr("r", 9)
    .attr("cx", x(0.0));

var t_line = timeline.insert("line")
  .attr("class","tline")
  .attr("x1",x(0.0))
  .attr("x2",x(0.0))
  .attr("y1",y.range()[0])
  .attr("y2",y.range()[1])
;


function set_t(t) {
    console.log("SET T");
    console.log(t);
  handle.attr("cx",x(t));
  t_line.attr("x1",x(t)).attr("x2",x(t));

  var signals = [];
  for (var obj_i in data.infrastructure.objects) {
      var obj = data.infrastructure.objects[obj_i];
      if (obj.type == "signal") {
          var green = false;
          for(var ei in data.infrastructure.events) {
              var ev = data.infrastructure.events[ei];
              if(ev.time > t) break;
              if(ev.event == "signal" && ev.ref == obj_i) {
                  if (ev.value == "green") { green = true; }
                  else { green = false; }
              }
          }
          signals.push({ "name":obj_i, "green": green, 
              "pt": data.infrastructure.nodes[obj.node].pt,
              "tangent": data.infrastructure.nodes[obj.node].tangent
          });
      }
  }

  console.log("SIGNALS");
  console.log(signals);

  var offset = 0.25;
  var s = signalgroup.selectAll("*").data(signals);
  s.exit().remove();
  s.enter().append("circle").merge(s)
      .attr("cx", function(d) { return gridx(d.pt[0] + offset*rotate_right(d.tangent)[0]); })
      .attr("cy", function(d) { return gridy(-(d.pt[1]) + offset*rotate_right(d.tangent)[1]); })
      .attr("r", 5)
      .attr("class", function(d) { if(d.green) { return "greensig"; } else { return "redsig"; }});

  for (var train in data.trains) {
      var d = data.trains[train];
      var consecutive = d.slice(1).map(function(b,i) { return [d[i],b]; });
      var train_intervals = [];
      for (var i in consecutive) {

          if (consecutive[i][0].time <= t && consecutive[i][1].time >= t) {



              var fraction = (t - consecutive[i][0].time)/(consecutive[i][1].time - consecutive[i][0].time);
              console.log("fraction");
              console.log(fraction);

              var joined_edges = [];
              for (var j in consecutive[i][1].edges) { 
                  var e = consecutive[i][1].edges[j];
                  joined_edges.push({n1: e.n1, n2: e.n2, start: e.start, end: e.end});
              }
              for (var j in consecutive[i][0].edges) {
                  var e = consecutive[i][0].edges[j];

                  var found = false;
                  for (var k in joined_edges) {
                      if (joined_edges[k].n1 == e.n1 && joined_edges[k].n2 == e.n2 ) {
                          found = true;
                          joined_edges[k].start = Math.min(joined_edges[k].start, e.start);
                          joined_edges[k].end   = Math.max(joined_edges[k].end, e.end);
                      }
                  }

                  if(!found) {
                      joined_edges.push({n1: e.n1, n2: e.n2, start: e.start, end: e.end});
                  }
              }

              var neg_x = consecutive[i][1].dx * (1-fraction);
              console.log("neg_x");
              console.log(neg_x);

              while (neg_x > 0.0 && joined_edges.length > 0) {
                  if (joined_edges[0].end - joined_edges[0].start > neg_x) {
                      joined_edges[0].end = joined_edges[0].end - neg_x;
                      neg_x = 0.0;
                  } else {
                      neg_x -= joined_edges[0].end - joined_edges[0].start;
                      joined_edges.shift();
                  }
              }

              var after_train = trainlength;
              var after_train_idx = 0;
              while (after_train_idx < joined_edges.length) {
                  if(joined_edges[after_train_idx].end - joined_edges[after_train_idx].start > after_train) {
                      joined_edges[after_train_idx].start = joined_edges[after_train_idx].end - after_train;
                      after_train = 0.0;
                      break;
                  } else {
                      after_train -= joined_edges[after_train_idx].end - joined_edges[after_train_idx].start;
                  }
                  after_train_idx += 1;
              }
              while (joined_edges.length > after_train_idx+1) {
                  joined_edges.pop();
              }

              console.log("joined_edges");
              console.log(joined_edges);

              for (var j in joined_edges) {
                  var edge = joined_edges[j];
                  var edgename = edge.n1 + "-" + edge.n2;
                  var edge_graphic = get_edge(edge.n1,edge.n2);
                  if(edge_graphic == null) continue;
                  var edgelines = edge_graphic.lines;
                  var edgelines_length = 0.0;
                  var start_frac = edge.start / edge_graphic.length;
                  var end_frac   = edge.end   / edge_graphic.length;
                  for(var k in edgelines) {
                      var l = edgelines[k];
                      var l_len = Math.sqrt((l[0][0] - l[1][0])**2 +
                                            (l[0][1] - l[1][1])**2);
                      edgelines_length += l_len;
                  }
                  var line_length = 0.0;
                  for(var k in edgelines) {
                      var l = edgelines[k];
                      var l_len = Math.sqrt((l[0][0] - l[1][0])**2 +
                                            (l[0][1] - l[1][1])**2);
                      let this_frac_start = line_length / edgelines_length;
                      line_length += l_len;
                      let this_frac_end   = line_length / edgelines_length;
                      var x1 = lerp(l[0][0], l[1][0], clamp(0.0, 1.0, 
                                    (start_frac-this_frac_start)/(this_frac_end-this_frac_start)));
                      var y1 = lerp(l[0][1], l[1][1], clamp(0.0, 1.0, 
                                    (start_frac-this_frac_start)/(this_frac_end-this_frac_start)));
                      var x2 = lerp(l[0][0], l[1][0], clamp(0.0, 1.0, 
                                    (end_frac-this_frac_start)/(this_frac_end-this_frac_start)));
                      var y2 = lerp(l[0][1], l[1][1], clamp(0.0, 1.0, 
                                    (end_frac-this_frac_start)/(this_frac_end-this_frac_start)));
                      train_intervals.push([[x1,y1],[x2,y2]]);
                  }
                  //console.log("edgelines_length");
                  //console.log(edgelines_length);
                  //intervals.push(...edgelines);
              }


              break;
          }

      }

      var ls = traingroup.selectAll("line.train.train"+train).data(train_intervals);
      ls.exit().remove();
      ls.enter().append("line").attr("class","train train"+train)
          .merge(ls)
        .attr("x1", function(d) { return gridx(d[0][0]); })
        .attr("x2", function(d) { return gridx(d[1][0]); })
        .attr("y1", function(d) { return gridy(-d[0][1]); })
        .attr("y2", function(d) { return gridy(-d[1][1]); });
  }
}


function get_edge(n1,n2) {
    let edge = edges[n1 + "-" + n2];
    if (edge == null) {
        edge = edges[n2 + "-" + n1];
        if (edge == null) return null;
        
        edge = { "length": edge.length,
                 "lines": edge.lines.map(reverse_line).reverse() };
    }
    return edge;
}

function reverse_line(a) { return [a[1],a[0]]; }

function clamp(a,b,x) {
    if (a > x) { return a; }
    else {
        if (b < x) { return b; }
        else { return x; }
    }
}

function reverse_vector(a) {
    return [-a[0], -a[1]];
}

function normalize_vector(a) {
    var len = Math.sqrt(a[0]*a[0] + a[1]*a[1]);
    if (len > 0) {
     return [a[0]/len, a[1]/len];
    } else {
        return a;
    }
}

function rotate_right(a) {
    return [a[1],-a[0]];
}

function line_to_vector(a) {
    return [a[1][0] - a[0][0], a[1][1] - a[0][1]];
}

function lerp(a,b,x) {
    return a + (b-a)*x;
}

set_t(0.0);
