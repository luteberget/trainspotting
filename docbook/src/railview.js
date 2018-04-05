var grid = d3.select("svg#gridview"),
    gridmargin = {right: 50, left: 50, top: 50, bottom: 50},
    gridwidth = 1000,
    gridheight = 400;

grid.attr("width",gridwidth);
grid.attr("height",gridheight);

var gridx = d3.scaleLinear()
    .rangeRound([gridmargin.left, gridwidth-gridmargin.right]).clamp(true);

var gridy = d3.scaleLinear()
    .rangeRound([gridheight-gridmargin.bottom, gridmargin.top]);

var lines = [];
var points = [];
for (var edge in edges) {
    for (var l in edges[edge]) {
        lines.push(edges[edge][l]);
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

// grid.selectAll("

// d3.select("div#chartId")
// .append("div")
// .classed("svg-container", true) //container class to make it responsive
// .append("svg")
// //responsive SVG needs these 2 attributes and no width and height attr
// .attr("preserveAspectRatio", "xMinYMin meet")
// .attr("viewBox", "0 0 600 400")
// //class to make it responsive
// .classed("svg-content-responsive", true); 

var svg = d3.select("svg#timeline"),
    margin = {right: 50, left: 50, top: 50, bottom: 50},
    width = 1000, //+svg.attr("width"),
    height = 1000; // +svg.attr("height");

svg.attr("width",width);
svg.attr("height",height);

var x = d3.scaleLinear()
    .rangeRound([margin.left, width-margin.right]).clamp(true);

var y = d3.scaleLinear()
    .rangeRound([height-margin.bottom, margin.top]);


	x.domain(d3.extent(data.trains["t1"], function(d) { return d.time }));
	y.domain(d3.extent(data.trains["t1"], function(d) { return d.x}));


var train = svg.append("g").attr("class","trains")
  .selectAll("g").data(Object.keys(data.trains).map(function (key) { return data.trains[key]; })).enter()
  .append("g").attr("class","trainline");

  var trainlength = 200.0;

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
// 	  svg.append("path")
// 	    .attr("class","trainline")
// 	    .attr("d", trainline(train));
// 
// 
// 	svg.selectAll(".dot")
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

var slider = svg.append("g")
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

var t_line = svg.insert("line")
  .attr("class","tline")
  .attr("x1",x(0.0))
  .attr("x2",x(0.0))
  .attr("y1",y.range()[0])
  .attr("y2",y.range()[1])
;

function set_t(t) {
  handle.attr("cx",x(t));
  t_line.attr("x1",x(t)).attr("x2",x(t));

  var intervals = [];
  for (var train in data.trains) {
      var d = data.trains[train];
      var consecutive = d.slice(1).map(function(b,i) { return [d[i],b]; });
      for (var i in consecutive) {

          if (consecutive[i][0].time <= t && consecutive[i][1].time >= t) {

              for (var j in consecutive[i][1].edges) {
                  var edge = consecutive[i][1].edges[j];
                  var edgename = edge.n1 + "-" + edge.n2;
                  var edgelines = edges[edgename];
                  //console.log(edgelines);
                  intervals.push(...edgelines);
              }

              break;
          }

      }
  }
  //console.log("lines ");
  //console.log(intervals);
  var ls = grid.selectAll("line.train").data(intervals);
  ls.exit().remove();
  ls.enter().append("line").attr("class","train")
      .merge(ls)
    .attr("x1", function(d) { return gridx(d[0][0]); })
    .attr("x2", function(d) { return gridx(d[1][0]); })
    .attr("y1", function(d) { return gridy(-d[0][1]); })
    .attr("y2", function(d) { return gridy(-d[1][1]); });

      
}
