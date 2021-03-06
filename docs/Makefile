examples = $(addsuffix .md, $(addprefix src/,\
	   $(patsubst %/,%,$(dir $(wildcard examples/*/.)))))

example_history = $(patsubst %.md,%-history.js, $(examples))
example_vis     = $(patsubst %.md,%-vis.js, $(examples))
example_plans   = $(patsubst src/%.md,%/plan, $(examples))
example_timing  = $(patsubst src/%.md,%/time, $(examples))

ROLLING = ../target/debug/rolling
VIS = ../target/debug/vis_rs
RAILPERFCHECK = ../railperfcheck/railperfcheck
RAILML = ../target/debug/railml2dgraph

.PHONY: book
book: $(examples) $(example_history) $(example_vis)
	mdbook build

performance: book
	./performance.sh

src/%.md: src/%-history.js src/%-vis.js %/template %/infrastructure %/routes %/plan
	sed \
	    -e "/RAILWAYINFRASTRUCTURE/ {r $*/infrastructure" -e 'd }' \
	    -e "/RAILWAYROUTES/ {r $*/routes" -e 'd }' \
	    -e "/RAILWAYDISPATCH/ {r $*/plan" -e 'd }' \
	    -e "/RAILWAYUSAGE/ {r $*/usage" -e 'd }' \
	    -e "/RAILWAYRAILMLINFRASTRUCTURE/ {r $*/railmlinfrastructure" -e 'd }' \
	    -e '/RAILWAYVIEW/ { i <div id="railview"></div>'  \
	    -e 'i <script src="d3/d3.js"></script>' \
	    -e 'i <script src="$*-history.js"></script>' \
	    -e 'i <script src="$*-vis.js"></script>' \
	    -e 'i <script src="railview.js"></script>' \
	    -e 'd }' \
	    $*/template > src/$*.md

src/%-history.js: $(ROLLING) %/infrastructure %/routes %/plan
	$(ROLLING) $*/infrastructure $*/routes $*/plan -vv \
	    -J src/$*-history.js -d 1.0

%/graph: $(ROLLING) %/infrastructure
	$(ROLLING) $*/infrastructure $*/routes $*/plan -vv -g $*/graph

src/%-vis.js: $(VIS) %/infrastructure
	$(VIS) -i $*/infrastructure -j src/$*-vis.js -vv

%/plan %/time: $(RAILPERFCHECK) %/infrastructure %/routes %/usage
	{ time $(RAILPERFCHECK) $*/infrastructure $*/routes $*/usage -p $*/plan | tee $*/railperfchecklog > /dev/null ; } 2> $*/time

%/infrastructure %/routes: $(RAILML) %/railmlinfrastructure
	$(RAILML) $*/railmlinfrastructure -o $*/infrastructure -r $*/routes

